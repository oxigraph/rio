//! Implementation of a generaliazed RDF version of the Trig syntax

use crate::error::*;
use crate::shared::*;
use crate::turtle::*;
use crate::utils::*;
use rio_api::iri::Iri;
use rio_api::model::*;
use rio_api::parser::GeneralizedQuadsParser;
use std::collections::HashMap;
use std::io::BufRead;
use std::str;

pub struct GTriGParser<R: BufRead> {
    read: LookAheadLineBasedByteReader<R>,
    base_iri: Option<Iri<String>>,
    namespaces: HashMap<String, String>,
    bnode_id_generator: BlankNodeIdGenerator,
    term_stack: OwnedTermStack,
    graph_stack: OwnedTermStack,
    temp_buf: String,
}

impl<R: BufRead> GTriGParser<R> {
    /// Builds the parser from a `BufRead` implementation and a base IRI for relative IRI resolution.
    ///
    /// The base IRI might be empty to state there is no base IRI.
    pub fn new(reader: R, base_iri: &str) -> Result<Self, TurtleError> {
        let read = LookAheadLineBasedByteReader::new(reader);
        let base_iri = if base_iri.is_empty() {
            None
        } else {
            Some(
                Iri::parse(base_iri.to_owned())
                    .map_err(|e| read.parse_error(TurtleErrorKind::InvalidIri(e)))?,
            )
        };
        Ok(Self {
            read,
            base_iri,
            namespaces: HashMap::default(),
            bnode_id_generator: BlankNodeIdGenerator::default(),
            graph_stack: OwnedTermStack::new(),
            term_stack: OwnedTermStack::new(),
            temp_buf: String::default(),
        })
    }

    fn make_quad<'a>(&'a self) -> GeneralizedQuad<'a> {
        let t = self.term_stack.last_triple();
        let gn = self.graph_stack.last();
        GeneralizedQuad {
            subject: GeneralizedTerm::from(&t[0]),
            predicate: GeneralizedTerm::from(&t[1]),
            object: GeneralizedTerm::from(&t[2]),
            graph_name: gn.map(GeneralizedTerm::from),
        }
    }
}

impl<R: BufRead> GeneralizedQuadsParser for GTriGParser<R> {
    type Error = TurtleError;

    fn parse_step<E: From<TurtleError>>(
        &mut self,
        on_quad: &mut impl FnMut(GeneralizedQuad) -> Result<(), E>,
    ) -> Result<(), E> {
        parse_block_or_directive(self, on_quad)
    }

    fn is_end(&self) -> bool {
        self.read.current() == EOF
    }
}

fn parse_block_or_directive<R: BufRead, E: From<TurtleError>>(
    parser: &mut GTriGParser<R>,
    on_quad: &mut impl FnMut(GeneralizedQuad) -> Result<(), E>,
) -> Result<(), E> {
    // [1g] 	trigDoc 	::= 	(directive | block)*
    // [2g] 	block 	::= 	triplesOrGraph | wrappedGraph | triples2 | "GRAPH" labelOrSubject wrappedGraph
    skip_whitespace(&mut parser.read)?;

    if parser.read.current() == EOF {
        Ok(())
    } else if parser.read.starts_with(b"@prefix") {
        parse_prefix_id(
            &mut parser.read,
            &mut parser.namespaces,
            &parser.base_iri,
            &mut parser.temp_buf,
        )?;
        Ok(())
    } else if parser.read.starts_with(b"@base") {
        parser.base_iri = Some(parse_base(
            &mut parser.read,
            &mut parser.temp_buf,
            &parser.base_iri,
        )?);
        Ok(())
    } else if parser.read.starts_with_ignore_ascii_case(b"BASE") {
        parser.base_iri = Some(parse_sparql_base(
            &mut parser.read,
            &mut parser.temp_buf,
            &parser.base_iri,
        )?);
        Ok(())
    } else if parser.read.starts_with_ignore_ascii_case(b"PREFIX") {
        parse_sparql_prefix(
            &mut parser.read,
            &mut parser.namespaces,
            &parser.base_iri,
            &mut parser.temp_buf,
        )?;
        Ok(())
    } else if parser.read.starts_with_ignore_ascii_case(b"GRAPH") {
        parser.read.consume_many("GRAPH".len())?;
        skip_whitespace(&mut parser.read)?;
        parse_generalized_term(parser, true)?;
        skip_whitespace(&mut parser.read)?;
        parse_generalized_wrapped_graph(parser, on_quad)?;
        parser.graph_stack.pop();
        Ok(())
    } else if parser.read.current() == b'{' {
        parse_generalized_wrapped_graph(parser, on_quad)
    } else if parser.read.current() == b'['
    && !is_followed_by_space_and_closing_bracket(&parser.read)
    || parser.read.current() == b'('
    {
        parse_generalized_triples2(parser, on_quad)
    } else {
        parse_generalized_triples_or_graph(parser, on_quad)
    }
}

fn parse_generalized_wrapped_graph<R: BufRead, E: From<TurtleError>>(
    parser: &mut GTriGParser<R>,
    on_quad: &mut impl FnMut(GeneralizedQuad) -> Result<(), E>,
) -> Result<(), E> {
    // [5g] 	wrappedGraph 	::= 	'{' triplesBlock? '}'
    // [6g] 	triplesBlock 	::= 	triples ('.' triplesBlock?)?
    parser.read.check_is_current(b'{')?;
    parser.read.consume()?;
    skip_whitespace(&mut parser.read)?;

    loop {
        if parser.read.current() == b'}' {
            parser.read.consume()?;
            return Ok(());
        }

        parse_generalized_triples(parser, on_quad)?;
        match parser.read.current() {
            b'.' => {
                parser.read.consume()?;
                skip_whitespace(&mut parser.read)?;
            }
            b'}' => {
                parser.read.consume()?;
                return Ok(());
            }
            _ => parser.read.unexpected_char_error()?,
        }
    }
}

fn parse_generalized_triples<R: BufRead, E: From<TurtleError>>(
    parser: &mut GTriGParser<R>,
    on_quad: &mut impl FnMut(GeneralizedQuad) -> Result<(), E>,
) -> Result<(), E> {
    // [6] 	triples 	::= 	subject predicateObjectList | blankNodePropertyList predicateObjectList?
    match parser.read.current() {
        b'[' if !is_followed_by_space_and_closing_bracket(&parser.read) => {
            parse_generalized_blank_node_property_list(parser, on_quad)?;
            skip_whitespace(&mut parser.read)?;
            if parser.read.current() != b'.' && parser.read.current() != b'}' {
                parse_generalized_predicate_object_list(parser, on_quad)?;
            }
        }
        _ => {
            parse_generalized_node(parser, on_quad)?;
            skip_whitespace(&mut parser.read)?;
            parse_generalized_predicate_object_list(parser, on_quad)?;
        }
    }
    parser.term_stack.pop();
    Ok(())
}

fn parse_generalized_triples2<R: BufRead, E: From<TurtleError>>(
    parser: &mut GTriGParser<R>,
    on_quad: &mut impl FnMut(GeneralizedQuad) -> Result<(), E>,
) -> Result<(), E> {
    // [4g] 	triples2 	::= 	blankNodePropertyList predicateObjectList? '.' | collection predicateObjectList '.'
    match parser.read.current() {
        b'[' if !is_followed_by_space_and_closing_bracket(&parser.read) => {
            parse_generalized_blank_node_property_list(parser, on_quad)?;
            skip_whitespace(&mut parser.read)?;
            if parser.read.current() != b'.' {
                parse_generalized_predicate_object_list(parser, on_quad)?;
            }
        }
        _ => {
            parse_generalized_collection(parser, on_quad)?;
            skip_whitespace(&mut parser.read)?;
            parse_generalized_predicate_object_list(parser, on_quad)?;
        }
    }

    parser.term_stack.pop();

    parser.read.check_is_current(b'.')?;
    parser.read.consume()?;
    Ok(())
}

fn parse_generalized_triples_or_graph<R: BufRead, E: From<TurtleError>>(
    parser: &mut GTriGParser<R>,
    on_quad: &mut impl FnMut(GeneralizedQuad) -> Result<(), E>,
) -> Result<(), E> {
    // [3g] 	triplesOrGraph 	::= 	labelOrSubject (wrappedGraph | predicateObjectList '.')
    parse_generalized_node(parser, on_quad)?;
    skip_whitespace(&mut parser.read)?;

    if parser.read.current() == b'{' {
        parser.graph_stack.steal(&mut parser.term_stack);
        parse_generalized_wrapped_graph(parser, on_quad)?;
        parser.graph_stack.pop();
    } else {
        parse_generalized_predicate_object_list(parser, on_quad)?;
        parser.read.check_is_current(b'.')?;
        parser.read.consume()?;
        parser.term_stack.pop();
    }
    Ok(())
}

fn parse_generalized_blank_node_property_list<R: BufRead, E: From<TurtleError>>(
    parser: &mut GTriGParser<R>,
    on_quad: &mut impl FnMut(GeneralizedQuad) -> Result<(), E>,
) -> Result<(), E> {
    parser.read.check_is_current(b'[')?;
    parser.read.consume()?;
    skip_whitespace(&mut parser.read)?;

    let blank_node = parser.term_stack.push(OwnedTermKind::BlankNode);
    blank_node.value.push_str(parser.bnode_id_generator.generate().as_ref());

    loop {
        parse_generalized_predicate_object_list(parser, on_quad)?;
        skip_whitespace(&mut parser.read)?;

        if parser.read.current() == b']' {
            parser.read.consume()?;
            return Ok(());
        }
    }
}

fn parse_generalized_collection<R: BufRead, E: From<TurtleError>>(
    parser: &mut GTriGParser<R>,
    on_quad: &mut impl FnMut(GeneralizedQuad) -> Result<(), E>,
) -> Result<(), E> {
    // [15] 	collection 	::= 	'(' object* ')'
    parser.read.check_is_current(b'(')?;
    parser.read.consume()?;

    parser.term_stack.push(OwnedTermKind::BlankNode);
    let mut root: Option<BlankNodeId> = None;
    loop {
        skip_whitespace(&mut parser.read)?;

        if parser.read.current() == EOF {
            return Ok(parser.read.unexpected_char_error()?);
        } else if parser.read.current() == b')' {
            parser.read.consume()?;
            match root {
                Some(id) => {
                    parser.term_stack.push(OwnedTermKind::StaticIri(RDF_REST));
                    parser.term_stack.push(OwnedTermKind::StaticIri(RDF_NIL));
                    on_quad(parser.make_quad())?;
                    parser.term_stack.pop();
                    parser.term_stack.pop();
                    assert_eq!(parser.term_stack.last().unwrap().kind, OwnedTermKind::BlankNode);
                    let buffer = &mut parser.term_stack.last_mut().value;
                    buffer.clear();
                    buffer.push_str(id.as_ref());
                }
                None => {
                    parser.term_stack.pop();
                    parser.term_stack.push(OwnedTermKind::StaticIri(RDF_NIL));
                }
            }
            return Ok(());
        } else {
            let new = parser.bnode_id_generator.generate();
            if root == None {
                root = Some(new);
            } else {
                parser.term_stack.push(OwnedTermKind::StaticIri(RDF_REST));
                let blank_node = parser.term_stack.push(OwnedTermKind::BlankNode);
                blank_node.value.push_str(new.as_ref());
                on_quad(parser.make_quad())?;
                parser.term_stack.pop();
                parser.term_stack.pop();
                }
            assert_eq!(parser.term_stack.last().unwrap().kind, OwnedTermKind::BlankNode);
            let buffer = &mut parser.term_stack.last_mut().value;
            buffer.clear();
            buffer.push_str(new.as_ref());
            parser.term_stack.push(OwnedTermKind::StaticIri(RDF_FIRST));
            parse_generalized_node(parser, on_quad)?;
            on_quad(parser.make_quad())?;
            parser.term_stack.pop();
            parser.term_stack.pop();
        }
    }
}

fn parse_generalized_predicate_object_list<R: BufRead, E: From<TurtleError>>(
    parser: &mut GTriGParser<R>,
    on_quad: &mut impl FnMut(GeneralizedQuad) -> Result<(), E>,
) -> Result<(), E> {
    // [7] 	predicateObjectList 	::= 	verb objectList (';' (verb objectList)?)*
    loop {
        parse_generalized_verb(parser, on_quad)?;
        skip_whitespace(&mut parser.read)?;

        parse_generalized_object_list(parser, on_quad)?;
        skip_whitespace(&mut parser.read)?;

        parser.term_stack.pop();

        while parser.read.current() == b';' {
            parser.read.consume()?;
            skip_whitespace(&mut parser.read)?;
        }
        match parser.read.current() {
            b'.' | b']' | b'}' | EOF => return Ok(()),
            _ => (), //continue
        }
    }
}

fn parse_generalized_verb<R: BufRead, E: From<TurtleError>>(
    parser: &mut GTriGParser<R>,
    on_quad: &mut impl FnMut(GeneralizedQuad) -> Result<(), E>,
) -> Result<(), E> {
    // [9] 	verb 	::= 	predicate | 'a'
    if parser.read.current() == b'a' {
        match parser.read.next() {
            // We check that it is not a prefixed URI
            Some(c)
                if is_possible_pn_chars_ascii(c) || c == b'.' || c == b':' || c > MAX_ASCII => (),
            _ => {
                parser.term_stack.push(OwnedTermKind::StaticIri(RDF_TYPE));
                parser.read.consume()?;
                return Ok(());
            }
        }
    }
    parse_generalized_node(parser, on_quad)
}

fn parse_generalized_object_list<R: BufRead, E: From<TurtleError>>(
    parser: &mut GTriGParser<R>,
    on_quad: &mut impl FnMut(GeneralizedQuad) -> Result<(), E>,
) -> Result<(), E> {
    // [8] 	objectList 	::= 	object (',' object)*
    loop {
        parse_generalized_node(parser, on_quad)?;
        on_quad(parser.make_quad())?;
        parser.term_stack.pop();

        skip_whitespace(&mut parser.read)?;
        if parser.read.current() != b',' {
            return Ok(());
        }
        parser.read.consume()?;
        skip_whitespace(&mut parser.read)?;
    }
}

fn parse_generalized_node<R: BufRead, E: From<TurtleError>>(
    parser: &mut GTriGParser<R>,
    on_quad: &mut impl FnMut(GeneralizedQuad) -> Result<(), E>,
) -> Result<(), E> {
    //[10] 	subject 	::= 	iri | BlankNode | collection
    match parser.read.current() {
        b'_' | b'[' if is_followed_by_space_and_closing_bracket(&parser.read) => {
            let blank_node = parser.term_stack.push(OwnedTermKind::BlankNode);
            parse_blank_node(
                &mut parser.read,
                &mut blank_node.value,
                &mut parser.bnode_id_generator,
            )?;
            Ok(())
        }
        b'[' => parse_generalized_blank_node_property_list(parser, on_quad),
        b'(' => parse_generalized_collection(parser, on_quad),
        _ =>  {
            parse_generalized_term(parser, false)?;
            Ok(())
        }
    }
}

fn parse_generalized_term<R: BufRead>(
    parser: &mut GTriGParser<R>,
    graph_name: bool,
) -> Result<(), TurtleError> {
    let stack = if graph_name { &mut parser.graph_stack } else { &mut parser.term_stack };
    match parser.read.current() {
        EOF => parser.read.unexpected_char_error()?,
        b'<' => {
            let named_node = stack.push(OwnedTermKind::NamedNode);
            parse_iri(
                &mut parser.read,
                &mut named_node.value,
                &mut parser.temp_buf,
                &parser.base_iri,
                &parser.namespaces,
            )
        }
        b'_' | b'[' => {
            let blank_node = stack.push(OwnedTermKind::BlankNode);
            parse_blank_node(
                &mut parser.read,
                &mut blank_node.value,
                &mut parser.bnode_id_generator,
            )
        }
        b'"' | b'\'' | b'+' | b'-' | b'.' | b'0'..=b'9' => {
            let literal = stack.push(OwnedTermKind::LiteralSimple);
            let term_type = parse_literal(
                &mut parser.read,
                &mut literal.value,
                &mut literal.extra,
                &mut parser.temp_buf,
                &parser.base_iri,
                &parser.namespaces,
            )?;
            literal.kind = term_type.into();
            Ok(())
        }
        b'?' | b'$' => {
            parser.read.consume()?;
            let variable = stack.push(OwnedTermKind::Variable);
            parse_variable_name(
                &mut parser.read,
                &mut variable.value,
            )
        }
        _ => {
            if parser.read.starts_with(b"true") || parser.read.starts_with(b"false") {
                let literal = stack.push(OwnedTermKind::LiteralDatatype);
                parse_literal(
                    &mut parser.read,
                    &mut literal.value,
                    &mut literal.extra,
                    &mut parser.temp_buf,
                    &parser.base_iri,
                    &parser.namespaces,
                ).map(|_| ())
            } else {
                let named_node = stack.push(OwnedTermKind::NamedNode);
                parse_iri(
                    &mut parser.read,
                    &mut named_node.value,
                    &mut parser.temp_buf,
                    &parser.base_iri,
                    &parser.namespaces,
                )
            }
        }
    }
}

pub(crate) fn parse_variable_name<'a>(
    read: &mut impl LookAheadByteRead,
    buffer: &'a mut String,
) -> Result<(), TurtleError> {
    let c = read.current();
    if c <= MAX_ASCII && (is_possible_pn_chars_u_ascii(c) || b'0' <= c && c <= b'9') {
        buffer.push(char::from(c))
    } else {
        let c = read_utf8_char(read)?;
        if is_possible_pn_chars_u_unicode(c) {
            buffer.push(c);
        } else {
            read.unexpected_char_error()?
        }
    }

    loop {
        read.consume()?;
        let c = read.current();
        if c <= MAX_ASCII && (is_possible_pn_chars_u_ascii(c) || b'0' <= c && c <= b'9' || c == 0xb7) {
            buffer.push(char::from(c))
        } else {
            let c = read_utf8_char(read)?;
            if is_possible_pn_chars_u_unicode(c) {
                buffer.push(c);
            } else {
                return Ok(());
            }
        }
    }
}



//

struct OwnedTermStack {
    inner: Vec<OwnedTerm>,
    len: usize,
}

impl OwnedTermStack {
    fn new() -> OwnedTermStack {
        OwnedTermStack {
            inner: Vec::with_capacity(3),
            len: 0,
        }
    }

    fn push(&mut self, kind: OwnedTermKind) -> &mut OwnedTerm {
        self.len += 1;
        if self.len > self.inner.len() {
            self.inner.push(OwnedTerm {
                kind,
                value: String::default(),
                extra: String::default(),
            })
        } else {
            self.inner[self.len - 1].kind = kind;
        }
        &mut self.inner[self.len - 1]
    }

    fn pop(&mut self) {
        assert!(self.len > 0);
        let top_term = &mut self.inner[self.len - 1];
        top_term.value.clear();
        top_term.extra.clear();
        self.len -= 1;
    }

    /// Steal the head of another stack
    fn steal(&mut self, other: &mut OwnedTermStack) {
        assert!(other.len > 0);
        let other_top = &other.inner[other.len - 1];
        let self_top = self.push(other_top.kind);
        self_top.value.push_str(&other_top.value);
        self_top.extra.push_str(&other_top.extra);
        other.pop();
    }

    fn last(&self) -> Option<&OwnedTerm> {
        match self.len {
            0 => None,
            _ => Some(&self.inner[self.len - 1]),
        }
    }

    fn last_mut(&mut self) -> &mut OwnedTerm {
        assert!(self.len > 0);
        &mut self.inner[self.len - 1]
    }

    fn last_triple(&self) -> &[OwnedTerm] {
        assert!(self.len >= 3);
        &self.inner[self.len - 3..]
    }
}

#[derive(Debug, PartialEq)]
struct OwnedTerm {
    kind: OwnedTermKind,
    value: String,
    extra: String,
}

#[derive(Clone, Copy, Debug, PartialEq)]
enum OwnedTermKind {
    NamedNode,
    StaticIri(&'static str),
    BlankNode,
    LiteralSimple,
    LiteralLanguage,
    LiteralDatatype,
    Variable,
}

impl<'a> From<&'a OwnedTerm> for GeneralizedTerm<'a> {
    fn from(other: &'a OwnedTerm) -> GeneralizedTerm<'a> {
        match other.kind {
            OwnedTermKind::NamedNode => GeneralizedTerm::NamedNode(NamedNode{ iri: &other.value }),
            OwnedTermKind::StaticIri(val) => GeneralizedTerm::NamedNode(NamedNode{ iri: val }),
            OwnedTermKind::BlankNode => GeneralizedTerm::BlankNode(BlankNode{ id: &other.value }),
            OwnedTermKind::LiteralSimple => GeneralizedTerm::Literal(Literal::Simple { value: &other.value }),
            OwnedTermKind::LiteralLanguage => GeneralizedTerm::Literal(Literal::LanguageTaggedString{ value: &other.value, language: &other.extra }),
            OwnedTermKind::LiteralDatatype => GeneralizedTerm::Literal(Literal::Typed{ value: &other.value, datatype: NamedNode{ iri: &other.extra } }),
            OwnedTermKind::Variable => GeneralizedTerm::Variable(Variable{ name: &other.value }),
        }
    }
}

impl From<TermType> for OwnedTermKind {
    fn from(other: TermType) -> OwnedTermKind {
        match other {
            TermType::NamedNode => OwnedTermKind::NamedNode,
            TermType::BlankNode => OwnedTermKind::BlankNode,
            TermType::SimpleLiteral => OwnedTermKind::LiteralSimple,
            TermType::LanguageTaggedString => OwnedTermKind::LiteralLanguage,
            TermType::TypedLiteral => OwnedTermKind::LiteralDatatype,
        }
    }
}

//

#[cfg(test)]
mod test {
    use super::*;
    use std::io::Cursor;

    #[test]
    fn all_variables() -> Result<(), TurtleError> {
        let gtrig = r#"
          ?s1 ?p1 ?o1.
          { ?s2 ?p2 ?o2 }
          ?g3 { ?s3 ?p3 ?o3 }
          GRAPH ?g4 { ?s4 ?p4 ?o4 }
        "#;

        let expected = vec![
            (v("s1"), v("p1"), v("o1"), None),
            (v("s2"), v("p2"), v("o2"), None),
            (v("s3"), v("p3"), v("o3"), Some(v("g3"))),
            (v("s4"), v("p4"), v("o4"), Some(v("g4"))),
        ];

        let mut got: Vec<(OwnedTerm, OwnedTerm, OwnedTerm, Option<OwnedTerm>)> = Vec::with_capacity(expected.len());

        GTriGParser::new(Cursor::new(gtrig), "http://example.org/base/")?
            .parse_all(&mut |quad| {
                got.push((
                    quad.subject.into(),
                    quad.predicate.into(),
                    quad.object.into(),
                    quad.graph_name.map(OwnedTerm::from),
                ));
                Ok(()) as Result<(), TurtleError>
            })?;

        assert_eq!(expected, got);
        Ok(())
    }

    #[test]
    fn all_literals() -> Result<(), TurtleError> {
        let gtrig = r#"
          "s1" "p1" "o1".
          { "s2" "p2" "o2" }
          "g3" { "s3" "p3" "o3" }
          GRAPH "g4" { "s4" "p4" "o4" }
        "#;

        let expected = vec![
            (l("s1"), l("p1"), l("o1"), None),
            (l("s2"), l("p2"), l("o2"), None),
            (l("s3"), l("p3"), l("o3"), Some(l("g3"))),
            (l("s4"), l("p4"), l("o4"), Some(l("g4"))),
        ];

        let mut got: Vec<(OwnedTerm, OwnedTerm, OwnedTerm, Option<OwnedTerm>)> = Vec::with_capacity(expected.len());

        GTriGParser::new(Cursor::new(gtrig), "http://example.org/base/")?
            .parse_all(&mut |quad| {
                got.push((
                    quad.subject.into(),
                    quad.predicate.into(),
                    quad.object.into(),
                    quad.graph_name.map(OwnedTerm::from),
                ));
                Ok(()) as Result<(), TurtleError>
            })?;

        assert_eq!(expected, got);
        Ok(())
    }

    #[test]
    fn composite_predicate() -> Result<(), TurtleError> {
        let gtrig = r#"
          ?s [ ?p ?o1 ] ?o2 .
        "#;

        let mut got: Vec<(OwnedTerm, OwnedTerm, OwnedTerm, Option<OwnedTerm>)> = Vec::with_capacity(2);

        GTriGParser::new(Cursor::new(gtrig), "http://example.org/base/")?
            .parse_all(&mut |quad| {
                got.push((
                    quad.subject.into(),
                    quad.predicate.into(),
                    quad.object.into(),
                    quad.graph_name.map(OwnedTerm::from),
                ));
                Ok(()) as Result<(), TurtleError>
            })?;

        assert_eq!(v("p"), got[0].1);
        assert_eq!(v("o1"), got[0].2);
        assert_eq!(v("s"), got[1].0);
        assert_eq!(v("o2"), got[1].2);
        assert_eq!(got[0].0, got[1].1);
        Ok(())
    }

    fn v<'a>(value: &'a str) -> OwnedTerm {
        OwnedTerm {
            kind: OwnedTermKind::Variable,
            value: value.to_string(),
            extra: String::new(),
        }
    }

    fn l<'a>(value: &'a str) -> OwnedTerm {
        OwnedTerm {
            kind: OwnedTermKind::LiteralSimple,
            value: value.to_string(),
            extra: String::new(),
        }
    }

    impl<'a> From<GeneralizedTerm<'a>> for OwnedTerm {
        fn from(other: GeneralizedTerm<'a>) -> OwnedTerm {
            match other {
                GeneralizedTerm::NamedNode(n) => OwnedTerm { kind: OwnedTermKind::NamedNode, value: n.iri.to_string(), extra: String::new() },
                GeneralizedTerm::BlankNode(n) => OwnedTerm { kind: OwnedTermKind::BlankNode, value: n.id.to_string(), extra: String::new() },
                GeneralizedTerm::Literal(Literal::Simple {value}) => OwnedTerm { kind: OwnedTermKind::LiteralSimple, value: value.to_string(), extra: String::new() },
                GeneralizedTerm::Literal(Literal::LanguageTaggedString {value, language}) => OwnedTerm { kind: OwnedTermKind::LiteralLanguage, value: value.to_string(), extra: language.to_string() },
                GeneralizedTerm::Literal(Literal::Typed {value, datatype}) => OwnedTerm { kind: OwnedTermKind::LiteralDatatype, value: value.to_string(), extra: datatype.to_string() },
                GeneralizedTerm::Variable(n) => OwnedTerm { kind: OwnedTermKind::Variable, value: n.name.to_string(), extra: String::new() },
            }
        }
    }
}
