//! Implementation of Turtle and Trig RDF syntax

use crate::error::*;
use crate::iri::IriParser;
use crate::shared::*;
use crate::utils::*;
use rio_api::model::*;
use rio_api::parser::{QuadParser, TripleParser};
use std::collections::HashMap;
use std::io::BufRead;
use std::str;

/// A [Turtle](https://www.w3.org/TR/turtle/) streaming parser.
///
/// It implements the `TripleParser` trait.
///
///
/// Count the number of of people using the `TripleParser` API:
/// ```
/// use rio_turtle::{TurtleParser, TurtleError};
/// use rio_api::parser::TripleParser;
/// use rio_api::model::NamedNode;
///
/// let file = b"@prefix schema: <http://schema.org/> .
/// <http://example.com/foo> a schema:Person ;
///     schema:name  \"Foo\" .
/// <http://example.com/bar> a schema:Person ;
///     schema:name  \"Bar\" .";
///
/// let rdf_type = NamedNode { iri: "http://www.w3.org/1999/02/22-rdf-syntax-ns#type" };
/// let schema_person = NamedNode { iri: "http://schema.org/Person" };
/// let mut count = 0;
/// TurtleParser::new(file.as_ref(), "").unwrap().parse_all(&mut |t| {
///     if t.predicate == rdf_type && t.object == schema_person.into() {
///         count += 1;
///     }
///     Ok(()) as Result<(), TurtleError>
/// }).unwrap();
/// assert_eq!(2, count)
/// ```
pub struct TurtleParser<R: BufRead> {
    read: LookAheadLineBasedByteReader<R>,
    iri_parser: IriParser,
    namespaces: HashMap<String, String>,
    bnode_id_generator: BlankNodeIdGenerator,
    subject_buf_stack: StringBufferStack,
    subject_type_stack: Vec<NamedOrBlankNodeType>,
    predicate_buf_stack: StringBufferStack,
    object_annotation_buf: String, // datatype or language tag
    temp_buf: String,
}

impl<R: BufRead> TurtleParser<R> {
    /// Builds the parser from a `BufRead` implementation and a base IRI for relative IRI resolution.
    ///
    /// The base IRI might be empty to state there is no base IRI.
    pub fn new(reader: R, base_iri: &str) -> Result<Self, TurtleError> {
        let read = LookAheadLineBasedByteReader::new(reader)?;
        let iri_parser = IriParser::new(base_iri.as_bytes())
            .map_err(|_| read.parse_error(TurtleErrorKind::InvalidBaseIRI))?;
        Ok(Self {
            read,
            iri_parser,
            namespaces: HashMap::default(),
            bnode_id_generator: BlankNodeIdGenerator::default(),
            subject_buf_stack: StringBufferStack::default(),
            subject_type_stack: Vec::default(),
            predicate_buf_stack: StringBufferStack::default(),
            object_annotation_buf: String::default(),
            temp_buf: String::default(),
        })
    }
}

impl<R: BufRead> TripleParser for TurtleParser<R> {
    type Error = TurtleError;

    fn parse_step<E: From<TurtleError>>(
        &mut self,
        on_triple: &mut impl FnMut(Triple) -> Result<(), E>,
    ) -> Result<(), E> {
        parse_statement(self, on_triple)
    }

    fn is_end(&self) -> bool {
        self.read.current() == EOF
    }
}

/// A [TriG](https://www.w3.org/TR/trig/) streaming parser.
///
/// It implements the `QuadParser` trait.
///
///
/// Count the number of of people using the `QuadParser` API:
/// ```
/// use rio_turtle::{TriGParser, TurtleError};
/// use rio_api::parser::QuadParser;
/// use rio_api::model::NamedNode;
///
/// let file = b"@prefix schema: <http://schema.org/> .
/// <http://example/> {
///     <http://example.com/foo> a schema:Person ;
///         schema:name  \"Foo\" .
///     <http://example.com/bar> a schema:Person ;
///         schema:name  \"Bar\" .
/// }";
///
/// let rdf_type = NamedNode { iri: "http://www.w3.org/1999/02/22-rdf-syntax-ns#type" };
/// let schema_person = NamedNode { iri: "http://schema.org/Person" };
/// let mut count = 0;
/// TriGParser::new(file.as_ref(), "").unwrap().parse_all(&mut |t| {
///     if t.predicate == rdf_type && t.object == schema_person.into() {
///         count += 1;
///     }
///     Ok(()) as Result<(), TurtleError>
/// }).unwrap();
/// assert_eq!(2, count)
/// ```
pub struct TriGParser<R: BufRead> {
    inner: TurtleParser<R>,
    graph_name_buf: String,
}

impl<R: BufRead> TriGParser<R> {
    /// Builds the parser from a `BufRead` implementation and a base IRI for relative IRI resolution.
    ///
    /// The base IRI might be empty to state there is no base URL.
    pub fn new(reader: R, base_iri: &str) -> Result<Self, TurtleError> {
        Ok(Self {
            inner: TurtleParser::new(reader, base_iri)?,
            graph_name_buf: String::default(),
        })
    }
}

impl<R: BufRead> QuadParser for TriGParser<R> {
    type Error = TurtleError;

    fn parse_step<E: From<TurtleError>>(
        &mut self,
        on_quad: &mut impl FnMut(Quad) -> Result<(), E>,
    ) -> Result<(), E> {
        parse_block_or_directive(self, on_quad)
    }

    fn is_end(&self) -> bool {
        self.inner.read.current() == EOF
    }
}

const RDF_TYPE: &str = "http://www.w3.org/1999/02/22-rdf-syntax-ns#type";
const RDF_NIL: &str = "http://www.w3.org/1999/02/22-rdf-syntax-ns#nil";
const RDF_FIRST: &str = "http://www.w3.org/1999/02/22-rdf-syntax-ns#first";
const RDF_REST: &str = "http://www.w3.org/1999/02/22-rdf-syntax-ns#rest";
const XSD_BOOLEAN: &str = "http://www.w3.org/2001/XMLSchema#boolean";
const XSD_DECIMAL: &str = "http://www.w3.org/2001/XMLSchema#decimal";
const XSD_DOUBLE: &str = "http://www.w3.org/2001/XMLSchema#double";
const XSD_INTEGER: &str = "http://www.w3.org/2001/XMLSchema#integer";

fn parse_statement<R: BufRead, E: From<TurtleError>>(
    parser: &mut TurtleParser<R>,
    on_triple: &mut impl FnMut(Triple) -> Result<(), E>,
) -> Result<(), E> {
    skip_whitespace(&mut parser.read)?;

    if parser.read.current() == EOF {
        Ok(())
    } else if parser.read.starts_with(b"@prefix") {
        parse_prefix_id(
            &mut parser.read,
            &mut parser.namespaces,
            &parser.iri_parser,
            &mut parser.temp_buf,
        )
        .map_err(E::from)
    } else if parser.read.starts_with(b"@base") {
        parse_base(
            &mut parser.read,
            &mut parser.temp_buf,
            &mut parser.object_annotation_buf,
            &mut parser.iri_parser,
        )
        .map_err(E::from)
    } else if parser.read.starts_with_ignore_ascii_case(b"BASE") {
        parse_sparql_base(
            &mut parser.read,
            &mut parser.temp_buf,
            &mut parser.object_annotation_buf,
            &mut parser.iri_parser,
        )
        .map_err(E::from)
    } else if parser.read.starts_with_ignore_ascii_case(b"PREFIX") {
        parse_sparql_prefix(
            &mut parser.read,
            &mut parser.namespaces,
            &parser.iri_parser,
            &mut parser.temp_buf,
        )
        .map_err(E::from)
    } else {
        parse_triples(parser, on_triple)?;

        parser.read.check_is_current(b'.')?;
        parser.read.consume()?;
        Ok(())
    }
}

fn parse_block_or_directive<R: BufRead, E: From<TurtleError>>(
    parser: &mut TriGParser<R>,
    on_quad: &mut impl FnMut(Quad) -> Result<(), E>,
) -> Result<(), E> {
    // [1g] 	trigDoc 	::= 	(directive | block)*
    // [2g] 	block 	::= 	triplesOrGraph | wrappedGraph | triples2 | "GRAPH" labelOrSubject wrappedGraph
    skip_whitespace(&mut parser.inner.read)?;

    if parser.inner.read.current() == EOF {
        Ok(())
    } else if parser.inner.read.starts_with(b"@prefix") {
        parse_prefix_id(
            &mut parser.inner.read,
            &mut parser.inner.namespaces,
            &parser.inner.iri_parser,
            &mut parser.inner.temp_buf,
        )?;
        Ok(())
    } else if parser.inner.read.starts_with(b"@base") {
        parse_base(
            &mut parser.inner.read,
            &mut parser.inner.temp_buf,
            &mut parser.inner.object_annotation_buf,
            &mut parser.inner.iri_parser,
        )?;
        Ok(())
    } else if parser.inner.read.starts_with_ignore_ascii_case(b"BASE") {
        parse_sparql_base(
            &mut parser.inner.read,
            &mut parser.inner.temp_buf,
            &mut parser.inner.object_annotation_buf,
            &mut parser.inner.iri_parser,
        )?;
        Ok(())
    } else if parser.inner.read.starts_with_ignore_ascii_case(b"PREFIX") {
        parse_sparql_prefix(
            &mut parser.inner.read,
            &mut parser.inner.namespaces,
            &parser.inner.iri_parser,
            &mut parser.inner.temp_buf,
        )?;
        Ok(())
    } else if parser.inner.read.starts_with_ignore_ascii_case(b"GRAPH") {
        parser.inner.read.consume_many("GRAPH".len())?;
        skip_whitespace(&mut parser.inner.read)?;

        let graph_name = parse_label_or_subject(
            &mut parser.inner.read,
            &mut parser.graph_name_buf,
            &mut parser.inner.temp_buf,
            &parser.inner.iri_parser,
            &parser.inner.namespaces,
            &mut parser.inner.bnode_id_generator,
        )?
        .with_value(&parser.graph_name_buf);
        skip_whitespace(&mut parser.inner.read)?;

        parse_wrapped_graph(
            &mut parser.inner,
            &mut on_triple_in_graph(on_quad, Some(graph_name)),
        )?;
        parser.graph_name_buf.clear();
        Ok(())
    } else if parser.inner.read.current() == b'{' {
        parse_wrapped_graph(&mut parser.inner, &mut on_triple_in_graph(on_quad, None))
    } else if parser.inner.read.current() == b'['
        && !is_followed_by_space_and_closing_bracket(&parser.inner.read)
        || parser.inner.read.current() == b'('
    {
        parse_triples2(&mut parser.inner, &mut on_triple_in_graph(on_quad, None))
    } else {
        parse_triples_or_graph(parser, on_quad)
    }
}

fn parse_triples_or_graph<R: BufRead, E: From<TurtleError>>(
    parser: &mut TriGParser<R>,
    on_quad: &mut impl FnMut(Quad) -> Result<(), E>,
) -> Result<(), E> {
    // [3g] 	triplesOrGraph 	::= 	labelOrSubject (wrappedGraph | predicateObjectList '.')
    let front_type = parse_label_or_subject(
        &mut parser.inner.read,
        &mut parser.graph_name_buf,
        &mut parser.inner.temp_buf,
        &parser.inner.iri_parser,
        &parser.inner.namespaces,
        &mut parser.inner.bnode_id_generator,
    )?;
    skip_whitespace(&mut parser.inner.read)?;

    if parser.inner.read.current() == b'{' {
        let graph_name = front_type.with_value(&parser.graph_name_buf);
        parse_wrapped_graph(
            &mut parser.inner,
            &mut on_triple_in_graph(on_quad, Some(graph_name)),
        )?;
        parser.graph_name_buf.clear();
    } else {
        parser
            .inner
            .subject_buf_stack
            .push()
            .push_str(&parser.graph_name_buf);
        parser.graph_name_buf.clear();
        parser.inner.subject_type_stack.push(front_type);
        parse_predicate_object_list(&mut parser.inner, &mut on_triple_in_graph(on_quad, None))?;
        parser.inner.subject_type_stack.pop();
        parser.inner.subject_buf_stack.pop();

        parser.inner.read.check_is_current(b'.')?;
        parser.inner.read.consume()?;
    }
    Ok(())
}

fn parse_triples2<R: BufRead, E: From<TurtleError>>(
    parser: &mut TurtleParser<R>,
    on_triple: &mut impl FnMut(Triple) -> Result<(), E>,
) -> Result<(), E> {
    // [4g] 	triples2 	::= 	blankNodePropertyList predicateObjectList? '.' | collection predicateObjectList '.'
    match parser.read.current() {
        b'[' if !is_followed_by_space_and_closing_bracket(&parser.read) => {
            parse_blank_node_property_list(parser, on_triple)?;
            skip_whitespace(&mut parser.read)?;
            if parser.read.current() != b'.' {
                parse_predicate_object_list(parser, on_triple)?;
            }
        }
        _ => {
            parse_collection(parser, on_triple)?;
            skip_whitespace(&mut parser.read)?;
            parse_predicate_object_list(parser, on_triple)?;
        }
    }

    parser.subject_buf_stack.pop();
    parser.subject_type_stack.pop();

    parser.read.check_is_current(b'.')?;
    parser.read.consume()?;
    Ok(())
}

fn parse_wrapped_graph<R: BufRead, E: From<TurtleError>>(
    parser: &mut TurtleParser<R>,
    on_triple: &mut impl FnMut(Triple) -> Result<(), E>,
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

        parse_triples(parser, on_triple)?;
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

fn parse_label_or_subject(
    read: &mut impl LookAheadByteRead,
    buffer: &mut String,
    temp_buffer: &mut String,
    iri_parser: &IriParser,
    namespaces: &HashMap<String, String>,
    bnode_id_generator: &mut BlankNodeIdGenerator,
) -> Result<NamedOrBlankNodeType, TurtleError> {
    //[7g] 	labelOrSubject 	::= 	iri | BlankNode
    Ok(match read.current() {
        b'_' | b'[' => {
            parse_blank_node(read, buffer, bnode_id_generator)?;
            NamedOrBlankNodeType::BlankNode
        }
        _ => {
            parse_iri(read, buffer, temp_buffer, iri_parser, namespaces)?;
            NamedOrBlankNodeType::NamedNode
        }
    })
}

fn parse_prefix_id(
    read: &mut impl LookAheadByteRead,
    namespaces: &mut HashMap<String, String>,
    iri_parser: &IriParser,
    temp_buffer: &mut String,
) -> Result<(), TurtleError> {
    // [4] 	prefixID 	::= 	'@prefix' PNAME_NS IRIREF '.'
    read.consume_many("@prefix".len())?;
    skip_whitespace(read)?;

    let mut prefix = String::default();
    parse_pname_ns(read, &mut prefix)?;
    skip_whitespace(read)?;

    let mut value = String::default();
    parse_iriref_relative(read, &mut value, temp_buffer, iri_parser)?;
    skip_whitespace(read)?;

    read.check_is_current(b'.')?;
    read.consume()?;

    namespaces.insert(prefix, value);
    Ok(())
}

fn parse_base(
    read: &mut impl LookAheadByteRead,
    buffer: &mut String,
    temp_buffer: &mut String,
    iri_parser: &mut IriParser,
) -> Result<(), TurtleError> {
    // [5] 	base 	::= 	'@base' IRIREF '.'
    read.consume_many("@base".len())?;
    skip_whitespace(read)?;

    parse_base_iriref(read, buffer, temp_buffer, iri_parser)?;
    skip_whitespace(read)?;

    read.check_is_current(b'.')?;
    read.consume()?;

    Ok(())
}

fn parse_sparql_base(
    read: &mut impl LookAheadByteRead,
    buffer: &mut String,
    temp_buffer: &mut String,
    iri_parser: &mut IriParser,
) -> Result<(), TurtleError> {
    // [5s] 	sparqlBase 	::= 	"BASE" IRIREF
    read.consume_many("BASE".len())?;
    skip_whitespace(read)?;

    parse_base_iriref(read, buffer, temp_buffer, iri_parser)
}

fn parse_base_iriref(
    read: &mut impl LookAheadByteRead,
    buffer: &mut String,
    temp_buffer: &mut String,
    iri_parser: &mut IriParser,
) -> Result<(), TurtleError> {
    parse_iriref_relative(read, buffer, temp_buffer, iri_parser)?;
    let result = iri_parser
        .set_base_iri(buffer.as_bytes())
        .map_err(|_| read.parse_error(TurtleErrorKind::InvalidBaseIRI));
    buffer.clear();
    result
}

fn parse_sparql_prefix(
    read: &mut impl LookAheadByteRead,
    namespaces: &mut HashMap<String, String>,
    iri_parser: &IriParser,
    temp_buffer: &mut String,
) -> Result<(), TurtleError> {
    // [6s] 	sparqlPrefix 	::= 	"PREFIX" PNAME_NS IRIREF
    read.consume_many("PREFIX".len())?;
    skip_whitespace(read)?;

    let mut prefix = String::default();
    parse_pname_ns(read, &mut prefix)?;
    skip_whitespace(read)?;

    let mut value = String::default();
    parse_iriref_relative(read, &mut value, temp_buffer, iri_parser)?;
    skip_whitespace(read)?;

    namespaces.insert(prefix, value);
    Ok(())
}

fn parse_triples<R: BufRead, E: From<TurtleError>>(
    parser: &mut TurtleParser<R>,
    on_triple: &mut impl FnMut(Triple) -> Result<(), E>,
) -> Result<(), E> {
    // [6] 	triples 	::= 	subject predicateObjectList | blankNodePropertyList predicateObjectList?
    match parser.read.current() {
        b'[' if !is_followed_by_space_and_closing_bracket(&parser.read) => {
            parse_blank_node_property_list(parser, on_triple)?;
            skip_whitespace(&mut parser.read)?;
            if parser.read.current() != b'.' && parser.read.current() != b'}' {
                parse_predicate_object_list(parser, on_triple)?;
            }
        }
        _ => {
            parse_subject(parser, on_triple)?;
            skip_whitespace(&mut parser.read)?;
            parse_predicate_object_list(parser, on_triple)?;
        }
    }

    parser.subject_buf_stack.pop();
    parser.subject_type_stack.pop();
    Ok(())
}

fn parse_predicate_object_list<R: BufRead, E: From<TurtleError>>(
    parser: &mut TurtleParser<R>,
    on_triple: &mut impl FnMut(Triple) -> Result<(), E>,
) -> Result<(), E> {
    // [7] 	predicateObjectList 	::= 	verb objectList (';' (verb objectList)?)*
    loop {
        parse_verb(
            &mut parser.read,
            parser.predicate_buf_stack.push(),
            &mut parser.temp_buf,
            &parser.iri_parser,
            &parser.namespaces,
        )?;
        skip_whitespace(&mut parser.read)?;

        parse_object_list(parser, on_triple)?;
        skip_whitespace(&mut parser.read)?;

        parser.predicate_buf_stack.pop();
        if parser.read.current() != b';' {
            return Ok(());
        }
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

fn parse_object_list<R: BufRead, E: From<TurtleError>>(
    parser: &mut TurtleParser<R>,
    on_triple: &mut impl FnMut(Triple) -> Result<(), E>,
) -> Result<(), E> {
    // [8] 	objectList 	::= 	object (',' object)*
    loop {
        parse_object(parser, on_triple)?;
        skip_whitespace(&mut parser.read)?;

        if parser.read.current() != b',' {
            return Ok(());
        }
        parser.read.consume()?;
        skip_whitespace(&mut parser.read)?;
    }
}

fn parse_verb<'a>(
    read: &mut impl LookAheadByteRead,
    buffer: &'a mut String,
    temp_buffer: &'a mut String,
    iri_parser: &IriParser,
    namespaces: &HashMap<String, String>,
) -> Result<(), TurtleError> {
    // [9] 	verb 	::= 	predicate | 'a'
    match read.current() {
        b'a' => {
            match read.next() {
                // We check that it is not a prefixed URI
                Some(c)
                    if is_possible_pn_chars_ascii(c) || c == b'.' || c == b':' || c > MAX_ASCII =>
                {
                    parse_predicate(read, buffer, temp_buffer, iri_parser, namespaces)
                }
                _ => {
                    buffer.push_str(RDF_TYPE);
                    read.consume()?;
                    Ok(())
                }
            }
        }
        _ => parse_predicate(read, buffer, temp_buffer, iri_parser, namespaces),
    }
}

fn parse_subject<R: BufRead, E: From<TurtleError>>(
    parser: &mut TurtleParser<R>,
    on_triple: &mut impl FnMut(Triple) -> Result<(), E>,
) -> Result<(), E> {
    //[10] 	subject 	::= 	iri | BlankNode | collection
    match parser.read.current() {
        b'_' | b'[' => {
            parser
                .subject_type_stack
                .push(NamedOrBlankNodeType::BlankNode);
            parse_blank_node(
                &mut parser.read,
                parser.subject_buf_stack.push(),
                &mut parser.bnode_id_generator,
            )?;
        }
        b'(' => parse_collection(parser, on_triple)?,
        _ => {
            parser
                .subject_type_stack
                .push(NamedOrBlankNodeType::NamedNode);
            parse_iri(
                &mut parser.read,
                parser.subject_buf_stack.push(),
                &mut parser.temp_buf,
                &parser.iri_parser,
                &parser.namespaces,
            )?;
        }
    };
    Ok(())
}

fn parse_predicate<'a>(
    read: &mut impl LookAheadByteRead,
    buffer: &'a mut String,
    temp_buffer: &'a mut String,
    iri_parser: &IriParser,
    namespaces: &HashMap<String, String>,
) -> Result<(), TurtleError> {
    //[11] 	predicate 	::= 	iri
    parse_iri(read, buffer, temp_buffer, iri_parser, namespaces)
}

fn parse_object<R: BufRead, E: From<TurtleError>>(
    parser: &mut TurtleParser<R>,
    on_triple: &mut impl FnMut(Triple) -> Result<(), E>,
) -> Result<(), E> {
    //[12] 	object 	::= 	iri | BlankNode | collection | blankNodePropertyList | literal

    match parser.read.current() {
        EOF => parser.read.unexpected_char_error()?,
        b'<' => {
            parse_iri(
                &mut parser.read,
                &mut parser.subject_buf_stack.push(),
                &mut parser.temp_buf,
                &parser.iri_parser,
                &parser.namespaces,
            )?;
            emit_triple(parser, TermType::NamedNode, on_triple)?;
        }
        b'(' => {
            parse_collection(parser, on_triple)?;
            let object_type = *parser.subject_type_stack.last().unwrap();
            parser.subject_type_stack.pop();
            emit_triple(parser, object_type.into(), on_triple)?;
        }
        b'[' if !is_followed_by_space_and_closing_bracket(&parser.read) => {
            parse_blank_node_property_list(parser, on_triple)?;
            let object_type = *parser.subject_type_stack.last().unwrap();
            parser.subject_type_stack.pop();
            emit_triple(parser, object_type.into(), on_triple)?;
        }
        b'_' | b'[' => {
            parse_blank_node(
                &mut parser.read,
                &mut parser.subject_buf_stack.push(),
                &mut parser.bnode_id_generator,
            )?;
            emit_triple(parser, TermType::BlankNode, on_triple)?;
        }
        b'"' | b'\'' | b'+' | b'-' | b'.' | b'0'..=b'9' => {
            let object_type = parse_literal(
                &mut parser.read,
                parser.subject_buf_stack.push(),
                &mut parser.object_annotation_buf,
                &mut parser.temp_buf,
                &parser.iri_parser,
                &parser.namespaces,
            )?;
            emit_triple(parser, object_type, on_triple)?;
            parser.object_annotation_buf.clear();
        }
        _ => {
            if parser.read.starts_with(b"true") || parser.read.starts_with(b"false") {
                let object_type = parse_literal(
                    &mut parser.read,
                    parser.subject_buf_stack.push(),
                    &mut parser.object_annotation_buf,
                    &mut parser.temp_buf,
                    &parser.iri_parser,
                    &parser.namespaces,
                )?;
                emit_triple(parser, object_type, on_triple)?;
                parser.object_annotation_buf.clear();
            } else {
                parse_iri(
                    &mut parser.read,
                    parser.subject_buf_stack.push(),
                    &mut parser.temp_buf,
                    &parser.iri_parser,
                    &parser.namespaces,
                )?;
                emit_triple(parser, TermType::NamedNode, on_triple)?;
            }
        }
    };
    parser.subject_buf_stack.pop();
    Ok(())
}

fn emit_triple<'a, R: BufRead, E: From<TurtleError>>(
    parser: &'a TurtleParser<R>,
    object_type: TermType,
    on_triple: &mut impl FnMut(Triple) -> Result<(), E>,
) -> Result<(), E> {
    let subject_buf = parser.subject_buf_stack.before_last();
    let subject_type = parser.subject_type_stack[parser.subject_type_stack.len() - 1];
    let predicate_buf = parser.predicate_buf_stack.last();
    let object_buf = parser.subject_buf_stack.last();
    on_triple(Triple {
        subject: subject_type.with_value(subject_buf),
        predicate: NamedNode { iri: predicate_buf },
        object: object_type.with_value(object_buf, &parser.object_annotation_buf),
    })?;
    Ok(())
}

fn parse_literal<'a>(
    read: &mut impl LookAheadByteRead,
    buffer: &'a mut String,
    annotation_buffer: &'a mut String,
    temp_buffer: &mut String,
    iri_parser: &IriParser,
    namespaces: &HashMap<String, String>,
) -> Result<TermType, TurtleError> {
    // [13] 	literal 	::= 	RDFLiteral | NumericLiteral | BooleanLiteral
    match read.current() {
        b'"' | b'\'' => parse_rdf_literal(
            read,
            buffer,
            annotation_buffer,
            temp_buffer,
            iri_parser,
            namespaces,
        ),
        b'+' | b'-' | b'.' | b'0'..=b'9' => {
            parse_numeric_literal(read, buffer, annotation_buffer)?;
            Ok(TermType::TypedLiteral)
        }
        _ => {
            parse_boolean_literal(read, buffer, annotation_buffer)?;
            Ok(TermType::TypedLiteral)
        }
    }
}

fn parse_blank_node_property_list<R: BufRead, E: From<TurtleError>>(
    parser: &mut TurtleParser<R>,
    on_triple: &mut impl FnMut(Triple) -> Result<(), E>,
) -> Result<(), E> {
    // [15] 	collection 	::= 	'(' object* ')'
    parser.read.check_is_current(b'[')?;
    parser.read.consume()?;
    skip_whitespace(&mut parser.read)?;

    parser
        .subject_buf_stack
        .push()
        .push_str(parser.bnode_id_generator.generate().as_ref());
    parser
        .subject_type_stack
        .push(NamedOrBlankNodeType::BlankNode);

    loop {
        parse_predicate_object_list(parser, on_triple)?;
        skip_whitespace(&mut parser.read)?;

        if parser.read.current() == b']' {
            parser.read.consume()?;
            return Ok(());
        }
    }
}

fn parse_collection<R: BufRead, E: From<TurtleError>>(
    parser: &mut TurtleParser<R>,
    on_triple: &mut impl FnMut(Triple) -> Result<(), E>,
) -> Result<(), E> {
    // [15] 	collection 	::= 	'(' object* ')'
    parser.read.check_is_current(b'(')?;
    parser.read.consume()?;

    parser
        .subject_type_stack
        .push(NamedOrBlankNodeType::BlankNode);
    parser.predicate_buf_stack.push().push_str(RDF_FIRST);

    let mut root: Option<BlankNodeId> = None;
    loop {
        skip_whitespace(&mut parser.read)?;

        if parser.read.current() == EOF {
            return Ok(parser.read.unexpected_char_error()?);
        } else if parser.read.current() == b')' {
            parser.read.consume()?;
            match root {
                Some(id) => {
                    on_triple(Triple {
                        subject: BlankNode {
                            id: parser.subject_buf_stack.last(),
                        }
                        .into(),
                        predicate: NamedNode { iri: RDF_REST },
                        object: NamedNode { iri: RDF_NIL }.into(),
                    })?;
                    parser.subject_buf_stack.pop();
                    parser.subject_buf_stack.push().push_str(id.as_ref());
                }
                None => {
                    parser.subject_buf_stack.push().push_str(RDF_NIL);
                    parser.subject_type_stack.pop();
                    parser
                        .subject_type_stack
                        .push(NamedOrBlankNodeType::NamedNode);
                }
            }
            parser.predicate_buf_stack.pop();
            return Ok(());
        } else {
            let new = parser.bnode_id_generator.generate();
            if root == None {
                root = Some(new);
            } else {
                on_triple(Triple {
                    subject: BlankNode {
                        id: parser.subject_buf_stack.last(),
                    }
                    .into(),
                    predicate: NamedNode { iri: RDF_REST },
                    object: BlankNode { id: new.as_ref() }.into(),
                })?;
                parser.subject_buf_stack.pop();
            }
            parser.subject_buf_stack.push().push_str(new.as_ref());

            parse_object(parser, on_triple)?;
        }
    }
}

fn parse_numeric_literal(
    read: &mut impl LookAheadByteRead,
    buffer: &mut String,
    annotation_buffer: &mut String,
) -> Result<(), TurtleError> {
    // [16] 	NumericLiteral 	::= 	INTEGER | DECIMAL | DOUBLE
    // [19] 	INTEGER 	::= 	[+-]? [0-9]+
    // [20] 	DECIMAL 	::= 	[+-]? [0-9]* '.' [0-9]+
    // [21] 	DOUBLE 	::= 	[+-]? ([0-9]+ '.' [0-9]* EXPONENT | '.' [0-9]+ EXPONENT | [0-9]+ EXPONENT)
    // merged [+-] [0-9]* ('.' [0-9]*)? EXPONENT?
    let c = read.current();
    match c {
        b'+' | b'-' => {
            buffer.push(char::from(c));
            read.consume()?
        }
        _ => (),
    }

    // We read the digits before .
    let mut count_before: usize = 0;
    loop {
        let c = read.current();
        match c {
            b'0'..=b'9' => {
                buffer.push(char::from(c));
                read.consume()?;
                count_before += 1;
            }
            _ => break,
        }
    }

    // We read the digits after .
    let count_after = if read.current() == b'.' {
        //We check if it is not the end of a statement
        if let Some(c) = read.next() {
            match c {
                b'0'..=b'9' | b'e' | b'E' => (),
                _ => {
                    return if count_before > 0 {
                        annotation_buffer.push_str(XSD_INTEGER);
                        Ok(())
                    } else {
                        read.unexpected_char_error()
                    }
                }
            }
        } else {
            return if count_before > 0 {
                annotation_buffer.push_str(XSD_INTEGER);
                Ok(())
            } else {
                read.unexpected_char_error()
            };
        }

        buffer.push('.');
        let mut count_after = 0;

        loop {
            read.consume()?;
            let c = read.current();
            match c {
                b'0'..=b'9' => {
                    buffer.push(char::from(c));
                    count_after += 1;
                }
                _ => break,
            }
        }
        Some(count_after)
    } else {
        None
    };

    // End
    let c = read.current();
    match c {
        b'e' | b'E' => {
            if count_before > 0 || count_after.unwrap_or(0) > 0 {
                parse_exponent(read, buffer)?;
                annotation_buffer.push_str(XSD_DOUBLE);
                Ok(())
            } else {
                read.unexpected_char_error()
            }
        }
        _ => {
            if count_after.is_none() && count_before > 0 {
                annotation_buffer.push_str(XSD_INTEGER);
                Ok(())
            } else if count_after != None && count_after != Some(0) {
                annotation_buffer.push_str(XSD_DECIMAL);
                Ok(())
            } else {
                read.unexpected_char_error()
            }
        }
    }
}

fn parse_rdf_literal(
    read: &mut impl LookAheadByteRead,
    buffer: &mut String,
    annotation_buffer: &mut String,
    temp_buffer: &mut String,
    iri_parser: &IriParser,
    namespaces: &HashMap<String, String>,
) -> Result<TermType, TurtleError> {
    // [128s] 	RDFLiteral 	::= 	String (LANGTAG | '^^' iri)?
    parse_string(read, buffer)?;
    skip_whitespace(read)?;

    match read.current() {
        b'@' => {
            parse_langtag(read, annotation_buffer)?;
            Ok(TermType::LanguageTaggedString)
        }
        b'^' => {
            read.consume()?;
            read.check_is_current(b'^')?;
            read.consume()?;
            skip_whitespace(read)?;
            parse_iri(read, annotation_buffer, temp_buffer, iri_parser, namespaces)?;
            Ok(TermType::TypedLiteral)
        }
        _ => Ok(TermType::SimpleLiteral),
    }
}

fn parse_boolean_literal(
    read: &mut impl LookAheadByteRead,
    buffer: &mut String,
    annotation_buffer: &mut String,
) -> Result<(), TurtleError> {
    if read.starts_with(b"true") {
        read.consume_many("true".len())?;
        buffer.push_str("true");
        annotation_buffer.push_str(XSD_BOOLEAN);
        Ok(())
    } else if read.starts_with(b"false") {
        read.consume_many("false".len())?;
        buffer.push_str("false");
        annotation_buffer.push_str(XSD_BOOLEAN);
        Ok(())
    } else {
        read.unexpected_char_error()
    }
}

fn parse_string(read: &mut impl LookAheadByteRead, buffer: &mut String) -> Result<(), TurtleError> {
    match read.current() {
        b'"' => {
            if read.starts_with(b"\"\"\"") {
                parse_string_literal_long_quote(read, buffer)
            } else {
                parse_string_literal_quote(read, buffer)
            }
        }
        b'\'' => {
            if read.starts_with(b"'''") {
                parse_string_literal_long_single_quote(read, buffer)
            } else {
                parse_string_literal_single_quote(read, buffer)
            }
        }
        _ => read.unexpected_char_error(),
    }
}

fn parse_iri(
    read: &mut impl LookAheadByteRead,
    buffer: &mut String,
    temp_buffer: &mut String,
    iri_parser: &IriParser,
    namespaces: &HashMap<String, String>,
) -> Result<(), TurtleError> {
    // [135s] 	iri 	::= 	IRIREF | PrefixedName
    match read.current() {
        b'<' => {
            parse_iriref_relative(read, buffer, temp_buffer, iri_parser)?;
        }
        _ => parse_prefixed_name(read, buffer, namespaces)?,
    }
    Ok(())
}

fn parse_prefixed_name<'a>(
    read: &mut impl LookAheadByteRead,
    buffer: &'a mut String,
    namespaces: &HashMap<String, String>,
) -> Result<(), TurtleError> {
    // [136s] 	PrefixedName 	::= 	PNAME_LN | PNAME_NS
    // It could be written: PNAME_NS PN_LOCAL?

    // PNAME_NS
    parse_pname_ns(read, buffer)?;
    if let Some(value) = namespaces.get(buffer.as_str()) {
        buffer.clear();
        buffer.push_str(value);
    } else {
        return Err(read.parse_error(TurtleErrorKind::UnknownPrefix(buffer.clone())));
    }

    // [168s] 	PN_LOCAL 	::= 	(PN_CHARS_U | ':' | [0-9] | PLX) ((PN_CHARS | '.' | ':' | PLX)* (PN_CHARS | ':' | PLX))?
    match read.current() {
        b'\\' => parse_pn_local_esc(read, buffer)?,
        b'%' => parse_percent(read, buffer)?,
        b':' | b'0'..=b'9' => buffer.push(char::from(read.current())),
        c if is_possible_pn_chars_u_ascii(c) => buffer.push(char::from(c)),
        _ => {
            let c = read_utf8_char(read)?;
            if is_possible_pn_chars_u_unicode(c) {
                buffer.push(c)
            } else {
                return Ok(());
            }
        }
    }

    loop {
        read.consume()?;
        match read.current() {
            b'.' => {
                if has_future_char_valid_pname_local(read) {
                    buffer.push('.')
                } else {
                    return Ok(());
                }
            }
            b'\\' => parse_pn_local_esc(read, buffer)?,
            b'%' => parse_percent(read, buffer)?,
            b':' => buffer.push(':'),
            c if is_possible_pn_chars_ascii(c) => buffer.push(char::from(c)),
            _ => {
                let c = read_utf8_char(read)?;
                if is_possible_pn_chars_unicode(c) {
                    buffer.push(c)
                } else {
                    return Ok(());
                }
            }
        }
    }
}

fn has_future_char_valid_pname_local(read: &impl LookAheadByteRead) -> bool {
    let mut i = 1;
    loop {
        match read.ahead(i) {
            Some(b':') | Some(b'%') | Some(b'\\') => return true,
            Some(c) if c > MAX_ASCII || is_possible_pn_chars_ascii(c) => return true,
            Some(b'.') => (),
            _ => return false,
        }
        i += 1;
    }
}

fn parse_blank_node<'a>(
    read: &mut impl LookAheadByteRead,
    buffer: &'a mut String,
    bnode_id_generator: &mut BlankNodeIdGenerator,
) -> Result<(), TurtleError> {
    // [137s] 	BlankNode 	::= 	BLANK_NODE_LABEL | ANON
    match read.current() {
        b'_' => {
            parse_blank_node_label(read, buffer)?;
        }
        b'[' => {
            parse_anon(read, buffer, bnode_id_generator)?;
        }
        _ => read.unexpected_char_error()?,
    }
    Ok(())
}

fn parse_pname_ns(
    read: &mut impl LookAheadByteRead,
    buffer: &mut String,
) -> Result<(), TurtleError> {
    // [139s] 	PNAME_NS 	::= 	PN_PREFIX? ':'
    parse_pn_prefix(read, buffer)?;
    match read.current() {
        b':' => {
            read.consume()?;
            Ok(())
        }
        _ => read.unexpected_char_error()?,
    }
}

fn parse_exponent(
    read: &mut impl LookAheadByteRead,
    buffer: &mut String,
) -> Result<(), TurtleError> {
    // [154s] 	EXPONENT 	::= 	[eE] [+-]? [0-9]+
    let c = read.current();
    match c {
        b'e' | b'E' => buffer.push(char::from(c)),
        _ => read.unexpected_char_error()?,
    };
    read.consume()?;

    let c = read.current();
    match c {
        b'+' | b'-' => {
            buffer.push(char::from(c));
            read.consume()?
        }
        _ => (),
    }

    let c = read.current();
    match c {
        b'0'..=b'9' => buffer.push(char::from(c)),
        _ => read.unexpected_char_error()?,
    }

    loop {
        read.consume()?;
        let c = read.current();
        match c {
            b'0'..=b'9' => buffer.push(char::from(c)),
            _ => return Ok(()),
        }
    }
}

fn parse_string_literal_single_quote(
    read: &mut impl LookAheadByteRead,
    buffer: &mut String,
) -> Result<(), TurtleError> {
    // [23] 	STRING_LITERAL_SINGLE_QUOTE 	::= 	"'" ([^#x27#x5C#xA#xD] | ECHAR | UCHAR)* "'" /* #x27=' #x5C=\ #xA=new line #xD=carriage return */
    parse_string_literal_quote_inner(read, buffer, b'\'')
}

fn parse_string_literal_long_single_quote(
    read: &mut impl LookAheadByteRead,
    buffer: &mut String,
) -> Result<(), TurtleError> {
    // [24] 	STRING_LITERAL_LONG_SINGLE_QUOTE 	::= 	"'''" (("'" | "''")? ([^'\] | ECHAR | UCHAR))* "'''"
    parse_string_literal_long_quote_inner(read, buffer, b'\'')
}

fn parse_string_literal_long_quote(
    read: &mut impl LookAheadByteRead,
    buffer: &mut String,
) -> Result<(), TurtleError> {
    // [25] 	STRING_LITERAL_LONG_QUOTE 	::= 	'"""' (('"' | '""')? ([^"\] | ECHAR | UCHAR))* '"""'
    parse_string_literal_long_quote_inner(read, buffer, b'"')
}

fn parse_string_literal_long_quote_inner(
    read: &mut impl LookAheadByteRead,
    buffer: &mut String,
    quote: u8,
) -> Result<(), TurtleError> {
    let prefix = [quote; 3];
    read.consume_many(2)?;
    loop {
        read.consume()?;
        match read.current() {
            c if c == quote && read.starts_with(&prefix) => {
                read.consume_many(3)?;
                return Ok(());
            }
            b'\\' => parse_echar_or_uchar(read, buffer)?,
            EOF => read.unexpected_char_error()?,
            c => buffer.push(if c <= 0x7F {
                char::from(c) //optimization to avoid UTF-8 decoding
            } else {
                read_utf8_char(read)?
            }),
        }
    }
}

fn parse_anon(
    read: &mut impl LookAheadByteRead,
    buffer: &mut String,
    bnode_id_generator: &mut BlankNodeIdGenerator,
) -> Result<(), TurtleError> {
    read.check_is_current(b'[')?;
    read.consume()?;
    skip_whitespace(read)?;

    read.check_is_current(b']')?;
    read.consume()?;

    buffer.push_str(bnode_id_generator.generate().as_ref());
    Ok(())
}

fn parse_pn_prefix(
    read: &mut impl LookAheadByteRead,
    buffer: &mut String,
) -> Result<(), TurtleError> {
    // [167s] 	PN_PREFIX 	::= 	PN_CHARS_BASE ((PN_CHARS | '.')* PN_CHARS)?
    match read.current() {
        c if c <= MAX_ASCII && is_possible_pn_chars_base_ascii(c) => buffer.push(char::from(c)),
        _ => {
            let c = read_utf8_char(read)?;
            if is_possible_pn_chars_base_unicode(c) {
                buffer.push(c)
            } else {
                return Ok(()); //PN_PREFIX is always optional
            }
        }
    }

    loop {
        read.consume()?;
        match read.current() {
            b'.' => match read.next() {
                Some(c) if is_possible_pn_chars_ascii(c) || c > MAX_ASCII => buffer.push('.'),
                _ => {
                    return Ok(());
                }
            },
            c if c <= MAX_ASCII && is_possible_pn_chars_ascii(c) => buffer.push(char::from(c)),
            _ => {
                let c = read_utf8_char(read)?;
                if is_possible_pn_chars_unicode(c) {
                    buffer.push(c)
                } else {
                    return Ok(());
                }
            }
        }
    }
}

fn parse_percent(
    read: &mut impl LookAheadByteRead,
    buffer: &mut String,
) -> Result<(), TurtleError> {
    // [170s] 	PERCENT 	::= 	'%' HEX HEX
    read.check_is_current(b'%')?;
    buffer.push('%');
    read.consume()?;
    parse_hex(read, buffer)?;
    read.consume()?;
    parse_hex(read, buffer)?;
    Ok(())
}

fn parse_hex(read: &mut impl LookAheadByteRead, buffer: &mut String) -> Result<(), TurtleError> {
    // [171s] 	HEX 	::= 	[0-9] | [A-F] | [a-f]
    let c = read.current();
    match c {
        b'0'..=b'9' | b'a'..=b'f' | b'A'..=b'F' => {
            buffer.push(char::from(c));
            Ok(())
        }
        _ => read.unexpected_char_error(),
    }
}

fn parse_pn_local_esc(
    read: &mut impl LookAheadByteRead,
    buffer: &mut String,
) -> Result<(), TurtleError> {
    // [172s] 	PN_LOCAL_ESC 	::= 	'\' ('_' | '~' | '.' | '-' | '!' | '$' | '&' | "'" | '(' | ')' | '*' | '+' | ',' | ';' | '=' | '/' | '?' | '#' | '@' | '%')
    read.check_is_current(b'\\')?;
    read.consume()?;
    let c = read.current();
    match c {
        b'_' | b'~' | b'.' | b'-' | b'!' | b'$' | b'&' | b'\'' | b'(' | b')' | b'*' | b'+'
        | b',' | b';' | b'=' | b'/' | b'?' | b'#' | b'@' | b'%' => {
            buffer.push(char::from(c));
            Ok(())
        }
        _ => read.unexpected_char_error(),
    }
}

fn skip_whitespace(read: &mut impl LookAheadByteRead) -> Result<(), TurtleError> {
    loop {
        match read.current() {
            b' ' | b'\t' | b'\n' | b'\r' => read.consume()?,
            b'#' => {
                while read.current() != b'\r' && read.current() != b'\n' && read.current() != EOF {
                    read.consume()?;
                }
            }
            _ => return Ok(()),
        }
    }
}

fn is_followed_by_space_and_closing_bracket(read: &impl LookAheadByteRead) -> bool {
    for i in 1.. {
        match read.ahead(i) {
            Some(b' ') | Some(b'\t') | Some(b'\n') | Some(b'\r') => (),
            Some(b']') => return true,
            _ => return false,
        }
    }
    false
}

#[derive(Clone, Copy)]
enum NamedOrBlankNodeType {
    NamedNode,
    BlankNode,
}

impl NamedOrBlankNodeType {
    fn with_value<'a>(&self, value: &'a str) -> NamedOrBlankNode<'a> {
        match self {
            NamedOrBlankNodeType::NamedNode => NamedNode { iri: value }.into(),
            NamedOrBlankNodeType::BlankNode => BlankNode { id: value }.into(),
        }
    }
}

#[derive(Clone, Copy)]
enum TermType {
    NamedNode,
    BlankNode,
    SimpleLiteral,
    LanguageTaggedString,
    TypedLiteral,
}

impl TermType {
    fn with_value<'a>(&self, value: &'a str, annotation: &'a str) -> Term<'a> {
        match self {
            TermType::NamedNode => NamedNode { iri: value }.into(),
            TermType::BlankNode => BlankNode { id: value }.into(),
            TermType::SimpleLiteral => Literal::Simple { value }.into(),
            TermType::LanguageTaggedString => Literal::LanguageTaggedString {
                value,
                language: annotation,
            }
            .into(),
            TermType::TypedLiteral => Literal::Typed {
                value,
                datatype: NamedNode { iri: annotation },
            }
            .into(),
        }
    }
}

impl From<NamedOrBlankNodeType> for TermType {
    fn from(t: NamedOrBlankNodeType) -> Self {
        match t {
            NamedOrBlankNodeType::NamedNode => TermType::NamedNode,
            NamedOrBlankNodeType::BlankNode => TermType::BlankNode,
        }
    }
}

fn on_triple_in_graph<'a, E>(
    on_quad: &'a mut impl FnMut(Quad) -> Result<(), E>,
    graph_name: Option<NamedOrBlankNode<'a>>,
) -> impl FnMut(Triple) -> Result<(), E> + 'a {
    move |t: Triple| {
        on_quad(Quad {
            subject: t.subject,
            predicate: t.predicate,
            object: t.object,
            graph_name,
        })
    }
}
