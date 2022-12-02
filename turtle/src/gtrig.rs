//! Implementation of a generalized RDF / RDF-star version of the Trig syntax

use crate::error::*;
use crate::gnquads::parse_variable;
use crate::gtriple_allocator::GeneralizedTripleAllocator;
use crate::shared::*;
use crate::turtle::*;
use crate::utils::*;
use oxiri::Iri;
use rio_api::model::*;
use rio_api::parser::GeneralizedQuadsParser;
use std::collections::HashMap;
use std::io::BufRead;
use std::mem::swap;

/// A [TriG](https://www.w3.org/TR/trig/) streaming parser parsing generalized quads.
///
/// Warning: RDF-star is not supported yet.
///
/// It implements the `GeneralizedQuadsParser` trait.
/// Using it requires to enable the `generalized` feature.
///
///
/// Count the number of people using the `QuadsParser` API:
/// ```
/// use rio_turtle::{GTriGParser, TurtleError};
/// use rio_api::parser::GeneralizedQuadsParser;
/// use rio_api::model::NamedNode;
///
/// let file = b"@prefix schema: <http://schema.org/> .
/// <http://example/> {
///     <http://example.com/foo> a schema:Person ;
///         schema:name  ?name .
///     <http://example.com/bar> a schema:Person ;
///         schema:name  ?name .
/// }";
///
/// let rdf_type = NamedNode { iri: "http://www.w3.org/1999/02/22-rdf-syntax-ns#type" };
/// let schema_person = NamedNode { iri: "http://schema.org/Person" };
/// let mut count = 0;
/// GTriGParser::new(file.as_ref(), None).parse_all(&mut |t| {
///     if t.predicate == rdf_type.into() && t.object == schema_person.into() {
///         count += 1;
///     }
///     Ok(()) as Result<(), TurtleError>
/// })?;
/// assert_eq!(2, count);
/// # Result::<_,rio_turtle::TurtleError>::Ok(())
/// ```
pub struct GTriGParser<R: BufRead> {
    read: LookAheadByteReader<R>,
    base_iri: Option<Iri<String>>,
    prefixes: HashMap<String, String>,
    bnode_id_generator: BlankNodeIdGenerator,
    triple_alloc: GeneralizedTripleAllocator,
    graph_name_alloc: GeneralizedTripleAllocator,
    temp_buf: String,
}

impl<R: BufRead> GTriGParser<R> {
    /// Builds the parser from a `BufRead` implementation, and a base IRI for relative IRI resolution.
    pub fn new(reader: R, base_iri: Option<Iri<String>>) -> Self {
        Self {
            read: LookAheadByteReader::new(reader),
            base_iri,
            prefixes: HashMap::default(),
            bnode_id_generator: BlankNodeIdGenerator::default(),
            triple_alloc: GeneralizedTripleAllocator::new(),
            graph_name_alloc: GeneralizedTripleAllocator::new(),
            temp_buf: String::with_capacity(64),
        }
    }

    fn make_quad(&self) -> GeneralizedQuad<'_> {
        self.triple_alloc
            .top_quad(self.graph_name_alloc.current_subject())
    }
}

impl<R: BufRead> GeneralizedQuadsParser for GTriGParser<R> {
    type Error = TurtleError;

    fn parse_step<E: From<TurtleError>>(
        &mut self,
        on_quad: &mut impl FnMut(GeneralizedQuad<'_>) -> Result<(), E>,
    ) -> Result<(), E> {
        self.parse_generalized_block_or_directive(on_quad)
    }

    fn is_end(&self) -> bool {
        self.read.current().is_none()
    }
}

impl<R: BufRead> GTriGParser<R> {
    fn parse_generalized_block_or_directive<E: From<TurtleError>>(
        &mut self,
        on_quad: &mut impl FnMut(GeneralizedQuad<'_>) -> Result<(), E>,
    ) -> Result<(), E> {
        // [1g] 	trigDoc 	::= 	(directive | block)*
        // [2g] 	block 	::= 	triplesOrGraph | wrappedGraph | triples2 | "GRAPH" labelOrSubject wrappedGraph
        skip_whitespace(&mut self.read)?;

        if self.read.current().is_none() {
            Ok(())
        } else if self.read.starts_with(b"@prefix") {
            self.parse_generalized_prefix_id()?;
            Ok(())
        } else if self.read.starts_with(b"@base") {
            self.base_iri = Some(parse_base(
                &mut self.read,
                &mut self.temp_buf,
                &self.base_iri,
            )?);
            Ok(())
        } else if self.read.starts_with_ignore_ascii_case(b"BASE") {
            self.base_iri = Some(parse_sparql_base(
                &mut self.read,
                &mut self.temp_buf,
                &self.base_iri,
            )?);
            Ok(())
        } else if self.read.starts_with_ignore_ascii_case(b"PREFIX") {
            self.parse_generalized_sparql_prefix()?;
            Ok(())
        } else if self.read.starts_with_ignore_ascii_case(b"GRAPH") {
            self.read.consume_many("GRAPH".len())?;
            skip_whitespace(&mut self.read)?;
            self.graph_name_alloc.push_triple_start();
            self.parse_generalized_term(0, true)?;
            skip_whitespace(&mut self.read)?;
            self.parse_generalized_wrapped_graph(on_quad)?;
            self.graph_name_alloc.pop_term(0);
            self.graph_name_alloc.pop_top_empty_triple();

            debug_assert_eq!(self.triple_alloc.complete_len(), 0);
            debug_assert_eq!(self.triple_alloc.incomplete_len(), 0);
            debug_assert_eq!(self.graph_name_alloc.complete_len(), 0);
            debug_assert_eq!(self.graph_name_alloc.incomplete_len(), 0);

            Ok(())
        } else if self.read.current() == Some(b'{') {
            self.parse_generalized_wrapped_graph(on_quad)?;

            debug_assert_eq!(self.triple_alloc.complete_len(), 0);
            debug_assert_eq!(self.triple_alloc.incomplete_len(), 0);
            debug_assert_eq!(self.graph_name_alloc.complete_len(), 0);
            debug_assert_eq!(self.graph_name_alloc.incomplete_len(), 0);

            Ok(())
        } else if self.read.current() == Some(b'[')
            && !is_followed_by_space_and_closing_bracket(&mut self.read)?
            || self.read.current() == Some(b'(')
        {
            self.parse_generalized_triples2(on_quad)?;

            debug_assert_eq!(self.triple_alloc.complete_len(), 0);
            debug_assert_eq!(self.triple_alloc.incomplete_len(), 0);
            debug_assert_eq!(self.graph_name_alloc.complete_len(), 0);
            debug_assert_eq!(self.graph_name_alloc.incomplete_len(), 0);

            Ok(())
        } else {
            self.parse_generalized_triples_or_graph(on_quad)?;

            debug_assert_eq!(self.triple_alloc.complete_len(), 0);
            debug_assert_eq!(self.triple_alloc.incomplete_len(), 0);
            debug_assert_eq!(self.graph_name_alloc.complete_len(), 0);
            debug_assert_eq!(self.graph_name_alloc.incomplete_len(), 0);

            Ok(())
        }
    }

    fn parse_generalized_prefix_id(&mut self) -> Result<(), TurtleError> {
        // [4] 	prefixID 	::= 	'@prefix' PNAME_NS IRIREF '.'
        self.read.consume_many("@prefix".len())?;
        skip_whitespace(&mut self.read)?;

        let mut prefix = String::default();
        parse_pname_ns(&mut self.read, &mut prefix)?;
        skip_whitespace(&mut self.read)?;

        let mut value = String::default();
        parse_generalized_iriref(
            &mut self.read,
            &mut value,
            &mut self.temp_buf,
            self.base_iri.as_ref(),
        )?;
        skip_whitespace(&mut self.read)?;

        self.read.check_is_current(b'.')?;
        self.read.consume()?;

        self.prefixes.insert(prefix, value);
        Ok(())
    }

    fn parse_generalized_sparql_prefix(&mut self) -> Result<(), TurtleError> {
        // [6s] 	sparqlPrefix 	::= 	"PREFIX" PNAME_NS IRIREF
        self.read.consume_many("PREFIX".len())?;
        skip_whitespace(&mut self.read)?;

        let mut prefix = String::default();
        parse_pname_ns(&mut self.read, &mut prefix)?;
        skip_whitespace(&mut self.read)?;

        let mut value = String::default();
        parse_generalized_iriref(
            &mut self.read,
            &mut value,
            &mut self.temp_buf,
            self.base_iri.as_ref(),
        )?;
        skip_whitespace(&mut self.read)?;

        self.prefixes.insert(prefix, value);
        Ok(())
    }

    fn parse_generalized_wrapped_graph<E: From<TurtleError>>(
        &mut self,
        on_quad: &mut impl FnMut(GeneralizedQuad<'_>) -> Result<(), E>,
    ) -> Result<(), E> {
        // [5g] 	wrappedGraph 	::= 	'{' triplesBlock? '}'
        // [6g] 	triplesBlock 	::= 	triples ('.' triplesBlock?)?
        self.read.check_is_current(b'{')?;
        self.read.consume()?;
        skip_whitespace(&mut self.read)?;

        loop {
            if self.read.current() == Some(b'}') {
                self.read.consume()?;
                break;
            }

            self.parse_generalized_triples(on_quad)?;
            debug_assert_eq!(self.triple_alloc.complete_len(), 0);
            debug_assert_eq!(self.triple_alloc.incomplete_len(), 0);
            match self.read.required_current()? {
                b'.' => {
                    self.read.consume()?;
                    skip_whitespace(&mut self.read)?;
                }
                b'}' => {
                    self.read.consume()?;
                    break;
                }
                _ => self.read.unexpected_char_error()?,
            }
        }
        Ok(())
    }

    fn parse_generalized_triples<E: From<TurtleError>>(
        &mut self,
        on_quad: &mut impl FnMut(GeneralizedQuad<'_>) -> Result<(), E>,
    ) -> Result<(), E> {
        // [6] 	triples 	::= 	subject predicateObjectList | blankNodePropertyList predicateObjectList?
        match self.read.current() {
            Some(b'[') if !is_followed_by_space_and_closing_bracket(&mut self.read)? => {
                let bn = self.parse_generalized_blank_node_property_list(on_quad)?;
                skip_whitespace(&mut self.read)?;
                if self.read.current() != Some(b'.') && self.read.current() != Some(b'}') {
                    self.triple_alloc.push_triple_start();
                    self.triple_alloc.try_push_atom(0, |b, _| {
                        b.push_str(bn.as_ref());
                        Ok(GeneralizedTerm::from(BlankNode { id: b }))
                    })?;
                    self.parse_generalized_predicate_object_list(on_quad)?;
                    self.triple_alloc.pop_term(0);
                    self.triple_alloc.pop_top_empty_triple();
                }
            }
            _ => {
                self.triple_alloc.push_triple_start();
                self.parse_generalized_node(0, on_quad)?;
                skip_whitespace(&mut self.read)?;
                self.parse_generalized_predicate_object_list(on_quad)?;
                self.triple_alloc.pop_term(0);
                self.triple_alloc.pop_top_empty_triple();
            }
        }
        Ok(())
    }

    fn parse_generalized_triples2<E: From<TurtleError>>(
        &mut self,
        on_quad: &mut impl FnMut(GeneralizedQuad<'_>) -> Result<(), E>,
    ) -> Result<(), E> {
        // [4g] 	triples2 	::= 	blankNodePropertyList predicateObjectList? '.' | collection predicateObjectList '.'
        match self.read.current() {
            Some(b'[') => {
                let bn = self.parse_generalized_blank_node_property_list(on_quad)?;
                skip_whitespace(&mut self.read)?;
                if self.read.current() != Some(b'.') {
                    self.triple_alloc.push_triple_start();
                    self.triple_alloc.try_push_atom(0, |b, _| {
                        b.push_str(bn.as_ref());
                        Ok(GeneralizedTerm::from(BlankNode { id: b }))
                    })?;
                    self.parse_generalized_predicate_object_list(on_quad)?;
                    self.triple_alloc.pop_term(0);
                    self.triple_alloc.pop_top_empty_triple();
                }
            }
            _ => {
                let collec = self.parse_generalized_collection(on_quad)?;
                self.triple_alloc.push_triple_start();
                self.triple_alloc
                    .try_push_atom(0, |b, _| allocate_collection(collec, b))?;
                skip_whitespace(&mut self.read)?;
                self.parse_generalized_predicate_object_list(on_quad)?;
                self.triple_alloc.pop_term(0);
                self.triple_alloc.pop_top_empty_triple();
            }
        }
        self.read.check_is_current(b'.')?;
        self.read.consume()?;
        Ok(())
    }

    fn parse_generalized_triples_or_graph<E: From<TurtleError>>(
        &mut self,
        on_quad: &mut impl FnMut(GeneralizedQuad<'_>) -> Result<(), E>,
    ) -> Result<(), E> {
        // [3g] 	triplesOrGraph 	::= 	labelOrSubject (wrappedGraph | predicateObjectList '.')
        self.triple_alloc.push_triple_start();
        self.parse_generalized_node(0, on_quad)?;
        skip_whitespace(&mut self.read)?;

        if self.read.current() == Some(b'{') {
            // what was supposed to be a subject is in fact a graph name
            swap(&mut self.triple_alloc, &mut self.graph_name_alloc);
            self.parse_generalized_wrapped_graph(on_quad)?;
            self.graph_name_alloc.pop_term(0);
            self.graph_name_alloc.pop_top_empty_triple();
        } else {
            self.parse_generalized_predicate_object_list(on_quad)?;
            self.triple_alloc.pop_term(0);
            self.triple_alloc.pop_top_empty_triple();

            self.read.check_is_current(b'.')?;
            self.read.consume()?;
        }
        Ok(())
    }

    fn parse_generalized_blank_node_property_list<E: From<TurtleError>>(
        &mut self,
        on_quad: &mut impl FnMut(GeneralizedQuad<'_>) -> Result<(), E>,
    ) -> Result<BlankNodeId, E> {
        self.read.check_is_current(b'[')?;
        self.read.consume()?;
        skip_whitespace(&mut self.read)?;

        let id = self.bnode_id_generator.generate();
        if self.read.current() == Some(b']') {
            self.read.consume()?;
            return Ok(id);
        }

        self.triple_alloc.push_triple_start();
        self.triple_alloc.try_push_atom(0, |b, _| {
            b.push_str(id.as_ref());
            Ok(GeneralizedTerm::from(BlankNode { id: b }))
        })?;

        loop {
            self.parse_generalized_predicate_object_list(on_quad)?;
            skip_whitespace(&mut self.read)?;

            if self.read.current() == Some(b']') {
                break;
            }
        }
        self.read.consume()?;
        self.triple_alloc.pop_term(0);
        self.triple_alloc.pop_top_empty_triple();
        Ok(id)
    }

    fn parse_generalized_collection<E: From<TurtleError>>(
        &mut self,
        on_quad: &mut impl FnMut(GeneralizedQuad<'_>) -> Result<(), E>,
    ) -> Result<Option<BlankNodeId>, E> {
        // [15] 	collection 	::= 	'(' object* ')'
        self.read.check_is_current(b'(')?;
        self.read.consume()?;
        let mut root: Option<BlankNodeId> = None;
        loop {
            skip_whitespace(&mut self.read)?;

            if self.read.current().is_none() {
                self.read.unexpected_char_error()?;
                unreachable!(); // unexpected_char always errs
            } else if self.read.current() != Some(b')') {
                let new = self.bnode_id_generator.generate();
                if root.is_none() {
                    root = Some(new);
                    self.triple_alloc.push_triple_start();
                } else {
                    self.triple_alloc.try_push_atom(1, |_, _| {
                        Ok(GeneralizedTerm::from(NamedNode { iri: RDF_REST }))
                    })?;
                    self.triple_alloc.try_push_atom(2, |b, _| {
                        b.push_str(new.as_ref());
                        Ok(GeneralizedTerm::from(BlankNode { id: b }))
                    })?;
                    on_quad(self.make_quad())?;
                    self.triple_alloc.pop_term(2);
                    self.triple_alloc.pop_term(1);
                    self.triple_alloc.pop_term(0);
                }

                self.triple_alloc.try_push_atom(0, |b, _| {
                    b.push_str(new.as_ref());
                    Ok(GeneralizedTerm::from(BlankNode { id: b }))
                })?;
                self.triple_alloc.try_push_atom(1, |_, _| {
                    Ok(GeneralizedTerm::from(NamedNode { iri: RDF_FIRST }))
                })?;
                self.parse_generalized_node(2, on_quad)?;
                on_quad(self.make_quad())?;
                self.triple_alloc.pop_term(2);
                self.triple_alloc.pop_term(1);
            } else {
                // trailing ')'
                break;
            }
        }
        self.read.consume()?;
        if root.is_some() {
            self.triple_alloc.try_push_atom(1, |_, _| {
                Ok(GeneralizedTerm::from(NamedNode { iri: RDF_REST }))
            })?;
            self.triple_alloc.try_push_atom(2, |_, _| {
                Ok(GeneralizedTerm::from(NamedNode { iri: RDF_NIL }))
            })?;
            on_quad(self.make_quad())?;
            self.triple_alloc.pop_top_triple();
        }
        Ok(root)
    }

    fn parse_generalized_predicate_object_list<E: From<TurtleError>>(
        &mut self,
        on_quad: &mut impl FnMut(GeneralizedQuad<'_>) -> Result<(), E>,
    ) -> Result<(), E> {
        // [7] 	predicateObjectList 	::= 	verb objectList (';' (verb objectList)?)*
        loop {
            self.parse_generalized_verb(on_quad)?;
            skip_whitespace(&mut self.read)?;

            self.parse_generalized_object_list(on_quad)?;
            skip_whitespace(&mut self.read)?;

            self.triple_alloc.pop_term(1);
            if self.read.current() != Some(b';') {
                return Ok(());
            }
            while self.read.current() == Some(b';') {
                self.read.consume()?;
                skip_whitespace(&mut self.read)?;
            }
            match self.read.current() {
                Some(b'.') | Some(b']') | Some(b'}') | None => return Ok(()),
                Some(b'|') => return Ok(()),
                _ => (), //continue
            }
        }
    }

    fn parse_generalized_verb<E: From<TurtleError>>(
        &mut self,
        on_quad: &mut impl FnMut(GeneralizedQuad<'_>) -> Result<(), E>,
    ) -> Result<(), E> {
        // [9] 	verb 	::= 	predicate | 'a'
        if self.read.current() == Some(b'a') {
            match self.read.next()? {
                // We check that it is not a prefixed URI
                Some(c)
                    if is_possible_pn_chars_ascii(c) || c == b'.' || c == b':' || c > MAX_ASCII =>
                {
                    self.parse_generalized_node(1, on_quad)
                }
                _ => {
                    self.read.consume()?;
                    self.triple_alloc.try_push_atom(1, |_, _| {
                        Ok(GeneralizedTerm::from(NamedNode { iri: RDF_TYPE }))
                    })
                }
            }
        } else {
            self.parse_generalized_node(1, on_quad)
        }
    }

    fn parse_generalized_object_list<E: From<TurtleError>>(
        &mut self,
        on_quad: &mut impl FnMut(GeneralizedQuad<'_>) -> Result<(), E>,
    ) -> Result<(), E> {
        // [8] 	objectList 	::= 	object (',' object)*
        loop {
            self.parse_generalized_node(2, on_quad)?;
            on_quad(self.make_quad())?;

            skip_whitespace(&mut self.read)?;
            if self.read.current() == Some(b'{') {
                self.read.check_is_next(b'|')?;
                self.read.consume_many(2)?;
                skip_whitespace(&mut self.read)?;

                self.triple_alloc.push_triple_start();
                self.triple_alloc.push_quoted_triple(0);
                self.parse_generalized_predicate_object_list(on_quad)?;

                self.read.check_is_current(b'|')?;
                self.read.check_is_next(b'}')?;
                self.read.consume_many(2)?;
                skip_whitespace(&mut self.read)?;
                self.triple_alloc.pop_annotation_triple();
            }

            self.triple_alloc.pop_term(2);
            if self.read.current() != Some(b',') {
                return Ok(());
            }
            self.read.consume()?;
            skip_whitespace(&mut self.read)?;
        }
    }

    fn parse_generalized_node<E: From<TurtleError>>(
        &mut self,
        pos: usize,
        on_quad: &mut impl FnMut(GeneralizedQuad<'_>) -> Result<(), E>,
    ) -> Result<(), E> {
        //[10] 	subject 	::= 	iri | BlankNode | collection
        match self.read.current() {
            Some(b'[') => {
                let bn = self.parse_generalized_blank_node_property_list(on_quad)?;
                self.triple_alloc.try_push_atom(pos, |b, _| {
                    b.push_str(bn.as_ref());
                    Ok(GeneralizedTerm::from(BlankNode { id: b }))
                })
            }
            Some(b'(') => {
                let collec = self.parse_generalized_collection(on_quad)?;
                self.triple_alloc
                    .try_push_atom(pos, |b, _| allocate_collection(collec, b))?;
                Ok(())
            }
            _ => {
                self.parse_generalized_term(pos, false)?;
                Ok(())
            }
        }
    }

    fn parse_generalized_term(&mut self, pos: usize, graph_name: bool) -> Result<(), TurtleError> {
        let read = &mut self.read;
        let alloc = if graph_name {
            &mut self.graph_name_alloc
        } else {
            &mut self.triple_alloc
        };
        match read.required_current()? {
            b'<' => {
                if read.required_next()? == b'<' {
                    read.consume_many(2)?;
                    skip_whitespace(read)?;

                    alloc.push_triple_start();
                    // at this point, we need to drop read and alloc to be able to use self in the loop
                    for i in 0..3 {
                        self.parse_generalized_term(i, graph_name)?;
                        skip_whitespace(&mut self.read)?;
                    }
                    self.read.check_is_current(b'>')?;
                    self.read.check_is_next(b'>')?;
                    self.read.consume_many(2)?;
                    // reassign alloc, since we had to drop it above (just before 'for' loop)
                    let alloc = if graph_name {
                        &mut self.graph_name_alloc
                    } else {
                        &mut self.triple_alloc
                    };
                    alloc.push_quoted_triple(pos);
                    Ok(())
                } else {
                    let temp_buf = &mut &mut self.temp_buf;
                    let base_iri = self.base_iri.as_ref();
                    alloc.try_push_atom(pos, |b, _| {
                        parse_generalized_iriref(read, b, temp_buf, base_iri)?;
                        Ok(GeneralizedTerm::from(NamedNode { iri: b }))
                    })
                }
            }
            b'_' | b'[' => {
                let bnode_id_generator = &mut self.bnode_id_generator;
                alloc.try_push_atom(pos, |b, _| {
                    parse_blank_node(read, b, bnode_id_generator).map(GeneralizedTerm::from)
                })
            }
            b'"' | b'\'' | b'+' | b'-' | b'.' | b'0'..=b'9' => {
                let temp_buf = &mut &mut self.temp_buf;
                let base_iri = &self.base_iri;
                let prefixes = &self.prefixes;
                alloc.try_push_atom(pos, |b1, b2| {
                    parse_literal(read, b1, b2, temp_buf, base_iri, prefixes)
                        .map(GeneralizedTerm::from)
                })
            }
            b'?' | b'$' => alloc.try_push_atom(pos, |b, _| {
                parse_variable(read, b).map(GeneralizedTerm::from)
            }),
            _ => {
                let base_iri = &self.base_iri;
                let prefixes = &self.prefixes;
                if read.starts_with(b"true") || read.starts_with(b"false") {
                    let temp_buf = &mut &mut self.temp_buf;
                    alloc.try_push_atom(pos, |b1, b2| {
                        parse_literal(read, b1, b2, temp_buf, base_iri, prefixes)
                            .map(GeneralizedTerm::from)
                    })
                } else {
                    alloc.try_push_atom(pos, |b, _| {
                        parse_prefixed_name(read, b, prefixes).map(GeneralizedTerm::from)
                    })
                }
            }
        }
    }
}

pub fn parse_generalized_iriref(
    read: &mut LookAheadByteReader<impl BufRead>,
    buffer: &mut String,
    temp_buf: &mut String,
    base_iri: Option<&Iri<String>>,
) -> Result<(), TurtleError> {
    if let Some(base_iri) = base_iri {
        parse_iriref(read, temp_buf)?;
        let result = base_iri.resolve_into(temp_buf, buffer).map_err(|error| {
            read.parse_error(TurtleErrorKind::InvalidIri {
                iri: temp_buf.to_owned(),
                error,
            })
        });
        temp_buf.clear();
        result
    } else {
        parse_iriref(read, buffer)
    }
}

fn parse_literal<'a>(
    read: &mut LookAheadByteReader<impl BufRead>,
    buffer: &'a mut String,
    annotation_buffer: &'a mut String,
    temp_buf: &mut String,
    base_iri: &Option<Iri<String>>,
    prefixes: &HashMap<String, String>,
) -> Result<Literal<'a>, TurtleError> {
    // [13] 	literal 	::= 	RDFLiteral | NumericLiteral | BooleanLiteral
    match read.required_current()? {
        b'"' | b'\'' => {
            match parse_rdf_literal(
                read,
                buffer,
                annotation_buffer,
                temp_buf,
                base_iri,
                prefixes,
            )? {
                Literal::LanguageTaggedString { .. } => Ok(Literal::LanguageTaggedString {
                    value: buffer,
                    language: annotation_buffer,
                }),
                Literal::Simple { .. } => Ok(Literal::Simple { value: buffer }),
                Literal::Typed { .. } => Ok(Literal::Typed {
                    value: buffer,
                    datatype: NamedNode {
                        iri: annotation_buffer,
                    },
                }),
            }
        }
        b'+' | b'-' | b'.' | b'0'..=b'9' => {
            match parse_numeric_literal(read, buffer)? {
                Literal::Typed { datatype, .. } => {
                    annotation_buffer.push_str(datatype.iri);
                }
                _ => unreachable!(),
            }
            Ok(Literal::Typed {
                value: buffer,
                datatype: NamedNode {
                    iri: annotation_buffer,
                },
            })
        }
        _ => {
            match parse_boolean_literal(read, buffer)? {
                Literal::Typed { datatype, .. } => {
                    annotation_buffer.push_str(datatype.iri);
                }
                _ => unreachable!(),
            }
            Ok(Literal::Typed {
                value: buffer,
                datatype: NamedNode {
                    iri: annotation_buffer,
                },
            })
        }
    }
}

#[allow(clippy::unnecessary_wraps)]
fn allocate_collection(
    collection: Option<BlankNodeId>,
    buffer: &mut String,
) -> Result<GeneralizedTerm<'_>, TurtleError> {
    match collection {
        Some(id) => {
            buffer.push_str(id.as_ref());
            Ok(BlankNode { id: buffer }.into())
        }
        None => Ok(NamedNode { iri: RDF_NIL }.into()),
    }
}

//

#[cfg(test)]
mod test {
    use super::*;
    use std::io::Cursor;

    const OK_TURTLE_ERROR: Result<(), TurtleError> = Ok(());

    #[test]
    fn relative_iri_references() -> Result<(), TurtleError> {
        let got = parse_gtrig(
            r#"
          <../s1> <#p1> </o1>.
          { <../s2> <#p2> </o2> }
          <//g3> { <../s3> <#p3> </o3> }
          GRAPH <//g4> { <../s4> <#p4> </o4> }
        "#,
        )?;

        let expected = parse_gnq(
            r#"
          <../s1> <#p1> </o1>.
          <../s2> <#p2> </o2>.
          <../s3> <#p3> </o3> <//g3>.
          <../s4> <#p4> </o4> <//g4>.
        "#,
        )?;

        assert_eq!(expected, got);
        Ok(())
    }

    #[test]
    fn relative_prefixes() -> Result<(), TurtleError> {
        let got = parse_gtrig(
            r#"
          @prefix s: <../>.
          PREFIX p: <#>
          PREFIX o: </>
          PREFIX g: <//>

          s:s1 p:p1 o:o1.
          { s:s2 p:p2 o:o2 }
          g:g3 { s:s3 p:p3 o:o3 }
          GRAPH g:g4 { s:s4 p:p4 o:o4 }
        "#,
        )?;

        let expected = parse_gnq(
            r#"
          <../s1> <#p1> </o1>.
          <../s2> <#p2> </o2>.
          <../s3> <#p3> </o3> <//g3>.
          <../s4> <#p4> </o4> <//g4>.
        "#,
        )?;

        assert_eq!(expected, got);
        Ok(())
    }

    #[test]
    fn all_variables() -> Result<(), TurtleError> {
        let got = parse_gtrig(
            r#"
          ?s1 ?p1 ?o1.
          { ?s2 ?p2 ?o2 }
          ?g3 { ?s3 ?p3 ?o3 }
          GRAPH ?g4 { ?s4 ?p4 ?o4 }
        "#,
        )?;

        let expected = parse_gnq(
            r#"
          ?s1 ?p1 ?o1.
          ?s2 ?p2 ?o2.
          ?s3 ?p3 ?o3 ?g3.
          ?s4 ?p4 ?o4 ?g4.
        "#,
        )
        .unwrap();

        assert_eq!(expected, got);
        Ok(())
    }

    #[test]
    fn all_literals() -> Result<(), TurtleError> {
        let got = parse_gtrig(
            r#"
          "s1" "p1" "o1".
          { "s2" "p2" "o2" }
          "g3" { "s3" "p3" "o3" }
          GRAPH "g4" { "s4" "p4" "o4" }
        "#,
        )?;

        let expected = parse_gnq(
            r#"
          "s1" "p1" "o1".
          "s2" "p2" "o2".
          "s3" "p3" "o3" "g3".
          "s4" "p4" "o4" "g4".
        "#,
        )
        .unwrap();

        assert_eq!(expected, got);
        Ok(())
    }

    #[test]
    fn all_quoted_triples() -> Result<(), TurtleError> {
        let got = parse_gtrig(
            r#"@prefix : <#>.
          << :ss1 _:ps1 "os1" >> << _:sp1 "pp1" ?op1 >> << "so1" ?po1 :oo1 >>.
          { << ?ss2 :ps2  _:os2 >> << :sp2 "pp2" _:op2 >> << "so2" _:po2 ?oo2 >> }
          << _:sg3 ?pg3  :og3 >> { << ?ss3 :ps3 ?os3 >> << :sp3 ?pp3 _:op3 >> << ?so3 _:po3 "oo3" >> }
          GRAPH << _:sg4 "pg4" :og4 >> { << "ss4" :ps4 _:os4 >> << :sp4 _:pp4 ?op4 >> << _:so4 ?po4 "oo4" >> }
        "#,
        )?;

        let expected = parse_gnq(r#"
          << <#ss1> _:ps1 "os1" >> << _:sp1 "pp1" ?op1 >> << "so1" ?po1 <#oo1> >>.
          << ?ss2 <#ps2> _:os2 >> << <#sp2> "pp2" _:op2 >> << "so2" _:po2 ?oo2 >>.
          << ?ss3 <#ps3> ?os3 >> << <#sp3> ?pp3 _:op3 >> << ?so3 _:po3 "oo3" >> << _:sg3 ?pg3  <#og3> >>.
          << "ss4" <#ps4> _:os4 >> << <#sp4> _:pp4 ?op4 >> << _:so4 ?po4 "oo4" >> << _:sg4 "pg4" <#og4> >>.
        "#).unwrap();

        assert_eq!(expected, got);
        Ok(())
    }

    #[test]
    fn deeply_nested_triple() -> Result<(), TurtleError> {
        let got = parse_gtrig(
            r#"@prefix : <#>.
          << << :a :b :c >> << :d :e :f >> << :g :h :i >> >> {
            << << :j :k :l >> << :m :n :o >> << :p :q :r >> >>
            << << :s :t :u >> << :v :w :x >> << :y :z :A >> >>
            << << :B :C :D >> << :E :F :G >> << :H :I :J >> >>
        }"#,
        )?;
        let expected = parse_gnq(r#"
            << << <#j> <#k> <#l> >> << <#m> <#n> <#o> >> << <#p> <#q> <#r> >> >>    << << <#s> <#t> <#u> >> << <#v> <#w> <#x> >> << <#y> <#z> <#A> >> >>    << << <#B> <#C> <#D> >> << <#E> <#F> <#G> >> << <#H> <#I> <#J> >> >>    << << <#a> <#b> <#c> >> << <#d> <#e> <#f> >> << <#g> <#h> <#i> >> >>.
        "#).unwrap();

        assert_eq!(expected, got);
        Ok(())
    }

    #[test]
    fn composite_predicate() -> Result<(), TurtleError> {
        let gtrig = r#"
          ?s [ ?p ?o1 ] ?o2 .
        "#;

        let mut got: Vec<(OwnedTerm, OwnedTerm, OwnedTerm, Option<OwnedTerm>)> =
            Vec::with_capacity(2);

        GTriGParser::new(
            Cursor::new(gtrig),
            Some(Iri::parse("http://example.org/base/".to_owned()).unwrap()),
        )
        .parse_all(&mut |quad| {
            got.push((
                quad.subject.into(),
                quad.predicate.into(),
                quad.object.into(),
                quad.graph_name.map(OwnedTerm::from),
            ));
            OK_TURTLE_ERROR
        })?;

        assert_eq!(v("p"), got[0].1);
        assert_eq!(v("o1"), got[0].2);
        assert_eq!(v("s"), got[1].0);
        assert_eq!(v("o2"), got[1].2);
        assert_eq!(got[0].0, got[1].1);
        Ok(())
    }

    fn parse_gtrig(
        txt: &str,
    ) -> Result<Vec<(OwnedTerm, OwnedTerm, OwnedTerm, Option<OwnedTerm>)>, TurtleError> {
        let mut got = Vec::new();
        GTriGParser::new(Cursor::new(txt), None).parse_all(&mut |quad| {
            got.push((
                quad.subject.into(),
                quad.predicate.into(),
                quad.object.into(),
                quad.graph_name.map(OwnedTerm::from),
            ));
            OK_TURTLE_ERROR
        })?;
        Ok(got)
    }

    fn parse_gnq(
        txt: &str,
    ) -> Result<Vec<(OwnedTerm, OwnedTerm, OwnedTerm, Option<OwnedTerm>)>, TurtleError> {
        let mut got = Vec::new();
        crate::GeneralizedNQuadsParser::new(Cursor::new(txt)).parse_all(&mut |quad| {
            got.push((
                quad.subject.into(),
                quad.predicate.into(),
                quad.object.into(),
                quad.graph_name.map(OwnedTerm::from),
            ));
            OK_TURTLE_ERROR
        })?;
        Ok(got)
    }

    fn v(value: &str) -> OwnedTerm {
        OwnedTerm::Variable(value.to_string())
    }

    impl<'a> From<GeneralizedTerm<'a>> for OwnedTerm {
        fn from(other: GeneralizedTerm<'a>) -> OwnedTerm {
            match other {
                GeneralizedTerm::NamedNode(n) => OwnedTerm::NamedNode(n.iri.to_string()),
                GeneralizedTerm::BlankNode(n) => OwnedTerm::BlankNode(n.id.to_string()),
                GeneralizedTerm::Literal(Literal::Simple { value }) => {
                    OwnedTerm::LiteralSimple(value.to_string())
                }
                GeneralizedTerm::Literal(Literal::LanguageTaggedString { value, language }) => {
                    OwnedTerm::LiteralLanguage(value.to_string(), language.to_string())
                }
                GeneralizedTerm::Literal(Literal::Typed { value, datatype }) => {
                    OwnedTerm::LiteralDatatype(value.to_string(), datatype.iri.to_string())
                }
                GeneralizedTerm::Variable(n) => OwnedTerm::Variable(n.name.to_string()),
                GeneralizedTerm::Triple(t) => {
                    OwnedTerm::Triple(Box::new([t[0].into(), t[1].into(), t[2].into()]))
                }
            }
        }
    }

    #[derive(Clone, Debug, PartialEq)]
    enum OwnedTerm {
        NamedNode(String),
        BlankNode(String),
        LiteralSimple(String),
        LiteralLanguage(String, String),
        LiteralDatatype(String, String),
        Variable(String),
        Triple(Box<[OwnedTerm; 3]>),
    }

    impl<'a> From<&'a OwnedTerm> for GeneralizedTerm<'a> {
        fn from(other: &'a OwnedTerm) -> GeneralizedTerm<'a> {
            match other {
                OwnedTerm::NamedNode(name) => GeneralizedTerm::NamedNode(NamedNode { iri: &name }),
                OwnedTerm::BlankNode(ident) => GeneralizedTerm::BlankNode(BlankNode { id: &ident }),
                OwnedTerm::LiteralSimple(value) => {
                    GeneralizedTerm::Literal(Literal::Simple { value: &value })
                }
                OwnedTerm::LiteralLanguage(value, tag) => {
                    GeneralizedTerm::Literal(Literal::LanguageTaggedString {
                        value: &value,
                        language: &tag,
                    })
                }
                OwnedTerm::LiteralDatatype(value, datatype) => {
                    GeneralizedTerm::Literal(Literal::Typed {
                        value: &value,
                        datatype: NamedNode { iri: &datatype },
                    })
                }
                OwnedTerm::Variable(name) => GeneralizedTerm::Variable(Variable { name: &name }),
                OwnedTerm::Triple(_) => {
                    unimplemented!()
                }
            }
        }
    }
}
