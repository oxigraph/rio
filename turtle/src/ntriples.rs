//! Implementation of N-Triples and N-Quads RDF syntax

use crate::error::*;
use crate::shared::*;
use crate::triple_allocator::TripleAllocator;
use crate::utils::*;
use rio_api::model::*;
use rio_api::parser::*;
use std::io::BufRead;

/// A [N-Triples](https://www.w3.org/TR/n-triples/) and [N-Triples-star](https://w3c.github.io/rdf-star/cg-spec/#n-triples-star) streaming parser.
///
/// It implements the [`TriplesParser`] trait.
///
/// Its memory consumption is linear in the size of the longest line of the file.
/// It does not do any allocation during parsing except buffer resizing
/// if a line significantly longer than the previous is encountered.
///
///
/// Count the number of people using the [`TriplesParser`] API:
/// ```
/// use rio_turtle::{NTriplesParser, TurtleError};
/// use rio_api::parser::TriplesParser;
/// use rio_api::model::NamedNode;
///
/// let file = b"<http://example.com/foo> <http://www.w3.org/1999/02/22-rdf-syntax-ns#type> <http://schema.org/Person> .
/// <http://example.com/foo> <http://schema.org/name> \"Foo\" .
/// <http://example.com/bar> <http://www.w3.org/1999/02/22-rdf-syntax-ns#type> <http://schema.org/Person> .
/// <http://example.com/bar> <http://schema.org/name> \"Bar\" .";
///
/// let rdf_type = NamedNode { iri: "http://www.w3.org/1999/02/22-rdf-syntax-ns#type" };
/// let schema_person = NamedNode { iri: "http://schema.org/Person" };
/// let mut count = 0;
/// NTriplesParser::new(file.as_ref()).parse_all(&mut |t| {
///     if t.predicate == rdf_type && t.object == schema_person.into() {
///         count += 1;
///     }
///     Ok(()) as Result<(), TurtleError>
/// })?;
/// assert_eq!(2, count);
/// # Result::<_,rio_turtle::TurtleError>::Ok(())
/// ```
pub struct NTriplesParser<R: BufRead> {
    read: LookAheadByteReader<R>,
    triple_alloc: TripleAllocator,
}

impl<R: BufRead> NTriplesParser<R> {
    pub fn new(reader: R) -> Self {
        Self {
            read: LookAheadByteReader::new(reader),
            triple_alloc: TripleAllocator::new(),
        }
    }
}

impl<R: BufRead> TriplesParser for NTriplesParser<R> {
    type Error = TurtleError;

    fn parse_step<E: From<TurtleError>>(
        &mut self,
        on_triple: &mut impl FnMut(Triple<'_>) -> Result<(), E>,
    ) -> Result<(), E> {
        match parse_triple_line(&mut self.read, &mut self.triple_alloc) {
            Ok(true) => match on_triple(*self.triple_alloc.top()) {
                Ok(()) => {
                    self.triple_alloc.pop_top_triple();
                    Ok(())
                }
                Err(err) => {
                    self.triple_alloc.clear();
                    Err(err)
                }
            },
            Ok(false) => Ok(()),
            Err(error) => {
                self.read.consume_line_end()?;
                self.triple_alloc.clear();
                Err(E::from(error))
            }
        }
    }

    fn is_end(&self) -> bool {
        self.read.current().is_none()
    }
}

/// A [N-Quads](https://www.w3.org/TR/n-quads/) and [N-Quads-star](https://w3c.github.io/rdf-star/cg-spec/#n-quads-star) streaming parser.
///
/// It implements the `QuadsParser` trait.
///
/// Its memory consumption is linear in the size of the longest line of the file.
/// It does not do any allocation during parsing except buffer resizing
/// if a line significantly longer than the previous is encountered.
///
///
/// Count the number of people using the `QuadsParser` API:
/// ```
/// use rio_turtle::{NQuadsParser, TurtleError};
/// use rio_api::parser::QuadsParser;
/// use rio_api::model::NamedNode;
///
/// let file = b"<http://example.com/foo> <http://www.w3.org/1999/02/22-rdf-syntax-ns#type> <http://schema.org/Person> <http://example.com/> .
/// <http://example.com/foo> <http://schema.org/name> \"Foo\" <http://example.com/> .
/// <http://example.com/bar> <http://www.w3.org/1999/02/22-rdf-syntax-ns#type> <http://schema.org/Person> .
/// <http://example.com/bar> <http://schema.org/name> \"Bar\" .";
///
/// let rdf_type = NamedNode { iri: "http://www.w3.org/1999/02/22-rdf-syntax-ns#type" };
/// let schema_person = NamedNode { iri: "http://schema.org/Person" };
/// let mut count = 0;
/// NQuadsParser::new(file.as_ref()).parse_all(&mut |t| {
///     if t.predicate == rdf_type && t.object == schema_person.into() {
///         count += 1;
///     }
///     Ok(()) as Result<(), TurtleError>
/// })?;
/// assert_eq!(2, count);
/// # Result::<_,rio_turtle::TurtleError>::Ok(())
/// ```
pub struct NQuadsParser<R: BufRead> {
    read: LookAheadByteReader<R>,
    triple_alloc: TripleAllocator,
    graph_name_buf: String,
}

impl<R: BufRead> NQuadsParser<R> {
    pub fn new(reader: R) -> Self {
        Self {
            read: LookAheadByteReader::new(reader),
            triple_alloc: TripleAllocator::new(),
            graph_name_buf: String::default(),
        }
    }
}

impl<R: BufRead> QuadsParser for NQuadsParser<R> {
    type Error = TurtleError;

    fn parse_step<E: From<TurtleError>>(
        &mut self,
        on_quad: &mut impl FnMut(Quad<'_>) -> Result<(), E>,
    ) -> Result<(), E> {
        match parse_quad_line(
            &mut self.read,
            &mut self.triple_alloc,
            &mut self.graph_name_buf,
        ) {
            Ok(Some(opt_graph_name)) => match on_quad(self.triple_alloc.top_quad(opt_graph_name)) {
                Ok(()) => {
                    self.triple_alloc.pop_top_triple();
                    Ok(())
                }
                Err(err) => {
                    self.triple_alloc.clear();
                    Err(err)
                }
            },
            Ok(None) => Ok(()),
            Err(error) => {
                self.read.consume_line_end()?;
                self.triple_alloc.clear();
                Err(E::from(error))
            }
        }
    }

    fn is_end(&self) -> bool {
        self.read.current().is_none()
    }
}

fn parse_triple_line(
    read: &mut LookAheadByteReader<impl BufRead>,
    triple_alloc: &mut TripleAllocator,
) -> Result<bool, TurtleError> {
    skip_whitespace(read)?;

    if matches!(
        read.current(),
        None | Some(b'#') | Some(b'\r') | Some(b'\n')
    ) {
        skip_until_eol(read)?;
        return Ok(false);
    }

    parse_triple(read, triple_alloc)?;

    read.check_is_current(b'.')?;
    read.consume()?;
    skip_whitespace(read)?;

    match read.current() {
        None | Some(b'#') | Some(b'\r') | Some(b'\n') => skip_until_eol(read)?,
        _ => read.unexpected_char_error()?,
    }

    Ok(true)
}

fn parse_triple(
    read: &mut LookAheadByteReader<impl BufRead>,
    triple_alloc: &mut TripleAllocator,
) -> Result<(), TurtleError> {
    triple_alloc.push_triple_start();

    parse_subject(read, triple_alloc)?;
    skip_whitespace(read)?;

    triple_alloc.try_push_predicate(|b| parse_iriref(read, b))?;
    skip_whitespace(read)?;

    parse_object(read, triple_alloc)?;
    skip_whitespace(read)?;

    Ok(())
}

fn parse_quad_line<'a>(
    read: &mut LookAheadByteReader<impl BufRead>,
    triple_alloc: &mut TripleAllocator,
    graph_name_buf: &'a mut String,
) -> Result<Option<Option<GraphName<'a>>>, TurtleError> {
    skip_whitespace(read)?;

    if matches!(
        read.current(),
        None | Some(b'#') | Some(b'\r') | Some(b'\n')
    ) {
        skip_until_eol(read)?;
        return Ok(None);
    }

    parse_triple(read, triple_alloc)?;
    let opt_graph_name = match read.current() {
        Some(b'<') | Some(b'_') => {
            graph_name_buf.clear();
            Some(parse_graph_name(read, graph_name_buf)?)
        }
        _ => None,
    };
    skip_whitespace(read)?;

    read.check_is_current(b'.')?;
    read.consume()?;
    skip_whitespace(read)?;

    match read.current() {
        None | Some(b'#') | Some(b'\r') | Some(b'\n') => skip_until_eol(read)?,
        _ => read.unexpected_char_error()?,
    }

    Ok(Some(opt_graph_name))
}

fn parse_subject(
    read: &mut LookAheadByteReader<impl BufRead>,
    triple_alloc: &mut TripleAllocator,
) -> Result<(), TurtleError> {
    match read.required_current()? {
        b'<' => match read.required_next()? {
            b'<' => {
                parse_embedded_triple(read, triple_alloc)?;
                triple_alloc.push_subject_triple();
                Ok(())
            }
            _ => triple_alloc.try_push_subject(|b| parse_iriref(read, b).map(Subject::from)),
        },
        b'_' => {
            triple_alloc.try_push_subject(|b| parse_blank_node_label(read, b).map(Subject::from))
        }
        _ => read.unexpected_char_error(),
    }
}

fn parse_object(
    read: &mut LookAheadByteReader<impl BufRead>,
    triple_alloc: &mut TripleAllocator,
) -> Result<(), TurtleError> {
    match read.required_current()? {
        b'<' => match read.required_next()? {
            b'<' => {
                parse_embedded_triple(read, triple_alloc)?;
                triple_alloc.push_object_triple();
                Ok(())
            }
            _ => triple_alloc.try_push_object(|b, _| parse_iriref(read, b).map(Term::from)),
        },
        b'_' => {
            triple_alloc.try_push_object(|b, _| parse_blank_node_label(read, b).map(Term::from))
        }
        b'"' => triple_alloc.try_push_object(|b1, b2| parse_literal(read, b1, b2).map(Term::from)),
        _ => read.unexpected_char_error(),
    }
}

fn parse_embedded_triple(
    read: &mut LookAheadByteReader<impl BufRead>,
    triple_alloc: &mut TripleAllocator,
) -> Result<(), TurtleError> {
    debug_assert_eq!(read.current(), Some(b'<'));
    debug_assert_eq!(read.next()?, Some(b'<'));
    read.increment_stack_size()?;
    read.consume_many(2)?;

    skip_whitespace(read)?;

    parse_triple(read, triple_alloc)?;

    read.check_is_current(b'>')?;
    read.consume()?;
    read.check_is_current(b'>')?;
    read.consume()?;
    read.decrement_stack_size();
    skip_whitespace(read)
}

fn parse_graph_name<'a>(
    read: &mut LookAheadByteReader<impl BufRead>,
    buffer: &'a mut String,
) -> Result<GraphName<'a>, TurtleError> {
    match read.required_current()? {
        b'<' => Ok(parse_iriref(read, buffer)?.into()),
        b'_' => Ok(parse_blank_node_label(read, buffer)?.into()),
        _ => read.unexpected_char_error(),
    }
}

fn parse_literal<'a>(
    read: &mut LookAheadByteReader<impl BufRead>,
    buffer: &'a mut String,
    annotation_buffer: &'a mut String,
) -> Result<Literal<'a>, TurtleError> {
    parse_string_literal_quote(read, buffer)?;
    skip_whitespace(read)?;

    match read.current() {
        Some(b'@') => {
            parse_langtag(read, annotation_buffer)?;
            Ok(Literal::LanguageTaggedString {
                value: buffer,
                language: annotation_buffer,
            })
        }
        Some(b'^') => {
            read.consume()?;
            read.check_is_current(b'^')?;
            read.consume()?;
            skip_whitespace(read)?;
            Ok(Literal::Typed {
                value: buffer,
                datatype: parse_iriref(read, annotation_buffer)?,
            })
        }
        _ => Ok(Literal::Simple { value: buffer }),
    }
}

fn skip_whitespace(read: &mut LookAheadByteReader<impl BufRead>) -> Result<(), TurtleError> {
    loop {
        match read.current() {
            Some(b' ') | Some(b'\t') => read.consume()?,
            _ => return Ok(()),
        }
    }
}

fn skip_until_eol(read: &mut LookAheadByteReader<impl BufRead>) -> Result<(), TurtleError> {
    loop {
        match read.current() {
            None => return Ok(()),
            Some(b'\n') => {
                read.consume()?;
                return Ok(());
            }
            _ => (),
        }
        read.consume()?;
    }
}

fn parse_iriref<'a>(
    read: &mut LookAheadByteReader<impl BufRead>,
    buffer: &'a mut String,
) -> Result<NamedNode<'a>, TurtleError> {
    parse_iriref_absolute(read, buffer)?;
    Ok(NamedNode { iri: buffer })
}

#[cfg(test)]
mod test {
    #[test]
    fn nquads_star_valid_quad() -> Result<(), Box<dyn std::error::Error>> {
        // adding this test because there is currenly no testsuite specific to N-Quads star
        use crate::{NQuadsParser, TurtleError};
        use rio_api::parser::QuadsParser;
        let file = b"<< <tag:a> <tag:b> <tag:c> >> <tag:d> << <tag:e> <tag:f> <tag:g> >> <tag:h>.";
        let mut count = 0;
        NQuadsParser::new(file.as_ref()).parse_all(&mut |_| -> Result<(), TurtleError> {
            count += 1;
            Ok(())
        })?;
        assert_eq!(1, count);
        Ok(())
    }

    #[test]
    fn nquads_star_invalid_graph_name() {
        // adding this test because there is currenly no testsuite specific to N-Quads star
        use crate::{NQuadsParser, TurtleError};
        use rio_api::parser::QuadsParser;
        let file = b"<tag:s> <tag:p> <tag:o> << <tag:a> <tag:b> <tag:c> >> .";
        let mut count = 0;
        let res = NQuadsParser::new(file.as_ref()).parse_all(&mut |_| -> Result<(), TurtleError> {
            count += 1;
            Ok(())
        });
        assert!(res.is_err());
    }
}
