//! Implementation of a generalized RDF / RDF-star version of the N-Quads syntax

use crate::error::*;
use crate::gtriple_allocator::GeneralizedTripleAllocator;
use crate::ntriples::{parse_literal, skip_until_eol, skip_whitespace};
use crate::shared::*;
use crate::utils::*;
use oxiri::IriRef;
use rio_api::model::*;
use rio_api::parser::*;
use std::io::BufRead;

/// A [N-Quads](https://www.w3.org/TR/n-quads/) streaming parser parsing generalized quads.
///
/// It implements the `GeneralizedQuadsParser` trait.
/// Using it requires to enable the `generalized` feature.
pub struct GeneralizedNQuadsParser<R: BufRead> {
    read: LookAheadByteReader<R>,
    triple_alloc: GeneralizedTripleAllocator,
    graph_name_alloc: GeneralizedTripleAllocator,
}

impl<R: BufRead> GeneralizedNQuadsParser<R> {
    pub fn new(reader: R) -> Self {
        Self {
            read: LookAheadByteReader::new(reader),
            triple_alloc: GeneralizedTripleAllocator::new(),
            graph_name_alloc: GeneralizedTripleAllocator::new(),
        }
    }
}

impl<R: BufRead> GeneralizedQuadsParser for GeneralizedNQuadsParser<R> {
    type Error = TurtleError;

    fn parse_step<E: From<TurtleError>>(
        &mut self,
        on_quad: &mut impl FnMut(GeneralizedQuad<'_>) -> Result<(), E>,
    ) -> Result<(), E> {
        match self.parse_quad_line() {
            Ok(Some(named_graph)) => {
                match on_quad(self.triple_alloc.top_quad(
                    named_graph.then(|| self.graph_name_alloc.current_subject().unwrap()),
                )) {
                    Ok(()) => {
                        if named_graph {
                            // named graph is allocated as the subject of an incomplete triple
                            self.graph_name_alloc.pop_term(0);
                            self.graph_name_alloc.pop_top_empty_triple();
                        }
                        self.triple_alloc.pop_top_triple();
                        debug_assert_eq!(self.triple_alloc.complete_len(), 0);
                        debug_assert_eq!(self.triple_alloc.incomplete_len(), 0);
                        debug_assert_eq!(self.graph_name_alloc.complete_len(), 0);
                        debug_assert_eq!(self.graph_name_alloc.incomplete_len(), 0);
                        Ok(())
                    }
                    Err(err) => {
                        self.triple_alloc.clear();
                        Err(err)
                    }
                }
            }
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

impl<R: BufRead> GeneralizedNQuadsParser<R> {
    fn parse_quad_line(&mut self) -> Result<Option<bool>, TurtleError> {
        let read = &mut self.read;
        let triple_alloc = &mut &mut self.triple_alloc;

        skip_whitespace(read)?;

        if matches!(
            read.current(),
            None | Some(b'#') | Some(b'\r') | Some(b'\n')
        ) {
            skip_until_eol(read)?;
            return Ok(None);
        }

        parse_triple(read, triple_alloc)?;
        let named_graph = match read.current() {
            Some(b'.') => false,
            _ => {
                self.graph_name_alloc.push_triple_start();
                parse_term(0, read, &mut self.graph_name_alloc)?;
                skip_whitespace(read)?;
                true
            }
        };

        read.check_is_current(b'.')?;
        read.consume()?;
        skip_whitespace(read)?;

        match read.current() {
            None | Some(b'#') | Some(b'\r') | Some(b'\n') => skip_until_eol(read)?,
            _ => read.unexpected_char_error()?,
        }

        Ok(Some(named_graph))
    }
}

fn parse_triple(
    read: &mut LookAheadByteReader<impl BufRead>,
    triple_alloc: &mut GeneralizedTripleAllocator,
) -> Result<(), TurtleError> {
    triple_alloc.push_triple_start();

    for i in 0..3 {
        parse_term(i, read, triple_alloc)?;
        skip_whitespace(read)?;
    }
    Ok(())
}

fn parse_term(
    pos: usize,
    read: &mut LookAheadByteReader<impl BufRead>,
    triple_alloc: &mut GeneralizedTripleAllocator,
) -> Result<(), TurtleError> {
    match read.required_current()? {
        b'<' => match read.required_next()? {
            b'<' => {
                parse_quoted_triple(read, triple_alloc)?;
                triple_alloc.push_quoted_triple(pos);
                Ok(())
            }
            _ => triple_alloc.try_push_atom(pos, |b, _| {
                parse_iriref(read, b)?;
                IriRef::parse(b.as_str()).map_err(|error| {
                    read.parse_error(TurtleErrorKind::InvalidIri {
                        iri: b.to_owned(),
                        error,
                    })
                })?;
                Ok(NamedNode { iri: b }.into())
            }),
        },
        b'_' => triple_alloc.try_push_atom(pos, |b, _| {
            parse_blank_node_label(read, b).map(GeneralizedTerm::from)
        }),
        b'"' => triple_alloc.try_push_atom(pos, |b1, b2| {
            parse_literal(read, b1, b2).map(GeneralizedTerm::from)
        }),
        b'?' | b'$' => triple_alloc.try_push_atom(pos, |b, _| {
            parse_variable(read, b).map(GeneralizedTerm::from)
        }),
        _ => read.unexpected_char_error(),
    }
}

fn parse_quoted_triple(
    read: &mut LookAheadByteReader<impl BufRead>,
    triple_alloc: &mut GeneralizedTripleAllocator,
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

pub(crate) fn parse_variable<'a>(
    read: &mut LookAheadByteReader<impl BufRead>,
    buffer: &'a mut String,
) -> Result<Variable<'a>, TurtleError> {
    read.consume()?;
    let c = read.required_current()?;
    if c <= MAX_ASCII && (is_possible_pn_chars_u_ascii(c) || (b'0'..=b'9').contains(&c)) {
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
        if let Some(c) = read.current() {
            if c <= MAX_ASCII
                && (is_possible_pn_chars_u_ascii(c) || (b'0'..=b'9').contains(&c) || c == 0xb7)
            {
                buffer.push(char::from(c))
            } else {
                let c = read_utf8_char(read)?;
                if is_possible_pn_chars_u_unicode(c) {
                    buffer.push(c);
                } else {
                    return Ok(Variable { name: buffer });
                }
            }
        } else {
            return Ok(Variable { name: buffer });
        }
    }
}

#[cfg(test)]
mod test {
    use super::*;

    #[test]
    fn nquads_relative_irirefs() -> Result<(), Box<dyn std::error::Error>> {
        // adding this test because there is currenly no testsuite specific to N-Quads star
        let file = r#"<#s> <../p> </o> <//g>."#;
        let mut count = 0;
        GeneralizedNQuadsParser::new(file.as_ref()).parse_all(&mut |q| -> Result<
            (),
            TurtleError,
        > {
            assert!(matches!(
                q.subject,
                GeneralizedTerm::NamedNode(NamedNode { iri: "#s" }),
            ));
            assert!(matches!(
                q.predicate,
                GeneralizedTerm::NamedNode(NamedNode { iri: "../p" }),
            ));
            assert!(matches!(
                q.object,
                GeneralizedTerm::NamedNode(NamedNode { iri: "/o" }),
            ));
            assert!(matches!(
                q.graph_name,
                Some(GeneralizedTerm::NamedNode(NamedNode { iri: "//g" })),
            ));
            count += 1;
            Ok(())
        })?;
        assert_eq!(1, count);
        Ok(())
    }

    #[test]
    fn nquads_star_valid_quad() -> Result<(), Box<dyn std::error::Error>> {
        // adding this test because there is currenly no testsuite specific to N-Quads star
        let file =
            br#"<< "a" _:b <tag:c> >> << "d" ?e <./f> >> << "g" $h <../i> >> << "j" _:k </l> >>."#;
        let mut count = 0;
        GeneralizedNQuadsParser::new(file.as_ref()).parse_all(&mut |q| -> Result<
            (),
            TurtleError,
        > {
            assert!(matches!(
                q.subject,
                GeneralizedTerm::Triple([
                    GeneralizedTerm::Literal(Literal::Simple { value: "a" }),
                    GeneralizedTerm::BlankNode(BlankNode { id: "b" }),
                    GeneralizedTerm::NamedNode(NamedNode { iri: "tag:c" }),
                ])
            ));
            assert!(matches!(
                q.predicate,
                GeneralizedTerm::Triple([
                    GeneralizedTerm::Literal(Literal::Simple { value: "d" }),
                    GeneralizedTerm::Variable(Variable { name: "e" }),
                    GeneralizedTerm::NamedNode(NamedNode { iri: "./f" }),
                ])
            ));
            assert!(matches!(
                q.object,
                GeneralizedTerm::Triple([
                    GeneralizedTerm::Literal(Literal::Simple { value: "g" }),
                    GeneralizedTerm::Variable(Variable { name: "h" }),
                    GeneralizedTerm::NamedNode(NamedNode { iri: "../i" }),
                ])
            ));
            assert!(matches!(
                q.graph_name,
                Some(GeneralizedTerm::Triple([
                    GeneralizedTerm::Literal(Literal::Simple { value: "j" }),
                    GeneralizedTerm::BlankNode(BlankNode { id: "k" }),
                    GeneralizedTerm::NamedNode(NamedNode { iri: "/l" }),
                ]))
            ));
            count += 1;
            Ok(())
        })?;
        assert_eq!(1, count);
        Ok(())
    }

    #[test]
    fn nquads_star_invalid_graph_name() {
        // adding this test because there is currenly no testsuite specific to N-Quads star
        let file = b"<tag:s> <tag:p> << <tag:a> <tag:b> <tag:c> .";
        let mut count = 0;
        let res = GeneralizedNQuadsParser::new(file.as_ref()).parse_all(&mut |_| -> Result<
            (),
            TurtleError,
        > {
            count += 1;
            Ok(())
        });
        assert!(res.is_err());
    }
}
