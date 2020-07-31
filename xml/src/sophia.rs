//! [Sophia] adapters for the [RDF/XML] parser.
//!
//! This module is available if feature `sophia` is enabled.
//!
//! Example: count the number of of people using the `Sophia` API:
//! ```
//! use rio_api::model::NamedNode;
//! use rio_xml::RdfXmlParser;
//! use sophia_api::parser::TripleParser;
//! use sophia_api::triple::{Triple, stream::TripleSource};
//! use sophia_api::term::term_eq;
//! use sophia_api::ns::rdf;
//!
//! let file = b"<?xml version=\"1.0\"?>
//! <rdf:RDF xmlns:rdf=\"http://www.w3.org/1999/02/22-rdf-syntax-ns#\" xmlns:schema=\"http://schema.org/\">
//!  <rdf:Description rdf:about=\"http://example.com/foo\">
//!    <rdf:type rdf:resource=\"http://schema.org/Person\" />
//!    <schema:name>Foo</schema:name>
//!  </rdf:Description>
//!  <schema:Person rdf:about=\"http://example.com/bar\" schema:name=\"Bar\" />
//! </rdf:RDF>";
//!
//! let schema_person = NamedNode { iri: "http://schema.org/Person" };
//! let mut count = 0;
//! RdfXmlParser::new(file.as_ref(), "x-no-base:///")
//!     .unwrap()
//!     .filter_triples(|t| term_eq(t.p(), &rdf::type_) && term_eq(t.o(), &schema_person))
//!     .for_each_triple(|_| { count += 1; })
//!     .unwrap();
//! assert_eq!(2, count)
//! ```
//!
//! [Sophia]: https://crates.io/crates/sophia
//! [RDF/XML]: https://www.w3.org/TR/rdf-syntax-grammar/

use crate::RdfXmlParser;

/// Copied from rio_turtle::sophia
macro_rules! impl_triple_source {
    ($parser:ident) => {
        mod as_sophia_triple_source {
            use super::*;
            use crate::sophia::RioStreamError;
            use rio_api::model::Term;
            use rio_api::parser::TriplesParser;
            use sophia_api::triple::stream::*;
            use sophia_api::triple::streaming_mode::*;
            use std::error::Error;
            use std::io::BufRead;

            impl<B: BufRead> TripleSource for $parser<B> {
                type Error = <$parser<B> as TriplesParser>::Error;
                type Triple = ScopedRioSourceTriple;
                fn try_for_some_triple<F, EF>(
                    &mut self,
                    f: &mut F,
                ) -> StreamResult<bool, Self::Error, EF>
                where
                    F: FnMut(StreamedTriple<'_, Self::Triple>) -> Result<(), EF>,
                    EF: Error,
                {
                    if self.is_end() {
                        return Ok(false);
                    }
                    self.parse_step(&mut |t| -> Result<(), RioStreamError<Self::Error, EF>> {
                        f(StreamedTriple::scoped([
                            t.subject.into(),
                            t.predicate.into(),
                            t.object,
                        ]))
                        .map_err(|e| SinkError(e).into())
                    })
                    .map_err(|e| e.into())
                    .and(Ok(true))
                }
            }

            /// Convenient type alias.
            type RioSourceTriple<'a> = [Term<'a>; 3];
            sophia_api::make_scoped_triple_streaming_mode!(ScopedRioSourceTriple, RioSourceTriple);
        }
    };
}

impl_triple_source!(RdfXmlParser);

// Also copied from rio_turtle::sophia
use sophia_api::quad::stream::*;
use std::error::Error;

// A wrapper around Sophia's `StreamError`
// fullfilling Rio's expectation that the error type of `triple_handler`/`quad_handler`
// implement From<TurtleError> (or whatever Rio-specific error returned by the parser).
//
struct RioStreamError<E1, E2>(StreamError<E1, E2>)
where
    E1: Error + 'static,
    E2: Error + 'static;

impl<E1, E2> From<E1> for RioStreamError<E1, E2>
where
    E1: Error + 'static,
    E2: Error + 'static,
{
    #[inline]
    fn from(other: E1) -> Self {
        RioStreamError(SourceError(other))
    }
}

impl<E1, E2> From<StreamError<E1, E2>> for RioStreamError<E1, E2>
where
    E1: Error + 'static,
    E2: Error + 'static,
{
    #[inline]
    fn from(other: StreamError<E1, E2>) -> Self {
        RioStreamError(other)
    }
}

impl<E1, E2> From<RioStreamError<E1, E2>> for StreamError<E1, E2>
where
    E1: Error + 'static,
    E2: Error + 'static,
{
    #[inline]
    fn from(other: RioStreamError<E1, E2>) -> Self {
        other.0
    }
}

// ---------------------------------------------------------------------------------
//                                      tests
// ---------------------------------------------------------------------------------

#[cfg(test)]
mod test {
    use super::*;
    use rio_api::model::{Literal, NamedNode};
    use sophia_api::graph::Graph;
    use sophia_api::ns::rdf;
    use sophia_api::term::matcher::ANY;
    use sophia_api::term::test::TestTerm;
    use sophia_api::triple::stream::TripleSource;

    #[test]
    fn test_simple_xml_string() -> Result<(), Box<dyn Error>> {
        let xml = r#"<?xml version="1.0" encoding="utf-8"?>
        <rdf:RDF xmlns:rdf="http://www.w3.org/1999/02/22-rdf-syntax-ns#"
                 xmlns="http://example.org/ns/">
          <rdf:Description rdf:about="http://localhost/ex#me">
            <knows>
              <Person>
                <name>Alice</name>
              </Person>
            </knows>
          </rdf:Description>
        </rdf:RDF>
        "#;

        let p = RdfXmlParser::new(xml.as_ref(), "http://localhost/ex")?;

        let g: Vec<[TestTerm<String>; 3]> = p.collect_triples()?;
        assert_eq!(g.len(), 3);
        assert!(g
            .triples_matching(
                &NamedNode {
                    iri: "http://localhost/ex#me"
                },
                &NamedNode {
                    iri: "http://example.org/ns/knows"
                },
                &ANY,
            )
            .next()
            .is_some());
        assert!(g
            .triples_matching(
                &ANY,
                &rdf::type_,
                &NamedNode {
                    iri: "http://example.org/ns/Person"
                },
            )
            .next()
            .is_some());
        assert!(g
            .triples_matching(
                &ANY,
                &NamedNode {
                    iri: "http://example.org/ns/name"
                },
                &Literal::Simple { value: "Alice" },
            )
            .next()
            .is_some());
        Ok(())
    }
}
