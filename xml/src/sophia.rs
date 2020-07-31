use crate::{RdfXmlError, RdfXmlParser};
use rio_api::model::Term;
use rio_api::parser::TriplesParser;
use sophia_api::make_scoped_triple_streaming_mode;
use sophia_api::quad::stream::*;
use sophia_api::triple::stream::TripleSource;
use sophia_api::triple::streaming_mode::StreamedTriple;
use std::error::Error;
use std::io::BufRead;

type RioSourceTriple<'a> = [Term<'a>; 3];
make_scoped_triple_streaming_mode!(ScopedRioSourceTriple, RioSourceTriple);

impl<B: BufRead> TripleSource for RdfXmlParser<B> {
    type Error = RdfXmlError;
    type Triple = ScopedRioSourceTriple;
    fn try_for_some_triple<F, EF>(&mut self, f: &mut F) -> StreamResult<bool, Self::Error, EF>
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

// A wrapper around Sophia's `StreamError`
// fulfilling Rio's expectation that the error type of `triple_handler`/`quad_handler`
// implement From<TurtleError> (or whatever Rio-specific error returned by the parser).
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

#[cfg(test)]
mod test {
    use super::*;
    use oxiri::Iri;
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

        let p = RdfXmlParser::new(
            xml.as_ref(),
            Some(Iri::parse("http://localhost/ex".to_owned())?),
        );

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
