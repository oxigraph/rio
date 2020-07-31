//! Sophia adapter for Turtle.

use crate::{TurtleError, TurtleParser};
use rio_api::parser::ParseError;
use sophia_api::parser::{Location, WithLocation};

impl WithLocation for TurtleError {
    fn location(&self) -> Location {
        match self.textual_position() {
            None => Location::Unknown,
            Some(pos) => Location::from_lico(
                (pos.line_number() + 1) as usize,
                (pos.byte_number() + 1) as usize,
            ),
        }
    }
}

impl_triple_source!(TurtleParser);

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
    fn test_simple_turtle_string() -> Result<(), Box<dyn std::error::Error>> {
        let turtle = br#"
            @prefix : <http://example.org/ns/> .

            <#me> :knows [ a :Person ; :name "Alice" ].
        "#;

        let p = TurtleParser::new(
            turtle.as_ref(),
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
