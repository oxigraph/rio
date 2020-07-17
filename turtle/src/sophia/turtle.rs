//! [Sophia] adapter for [Turtle].
//!
//! Example: count the number of of people using the `Sophia` API:
//! ```
//! use rio_api::model::NamedNode;
//! use rio_turtle::sophia::TurtleParser;
//! use sophia_api::parser::TripleParser;
//! use sophia_api::triple::{Triple, stream::TripleSource};
//! use sophia_api::term::term_eq;
//! use sophia_api::ns::rdf;
//!
//! let file = b"@prefix schema: <http://schema.org/> .
//! <http://example.com/foo> a schema:Person ;
//!     schema:name  \"Foo\" .
//! <http://example.com/bar> a schema:Person ;
//!     schema:name  \"Bar\" .
//! ";
//!
//! let schema_person = NamedNode { iri: "http://schema.org/Person" };
//! let mut count = 0;
//! TurtleParser::default()
//!     .parse(file.as_ref())
//!     .filter_triples(|t| term_eq(t.p(), &rdf::type_) && term_eq(t.o(), &schema_person))
//!     .for_each_triple(|_| { count += 1; })
//!     .unwrap();
//! assert_eq!(2, count)
//! ```
//!
//! [Sophia]: https://crates.io/crates/sophia
//! [Turtle]: https://www.w3.org/TR/turtle/

use crate::{TurtleError, TurtleParser as RioTurtleParser};
use rio_api::parser::ParseError;
use rio_api::sophia::StrictRioSource;
use sophia_api::parser::{Location, TripleParser, WithLocation};
use std::io::BufRead;

/// An implementation of [`sophia_api::parser::TripleParser`]
/// around [the turtle parser].
///
/// [`sophia_api::parser::TripleParser`]: https://docs.rs/sophia_api/latest/sophia_api/parser/trait.TripleParser.html
/// [the turtle parser]: ../../struct.TurtleParser.html
#[derive(Clone, Debug, Default)]
pub struct TurtleParser {
    pub base: Option<String>,
}

impl<B: BufRead> TripleParser<B> for TurtleParser {
    type Source = StrictRioSource<RioTurtleParser<B>, TurtleError>;
    fn parse(&self, data: B) -> Self::Source {
        let base: &str = match &self.base {
            Some(base) => &base,
            None => "x-no-base:///",
        };
        StrictRioSource::from(RioTurtleParser::new(data, base))
    }
}

impl WithLocation for TurtleError {
    fn location(&self) -> Location {
        match self.textual_position() {
            None => Location::Unknown,
            Some(pos) => Location::from_lico(pos.line_number() + 1, pos.byte_number() + 1),
        }
    }
}

sophia_api::def_mod_functions_for_bufread_parser!(TurtleParser, TripleParser);

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
    fn test_simple_turtle_string() -> Result<(), Box<dyn std::error::Error>> {
        let turtle = r#"
            @prefix : <http://example.org/ns/> .

            <#me> :knows [ a :Person ; :name "Alice" ].
        "#;

        let p = TurtleParser {
            base: Some("http://localhost/ex".to_string()),
        };

        let g: Vec<[TestTerm<String>; 3]> = p.parse_str(&turtle).collect_triples()?;
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
