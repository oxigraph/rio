//! [Sophia] adapter for [generalized] [TriG]
//!
//! Using it requires to enable the `generalized` feature.
//!
//! Example: count the number of of people using the `Sophia` API:
//! ```
//! use rio_api::model::NamedNode;
//! use rio_turtle::sophia::GTriGParser;
//! use sophia_api::parser::QuadParser;
//! use sophia_api::quad::{Quad, stream::QuadSource};
//! use sophia_api::term::term_eq;
//! use sophia_api::ns::rdf;
//!
//! let file = b"@prefix schema: <http://schema.org/> .
//! <http://example/> {
//!     <http://example.com/foo> a schema:Person ;
//!         schema:name  ?name .
//!     <http://example.com/bar> a schema:Person ;
//!         schema:name  ?name .
//! }";
//!
//! let schema_person = NamedNode { iri: "http://schema.org/Person" };
//! let mut count = 0;
//! GTriGParser::default()
//!     .parse(file.as_ref())
//!     .filter_quads(|q| term_eq(q.p(), &rdf::type_) && term_eq(q.o(), &schema_person))
//!     .for_each_quad(|_| { count += 1; })
//!     .unwrap();
//! assert_eq!(2, count)
//! ```
//!
//! [Sophia]: https://crates.io/crates/sophia
//! [generalized]: https://docs.rs/sophia/latest/sophia/#generalized-vs-strict-rdf-model
//! [TriG]: https://www.w3.org/TR/trig/

use crate::{GTriGParser as RioGTriGParser, TurtleError};
use rio_api::sophia::GeneralizedRioSource;
use sophia_api::parser::QuadParser;
use std::io::BufRead;

/// An implementation of [`sophia_api::parser::QuadParser`]
/// around [the generalized trig parser].
///
/// [`sophia_api::parser::QuadParser`]: https://docs.rs/sophia_api/latest/sophia_api/parser/trait.QuadParser.html
/// [the generalized trig parser]: ../../struct.GTriGParser.html
#[derive(Clone, Debug, Default)]
pub struct GTriGParser {
    pub base: Option<String>,
}

impl<B: BufRead> QuadParser<B> for GTriGParser {
    type Source = GeneralizedRioSource<RioGTriGParser<B>, TurtleError>;
    fn parse(&self, data: B) -> Self::Source {
        let base: &str = match &self.base {
            Some(base) => &base,
            None => "",
        };
        GeneralizedRioSource::from(RioGTriGParser::new(data, base))
    }
}

sophia_api::def_mod_functions_for_bufread_parser!(GTriGParser, QuadParser);

// ---------------------------------------------------------------------------------
//                                      tests
// ---------------------------------------------------------------------------------

#[cfg(test)]
mod test {
    use super::*;
    use rio_api::model::{NamedNode, Variable};
    use sophia_api::dataset::Dataset;
    use sophia_api::ns::rdf;
    use sophia_api::quad::stream::QuadSource;
    use sophia_api::term::matcher::ANY;
    use sophia_api::term::test::TestTerm;

    #[test]
    fn test_simple_gtrig_string() -> Result<(), Box<dyn std::error::Error>> {
        let gtrig = r#"
            @prefix : <http://example.org/ns/> .

            <#g1> {
                <#me> :knows _:alice.
            }
            <#g2> {
                _:alice a :Person ; :name ?name.
            }
        "#;

        let p = GTriGParser {
            base: Some("http://localhost/ex".into()),
        };

        let d: Vec<([TestTerm<String>; 3], Option<TestTerm<String>>)> =
            p.parse_str(&gtrig).collect_quads()?;
        assert_eq!(d.len(), 3);
        assert!(d
            .quads_matching(
                &NamedNode {
                    iri: "http://localhost/ex#me"
                },
                &NamedNode {
                    iri: "http://example.org/ns/knows"
                },
                &ANY,
                &Some(&NamedNode {
                    iri: "http://localhost/ex#g1"
                }),
            )
            .next()
            .is_some());
        assert!(d
            .quads_matching(
                &ANY,
                &rdf::type_,
                &NamedNode {
                    iri: "http://example.org/ns/Person"
                },
                &Some(&NamedNode {
                    iri: "http://localhost/ex#g2"
                }),
            )
            .next()
            .is_some());
        assert!(d
            .quads_matching(
                &ANY,
                &NamedNode {
                    iri: "http://example.org/ns/name"
                },
                &Variable { name: "name" },
                &Some(&NamedNode {
                    iri: "http://localhost/ex#g2"
                }),
            )
            .next()
            .is_some());
        Ok(())
    }
}
