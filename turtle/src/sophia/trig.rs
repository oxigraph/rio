//! [Sophia] adapter for [TriG]
//!
//! Example: count the number of of people using the `Sophia` API:
//! ```
//! use rio_api::model::NamedNode;
//! use rio_turtle::TriGParser;
//! use sophia_api::parser::QuadParser;
//! use sophia_api::quad::{Quad, stream::QuadSource};
//! use sophia_api::term::term_eq;
//! use sophia_api::ns::rdf;
//!
//! let file = b"@prefix schema: <http://schema.org/> .
//! <my_graph> {
//!     <foo> a schema:Person ;
//!         schema:name  \"Foo\" .
//!     <bar> a schema:Person ;
//!         schema:name  \"Bar\" .
//! }";
//!
//! let schema_person = NamedNode { iri: "http://schema.org/Person" };
//! let mut count = 0;
//! TriGParser::new(file.as_ref(), "http://example.com/")
//!     .unwrap()
//!     .filter_quads(|q| term_eq(q.p(), &rdf::type_) && term_eq(q.o(), &schema_person))
//!     .for_each_quad(|_| { count += 1; })
//!     .unwrap();
//! assert_eq!(2, count)
//! ```
//!
//! [Sophia]: https://crates.io/crates/sophia
//! [TriG]: https://www.w3.org/TR/trig/

use crate::TriGParser;

impl_quad_source!(TriGParser);

// ---------------------------------------------------------------------------------
//                                      tests
// ---------------------------------------------------------------------------------

#[cfg(test)]
mod test {
    use super::*;
    use rio_api::model::{Literal, NamedNode};
    use sophia_api::dataset::Dataset;
    use sophia_api::ns::rdf;
    use sophia_api::quad::stream::QuadSource;
    use sophia_api::term::matcher::ANY;
    use sophia_api::term::test::TestTerm;

    #[test]
    fn test_simple_trig_string() -> Result<(), Box<dyn std::error::Error>> {
        let trig = r#"
            @prefix : <http://example.org/ns/> .

            <#g1> {
                <#me> :knows _:alice.
            }
            <#g2> {
                _:alice a :Person ; :name "Alice".
            }
        "#;

        let p = TriGParser::new(trig.as_ref(), "http://localhost/ex")?;

        let d: Vec<([TestTerm<String>; 3], Option<TestTerm<String>>)> = p.collect_quads()?;
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
                &Literal::Simple { value: "Alice" },
                &Some(&NamedNode {
                    iri: "http://localhost/ex#g2"
                }),
            )
            .next()
            .is_some());
        Ok(())
    }
}
