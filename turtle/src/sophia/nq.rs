//! [Sophia] adapter for [N-Quads].
//!
//!
//! Example: count the number of of people using the `Sophia` API:
//! ```
//! use rio_api::model::NamedNode;
//! use rio_turtle::NQuadsParser;
//! use sophia_api::parser::QuadParser;
//! use sophia_api::quad::{Quad, stream::QuadSource};
//! use sophia_api::term::term_eq;
//! use sophia_api::ns::rdf;
//!
//! let file = b"
//! <http://example.com/foo> <http://www.w3.org/1999/02/22-rdf-syntax-ns#type> <http://schema.org/Person> <http://example/>.
//! <http://example.com/foo> <http://schema.org/name>  \"Foo\" <http://example/>.
//! <http://example.com/bar> <http://www.w3.org/1999/02/22-rdf-syntax-ns#type> <http://schema.org/Person>  <http://example/>.
//! <http://example.com/bar> <http://schema.org/name>  \"Bar\" <http://example/>.
//! ";
//!
//! let schema_person = NamedNode { iri: "http://schema.org/Person" };
//! let mut count = 0;
//! NQuadsParser::new(file.as_ref())
//!     .unwrap()
//!     .filter_quads(|q| term_eq(q.p(), &rdf::type_) && term_eq(q.o(), &schema_person))
//!     .for_each_quad(|_| { count += 1; })
//!     .unwrap();
//! assert_eq!(2, count)
//! ```
//!
//! [Sophia]: https://crates.io/crates/sophia
//! [N-Quads]: https://www.w3.org/TR/n-quads/

use crate::NQuadsParser;

impl_quad_source!(NQuadsParser);

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
    fn test_simple_nq_string() -> Result<(), Box<dyn std::error::Error>> {
        let nquads = r#"
            <http://localhost/ex#me> <http://example.org/ns/knows> _:b1.
            _:b1 <http://www.w3.org/1999/02/22-rdf-syntax-ns#type> <http://example.org/ns/Person> <tag:g1>.
            _:b1 <http://example.org/ns/name> "Alice" <tag:g1>.
        "#;

        let p = NQuadsParser::new(nquads.as_ref())?;

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
                #[allow(trivial_casts)]
                &(None as Option<&NamedNode<'_>>),
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
                &Some(&NamedNode { iri: "tag:g1" }),
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
                &Some(&NamedNode { iri: "tag:g1" }),
            )
            .next()
            .is_some());
        Ok(())
    }
}
