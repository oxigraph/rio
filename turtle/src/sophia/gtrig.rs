//! Sophia adapter for generalized TriG.

use crate::GTriGParser;

impl_quad_source_generalized!(GTriGParser);

#[cfg(test)]
mod test {
    use super::*;
    use oxiri::Iri;
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

        let p = GTriGParser::new(
            gtrig.as_ref(),
            Some(Iri::parse("http://localhost/ex".to_owned())?),
        );

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
