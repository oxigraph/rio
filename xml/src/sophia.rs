//! [Sophia] adapters for the [RDF/XML] parser.
//!
//! This module is available if feature `sophia` is enabled.
//!
//! Example: count the number of of people using the `Sophia` API:
//! ```
//! use rio_api::model::NamedNode;
//! use rio_xml::sophia::RdfXmlParser;
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
//! RdfXmlParser::default()
//!     .parse(file.as_ref())
//!     .filter_triples(|t| term_eq(t.p(), &rdf::type_) && term_eq(t.o(), &schema_person))
//!     .for_each_triple(|_| { count += 1; })
//!     .unwrap();
//! assert_eq!(2, count)
//! ```
//!
//! [Sophia]: https://crates.io/crates/sophia
//! [RDF/XML]: https://www.w3.org/TR/rdf-syntax-grammar/

use crate::{RdfXmlError, RdfXmlParser as RioRdfXmlParser};
use rio_api::sophia::*;
use sophia_api::parser::TripleParser;
use std::io::BufRead;

/// An implementation of [`sophia_api::parser::TripleParser`]
/// around [the RDF/XML parser].
///
/// [`sophia_api::parser::TripleParser`]: https://docs.rs/sophia_api/latest/sophia_api/parser/trait.TripleParser.html
/// [the RDF/XML parser]: ../../struct.RdfXmlParser.html
#[derive(Clone, Debug, Default)]
pub struct RdfXmlParser {
    pub base: Option<String>,
}

impl<B: BufRead> TripleParser<B> for RdfXmlParser {
    type Source = StrictRioSource<RioRdfXmlParser<B>, RdfXmlError>;
    fn parse(&self, data: B) -> Self::Source {
        let base: &str = match &self.base {
            Some(base) => &base,
            None => "x-no-base:///",
        };
        StrictRioSource::from(RioRdfXmlParser::new(data, base))
    }
}

sophia_api::def_mod_functions_for_bufread_parser!(RdfXmlParser, TripleParser);

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
    fn test_simple_xml_string() -> Result<(), Box<dyn std::error::Error>> {
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

        let p = RdfXmlParser {
            base: Some("http://localhost/ex".into()),
        };

        let g: Vec<[TestTerm<String>; 3]> = p.parse_str(&xml).collect_triples()?;
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
