use rio_api::formatter::TriplesFormatter;
use rio_api::model::*;
use rio_api::parser::TriplesParser;
use rio_xml::{RdfXmlError, RdfXmlFormatter, RdfXmlParser};
use std::io::Cursor;

#[test]
fn simple_roundtrip() -> Result<(), RdfXmlError> {
    let foo_node = NamedNode {
        iri: "http://example.com/foo",
    };
    let bar_node = NamedNode {
        iri: "http://example.com/b%adar",
    };
    let bad = NamedNode {
        iri: "http://example.org/properties:p",
    };

    let bnode = BlankNode { id: "foobar" };
    let simple = Literal::Simple { value: "sim\"le" };
    let language = Literal::LanguageTaggedString {
        value: "sim\"le",
        language: "en",
    };
    let datatype = Literal::Typed {
        value: "sim\"le",
        datatype: NamedNode {
            iri: "http://example.com/d𓀀t",
        },
    };

    let graph = vec![
        Triple {
            subject: foo_node.into(),
            predicate: bar_node,
            object: bar_node.into(),
        },
        Triple {
            subject: foo_node.into(),
            predicate: bar_node,
            object: bnode.into(),
        },
        Triple {
            subject: foo_node.into(),
            predicate: bar_node,
            object: simple.into(),
        },
        Triple {
            subject: foo_node.into(),
            predicate: bar_node,
            object: language.into(),
        },
        Triple {
            subject: foo_node.into(),
            predicate: bar_node,
            object: datatype.into(),
        },
        Triple {
            subject: bar_node.into(),
            predicate: bar_node,
            object: bar_node.into(),
        },
        Triple {
            subject: bnode.into(),
            predicate: bar_node,
            object: bar_node.into(),
        },
        Triple {
            subject: bnode.into(),
            predicate: bad,
            object: foo_node.into(),
        },
    ];

    let mut formatter = RdfXmlFormatter::new(Vec::default())?;
    for t in &graph {
        formatter.format(t)?;
    }
    let xml = formatter.finish()?;

    let mut count = 0;
    RdfXmlParser::new(Cursor::new(&xml), None).parse_all(&mut |_| {
        count += 1;
        Ok(()) as Result<(), RdfXmlError>
    })?;

    assert_eq!(count, graph.len());

    Ok(())
}
