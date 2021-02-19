use rio_api::formatter::TriplesFormatter;
use rio_api::model::*;
use rio_api::parser::TriplesParser;
use rio_xml::{RdfXmlError, RdfXmlFormatter, RdfXmlParser};
use std::io::Cursor;

#[test]
#[allow(clippy::blacklisted_name)]
fn simple_roundtrip() -> Result<(), RdfXmlError> {
    let foo = NamedNode {
        iri: "http://example.com/foo",
    };
    let bar = NamedNode {
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
            subject: foo.into(),
            predicate: bar,
            object: bar.into(),
        },
        Triple {
            subject: foo.into(),
            predicate: bar,
            object: bnode.into(),
        },
        Triple {
            subject: foo.into(),
            predicate: bar,
            object: simple.into(),
        },
        Triple {
            subject: foo.into(),
            predicate: bar,
            object: language.into(),
        },
        Triple {
            subject: foo.into(),
            predicate: bar,
            object: datatype.into(),
        },
        Triple {
            subject: bar.into(),
            predicate: bar,
            object: bar.into(),
        },
        Triple {
            subject: bnode.into(),
            predicate: bar,
            object: bar.into(),
        },
        Triple {
            subject: bnode.into(),
            predicate: bad,
            object: foo.into(),
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
