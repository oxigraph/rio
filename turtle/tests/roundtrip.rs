use rio_api::formatter::{QuadsFormatter, TriplesFormatter};
use rio_api::model::*;
use rio_api::parser::*;
use rio_turtle::*;
use std::io::Cursor;

#[test]
fn ntriples_roundtrip() -> Result<(), TurtleError> {
    let graph = example_graph(true);

    let mut formatter = NTriplesFormatter::new(Vec::default());
    for t in &graph {
        formatter.format(t)?;
    }
    let nt = formatter.finish()?;

    let mut count = 0;
    NTriplesParser::new(Cursor::new(&nt)).parse_all(&mut |_| {
        count += 1;
        Ok(()) as Result<(), TurtleError>
    })?;

    assert_eq!(count, graph.len());

    Ok(())
}

#[test]
fn nquads_roundtrip() -> Result<(), TurtleError> {
    let dataset = example_dataset(true);

    let mut formatter = NQuadsFormatter::new(Vec::default());
    for q in &dataset {
        formatter.format(q)?;
    }
    let nt = formatter.finish()?;

    let mut count = 0;
    NQuadsParser::new(Cursor::new(&nt)).parse_all(&mut |_| {
        count += 1;
        Ok(()) as Result<(), TurtleError>
    })?;

    assert_eq!(count, dataset.len());

    Ok(())
}

#[test]
fn turtle_roundtrip() -> Result<(), TurtleError> {
    let graph = example_graph(true);

    let mut formatter = TurtleFormatter::new(Vec::default());
    for t in &graph {
        formatter.format(t)?;
    }
    let turtle = formatter.finish()?;

    let mut count = 0;
    TurtleParser::new(Cursor::new(&turtle), None).parse_all(&mut |_| {
        count += 1;
        Ok(()) as Result<(), TurtleError>
    })?;

    assert_eq!(count, graph.len());

    Ok(())
}

#[test]
fn trig_roundtrip() -> Result<(), TurtleError> {
    let dataset = example_dataset(true);

    let mut formatter = TriGFormatter::new(Vec::default());
    for q in &dataset {
        formatter.format(q)?;
    }
    let trig = formatter.finish()?;

    let mut count = 0;
    TriGParser::new(Cursor::new(&trig), None).parse_all(&mut |_| {
        count += 1;
        Ok(()) as Result<(), TurtleError>
    })?;

    assert_eq!(count, dataset.len());

    Ok(())
}

#[cfg(feature = "generalized")]
#[test]
fn gtrig_roundtrip() -> Result<(), TurtleError> {
    let dataset = example_dataset(false);

    let mut formatter = TriGFormatter::new(Vec::default());
    for q in &dataset {
        formatter.format(q)?;
    }
    let trig = formatter.finish()?;

    let mut count = 0;
    GTriGParser::new(Cursor::new(&trig), None).parse_all(&mut |_| {
        count += 1;
        Ok(()) as Result<(), TurtleError>
    })?;

    assert_eq!(count, dataset.len());

    Ok(())
}

const TRIPLE: Triple<'static> = Triple {
    subject: Subject::NamedNode(NamedNode {
        iri: "http://example.com/s",
    }),
    predicate: NamedNode {
        iri: "http://example.com/p",
    },
    object: Term::NamedNode(NamedNode {
        iri: "http://example.com/o",
    }),
};

#[allow(clippy::disallowed_names)]
fn example_graph(rdf_star: bool) -> Vec<Triple<'static>> {
    let foo = NamedNode {
        iri: "http://example.com/foo",
    };
    let bar = NamedNode {
        iri: "http://example.com/bar",
    };
    let bnode = BlankNode { id: "foobar" };
    let simple = Literal::Simple { value: "simp\"le" };
    let language = Literal::LanguageTaggedString {
        value: "sim\"le",
        language: "en",
    };
    let datatype = Literal::Typed {
        value: "sim\"le",
        datatype: NamedNode {
            iri: "http://example.com/dt",
        },
    };
    let mut triples = vec![
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
            predicate: foo,
            object: datatype.into(),
        },
        Triple {
            subject: bar.into(),
            predicate: bar,
            object: bar.into(),
        },
    ];
    if rdf_star {
        triples.push(Triple {
            subject: (&TRIPLE).into(),
            predicate: bar,
            object: simple.into(),
        });
        triples.push(Triple {
            subject: foo.into(),
            predicate: bar,
            object: (&TRIPLE).into(),
        });
    }
    triples
}

fn example_dataset(rdf_star: bool) -> Vec<Quad<'static>> {
    example_graph(rdf_star)
        .into_iter()
        .flat_map(|t| {
            vec![
                Quad {
                    subject: t.subject,
                    predicate: t.predicate,
                    object: t.object,
                    graph_name: None,
                },
                Quad {
                    subject: t.subject,
                    predicate: t.predicate,
                    object: t.object,
                    graph_name: Some(
                        NamedNode {
                            iri: "http://example/",
                        }
                        .into(),
                    ),
                },
            ]
            .into_iter()
        })
        .collect()
}
