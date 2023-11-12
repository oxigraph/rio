use oxiri::Iri;
use rio_api::parser::{QuadsParser, TriplesParser};
use rio_testsuite::manifest::TestManifest;
use rio_testsuite::model::OwnedDataset;
use rio_testsuite::parser_evaluator::*;
use rio_testsuite::report::TestOutcome;
use rio_turtle::{NQuadsParser, NTriplesParser, TriGParser, TurtleParser};
use rio_xml::RdfXmlParser;
use std::error::Error;
use std::fs::File;
use std::io::BufReader;

pub fn parse_rdf_test_file(url: &str) -> Result<OwnedDataset, Box<dyn Error>> {
    let base = env!("CARGO_MANIFEST_DIR").to_owned() + "/rio-tests";
    let read = BufReader::new(
        File::open(url.replace("https://github.com/Tpt/rio/tests", &base))
            .map_err(|e| TestEvaluationError::Io(url.to_owned(), e))?,
    );
    let base_iri = Iri::parse(url.to_owned())?;

    if url.ends_with(".nt") {
        NTriplesParser::new(read)
            .into_iter(|t| Ok(t.into()))
            .collect()
    } else if url.ends_with(".nq") {
        NQuadsParser::new(read)
            .into_iter(|t| Ok(t.into()))
            .collect()
    } else if url.ends_with(".ttl") {
        TurtleParser::new(read, Some(base_iri))
            .into_iter(|t| Ok(t.into()))
            .collect()
    } else if url.ends_with(".trig") {
        TriGParser::new(read, Some(base_iri))
            .into_iter(|t| Ok(t.into()))
            .collect()
    } else if url.ends_with(".rdf") {
        RdfXmlParser::new(read, Some(base_iri))
            .into_iter(|t| Ok(t.into()))
            .collect()
    } else {
        Err(Box::new(TestEvaluationError::UnsupportedFormat(
            url.to_owned(),
        )))
    }
}

fn run_testsuite(manifest_uri: String) -> Result<(), Box<dyn Error>> {
    let manifest = TestManifest::new(manifest_uri, parse_rdf_test_file);

    let results = evaluate_parser_tests(manifest, parse_rdf_test_file)?;

    let mut errors = Vec::default();
    for result in results {
        if let TestOutcome::Failed { error } = result.outcome {
            errors.push(format!("{}: failed with error {}", result.test, error))
        }
    }

    assert!(errors.is_empty(), "\n{}\n", errors.join("\n"));
    Ok(())
}

#[test]
fn rio_testsuite() -> Result<(), Box<dyn Error>> {
    run_testsuite("https://github.com/Tpt/rio/tests/manifest.ttl".to_owned())
}
