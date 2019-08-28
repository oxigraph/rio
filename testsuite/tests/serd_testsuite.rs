use rio_api::parser::{QuadsParser, TriplesParser};
use rio_testsuite::manifest::TestManifest;
use rio_testsuite::model::OwnedDataset;
use rio_testsuite::parser_evaluator::*;
use rio_testsuite::report::TestOutcome;
use rio_turtle::{NQuadsParser, NTriplesParser, TriGParser, TurtleParser};
use std::error::Error;
use std::fs::File;
use std::io::BufReader;

pub fn parse_rdf_test_file(url: &str) -> Result<OwnedDataset, Box<dyn Error>> {
    let base = env!("CARGO_MANIFEST_DIR").to_owned() + "/serd-tests";
    let read = BufReader::new(
        File::open(&url.replace("http://drobilla.net/sw/serd/tests", &base))
            .map_err(|e| TestEvaluationError::IO(url.to_owned(), e))?,
    );

    if url.ends_with(".nt") {
        NTriplesParser::new(read)?
            .into_iter(|t| Ok(t.into()))
            .collect()
    } else if url.ends_with(".nq") {
        NQuadsParser::new(read)?
            .into_iter(|t| Ok(t.into()))
            .collect()
    } else if url.ends_with(".ttl") {
        TurtleParser::new(read, url)?
            .into_iter(|t| Ok(t.into()))
            .collect()
    } else if url.ends_with(".trig") {
        TriGParser::new(read, url)?
            .into_iter(|t| Ok(t.into()))
            .collect()
    } else {
        Err(Box::new(TestEvaluationError::UnsupportedFormat(
            url.to_owned(),
        )))
    }
}

fn run_testsuite(manifest_uri: String) -> Result<(), Box<dyn Error>> {
    let manifest = TestManifest::new(manifest_uri, |url| parse_rdf_test_file(url));

    let results = evaluate_parser_tests(manifest, |url| parse_rdf_test_file(url))?;

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
fn serd_good_testsuite() -> Result<(), Box<dyn Error>> {
    run_testsuite("http://drobilla.net/sw/serd/tests/good/manifest.ttl".to_owned())
}

#[test]
fn serd_bad_testsuite() -> Result<(), Box<dyn Error>> {
    run_testsuite("http://drobilla.net/sw/serd/tests/bad/manifest.ttl".to_owned())
}
