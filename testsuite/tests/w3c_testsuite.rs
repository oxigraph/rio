use rio_testsuite::manifest::TestManifest;
use rio_testsuite::model::OwnedDataset;
use rio_testsuite::parser_evaluator::*;
use rio_testsuite::report::TestOutcome;
use std::error::Error;
use std::path::{Path, PathBuf};

fn get_test_path() -> PathBuf {
    let mut base_path = PathBuf::from(env!("CARGO_MANIFEST_DIR"));
    base_path.push("rdf-tests");
    base_path
}

fn run_testsuite(manifest_uri: &str) -> Result<(), Box<dyn Error>> {
    run_testsuite_with(manifest_uri, parse_w3c_rdf_test_file)
}

fn run_testsuite_with<R>(manifest_uri: &str, file_reader: R) -> Result<(), Box<dyn Error>>
where
    R: Fn(&str, &Path) -> Result<OwnedDataset, Box<dyn Error>>,
{
    let test_path = get_test_path();
    let manifest = TestManifest::new(manifest_uri.to_owned(), |file| {
        file_reader(file, &test_path)
    });

    let results = evaluate_parser_tests(manifest, |file| file_reader(file, &test_path))?;

    let mut errors = Vec::default();
    if results.is_empty() {
        errors.push(format!("<{}>: no entry found", manifest_uri));
    }
    for result in results {
        if let TestOutcome::Failed { error } = result.outcome {
            errors.push(format!("{}: failed with error {}", result.test, error))
        }
    }

    assert!(errors.is_empty(), "\n{}\n", errors.join("\n"));
    Ok(())
}

#[test]
fn ntriples_w3c_testsuite() -> Result<(), Box<dyn Error>> {
    run_testsuite("http://w3c.github.io/rdf-tests/ntriples/manifest.ttl")
}

#[test]
fn nquads_w3c_testsuite() -> Result<(), Box<dyn Error>> {
    run_testsuite_with(
        "http://w3c.github.io/rdf-tests/ntriples/manifest.ttl",
        parse_w3c_rdf_test_file_for_nquads,
    )
}

#[test]
fn turtle_w3c_testsuite() -> Result<(), Box<dyn Error>> {
    run_testsuite("http://w3c.github.io/rdf-tests/turtle/manifest.ttl")
}

#[test]
fn trig_w3c_testsuite() -> Result<(), Box<dyn Error>> {
    run_testsuite_with(
        "http://w3c.github.io/rdf-tests/trig/manifest.ttl",
        parse_w3c_rdf_test_file_for_trig,
    )
}

#[test]
fn rdf_xml_w3c_testsuite() -> Result<(), Box<dyn Error>> {
    run_testsuite("http://www.w3.org/2013/RDFXMLTests/manifest.ttl")
}

#[test]
fn ntriples_star_w3c_testsuite() -> Result<(), Box<dyn Error>> {
    run_testsuite("https://w3c.github.io/rdf-star/tests/nt/syntax/manifest.ttl")
}

#[test]
fn nquads_star_w3c_testsuite() -> Result<(), Box<dyn Error>> {
    run_testsuite_with(
        "https://w3c.github.io/rdf-star/tests/nt/syntax/manifest.ttl",
        parse_w3c_rdf_test_file_for_nquads,
    )
}

#[test]
fn turtle_star_w3c_testsuite() -> Result<(), Box<dyn Error>> {
    run_testsuite("https://w3c.github.io/rdf-star/tests/turtle/syntax/manifest.ttl")?;
    run_testsuite("https://w3c.github.io/rdf-star/tests/turtle/eval/manifest.ttl")?;
    Ok(())
}

#[test]
fn trig_star_w3c_testsuite() -> Result<(), Box<dyn Error>> {
    run_testsuite_with(
        "https://w3c.github.io/rdf-star/tests/trig/syntax/manifest.ttl",
        parse_w3c_rdf_test_file_for_trig,
    )?;
    run_testsuite_with(
        "https://w3c.github.io/rdf-star/tests/trig/eval/manifest.ttl",
        parse_w3c_rdf_test_file_for_trig,
    )?;
    run_testsuite_with(
        "https://w3c.github.io/rdf-star/tests/turtle/syntax/manifest.ttl",
        parse_w3c_rdf_test_file_for_trig,
    )?;
    run_testsuite_with(
        "https://w3c.github.io/rdf-star/tests/turtle/eval/manifest.ttl",
        parse_w3c_rdf_test_file_for_trig,
    )
}

#[cfg(feature = "generalized")]
#[test]
fn gtrig_w3c_testsuite() -> Result<(), Box<dyn Error>> {
    run_testsuite_with(
        "http://w3c.github.io/rdf-tests/trig/manifest.ttl",
        parse_w3c_rdf_test_file_for_gtrig,
    )
}
