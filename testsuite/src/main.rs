use rio_testsuite::manifest::TestManifest;
use rio_testsuite::model::OwnedDataset;
use rio_testsuite::parser_evaluator::*;
use rio_testsuite::report::TestOutcome;
use std::env;
use std::error::Error;
use std::path::Path;

fn main() {
    let args: Vec<String> = env::args().collect();
    if args.len() != 3 {
        eprintln!("Expecting two arguments: <w3c_rdf_tests_base_path> <manifest_url>");
        return;
    }
    let mut parse_func: fn(&str, &Path) -> Result<OwnedDataset, Box<dyn Error>> =
        parse_w3c_rdf_test_file;
    let mut manifest_url: &str = &args[2];

    if args[2] == "ntriples" || args[2] == "nt" {
        manifest_url = "http://w3c.github.io/rdf-tests/ntriples/manifest.ttl";
    }
    if args[2] == "nquads" || args[2] == "nq" {
        manifest_url = "http://w3c.github.io/rdf-tests/nquads/manifest.ttl";
    }
    if args[2] == "turtle" || args[2] == "ttl" {
        manifest_url = "http://w3c.github.io/rdf-tests/turtle/manifest.ttl";
    }
    if args[2] == "rdf-xml" || args[2] == "rdf" {
        manifest_url = "http://w3c.github.io/rdf-tests/rdf-xml/manifest.ttl";
    }
    if args[2] == "trig" {
        manifest_url = "http://w3c.github.io/rdf-tests/trig/manifest.ttl";
    }
    #[cfg(feature = "generalized")]
    {
        if args[2] == "gtrig" {
            parse_func = parse_w3c_rdf_test_file_with_gtrig;
            manifest_url = "http://w3c.github.io/rdf-tests/trig/manifest.ttl";
        }
    }

    let test_path = Path::new(&args[1]);
    let manifest = TestManifest::new(manifest_url.to_string(), |url| parse_func(url, &test_path));

    match evaluate_parser_tests(manifest, |url| parse_func(url, &test_path)) {
        Ok(results) => {
            for result in results {
                match result.outcome {
                    TestOutcome::Passed => println!("{}: passed", result.test),
                    TestOutcome::Failed { error } => {
                        println!("{}: failed with error {}", result.test, error)
                    }
                }
            }
        }
        Err(e) => eprintln!("{}", e),
    }
}
