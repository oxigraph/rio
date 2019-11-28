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
    let mut manifest_url = match args[2].as_ref() {
        "ntriples" | "nt" => "http://w3c.github.io/rdf-tests/ntriples/manifest.ttl",
        "nquads" | "nq" => "http://w3c.github.io/rdf-tests/nquads/manifest.ttl",
        "turtle" | "ttl" => "http://w3c.github.io/rdf-tests/turtle/manifest.ttl",
        "rdf-xml" | "rdf" => "http://w3c.github.io/rdf-tests/rdf-xml/manifest.ttl",
        "trig" => "http://w3c.github.io/rdf-tests/trig/manifest.ttl",
        _ => &args[2],
    };

    #[cfg(feature = "generalized")]
    {
        if manifest_url == "gtrig" {
            manifest_url = "http://w3c.github.io/rdf-tests/trig/manifest.ttl";
            parse_func = parse_w3c_rdf_test_file_for_gtrig;
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
