use rio_testsuite::manifest::TestManifest;
use rio_testsuite::parser_evaluator::*;
use rio_testsuite::report::TestOutcome;
use std::env;
use std::path::Path;

fn main() {
    let args: Vec<String> = env::args().collect();
    if args.len() != 3 {
        eprintln!("Expecting two arguments: <w3c_rdf_tests_base_path> <manifest_url>");
        return;
    }
    let test_path = Path::new(&args[1]);
    let manifest = TestManifest::new(args[2].clone(), |url| {
        parse_w3c_rdf_test_file(url, &test_path)
    });

    match evaluate_parser_tests(manifest, |url| parse_w3c_rdf_test_file(url, &test_path)) {
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
