use criterion::{criterion_group, criterion_main, Criterion, Throughput};
use rio_api::parser::{QuadsParser, TriplesParser};
use rio_testsuite::manifest::TestManifest;
use rio_testsuite::parser_evaluator::{
    parse_w3c_rdf_test_file, read_w3c_rdf_test_file, TestEvaluationError,
};
use rio_turtle::{NQuadsParser, NTriplesParser, TriGParser, TurtleError, TurtleParser};
use std::error::Error;
use std::io::Read;
use std::path::PathBuf;

fn get_test_path() -> PathBuf {
    let mut base_path = PathBuf::from(env!("CARGO_MANIFEST_DIR"));
    base_path.push("rdf-tests");
    return base_path;
}

fn test_data_from_testsuite(
    manifest_uri: String,
    include_tests_types: &[&str],
) -> Result<Vec<u8>, Box<dyn Error>> {
    let test_path = get_test_path();
    let manifest = TestManifest::new(manifest_uri, |url| parse_w3c_rdf_test_file(url, &test_path));
    let mut data = Vec::default();
    for test in manifest {
        let test = test?;
        if include_tests_types.contains(&test.kind.iri.as_str()) {
            read_w3c_rdf_test_file(&test.action, &test_path)?
                .read_to_end(&mut data)
                .map_err(|e| TestEvaluationError::IO(test.action, e))?;
            data.push(b'\n');
        }
    }
    Ok(data)
}

fn ntriples_test_data() -> Result<Vec<u8>, Box<dyn Error>> {
    test_data_from_testsuite(
        "http://w3c.github.io/rdf-tests/ntriples/manifest.ttl".to_owned(),
        &["http://www.w3.org/ns/rdftest#TestNTriplesPositiveSyntax"],
    )
}

fn nquads_test_data() -> Result<Vec<u8>, Box<dyn Error>> {
    test_data_from_testsuite(
        "http://w3c.github.io/rdf-tests/nquads/manifest.ttl".to_owned(),
        &["http://www.w3.org/ns/rdftest#TestNQuadsPositiveSyntax"],
    )
}

fn turtle_test_data() -> Result<Vec<u8>, Box<dyn Error>> {
    test_data_from_testsuite(
        "http://w3c.github.io/rdf-tests/turtle/manifest.ttl".to_owned(),
        &[
            "http://www.w3.org/ns/rdftest#TestTurtlePositiveSyntax",
            "http://www.w3.org/ns/rdftest#TestTurtleEval",
        ],
    )
}

fn trig_test_data() -> Result<Vec<u8>, Box<dyn Error>> {
    test_data_from_testsuite(
        "http://w3c.github.io/rdf-tests/trig/manifest.ttl".to_owned(),
        &[
            "http://www.w3.org/ns/rdftest#TestTrigPositiveSyntax",
            "http://www.w3.org/ns/rdftest#TestTrigEval",
        ],
    )
}

fn parse_bench(c: &mut Criterion, name: &str, data: Vec<u8>, bench: impl Fn(&[u8])) {
    let mut group = c.benchmark_group(name);
    group.throughput(Throughput::Bytes(data.len() as u64));
    group.bench_function("parse", |b| b.iter(|| bench(&data)));
    group.finish();
}

fn parse_ntriples(c: &mut Criterion, data: Vec<u8>) {
    parse_bench(c, "ntriples", data, |data| {
        let mut count: usize = 0;
        NTriplesParser::new(data)
            .unwrap()
            .parse_all(&mut |_| {
                count += 1;
                Ok(()) as Result<(), TurtleError>
            })
            .unwrap();
    });
}

fn parse_nquads(c: &mut Criterion, data: Vec<u8>) {
    parse_bench(c, "nquads", data, |data| {
        let mut count: usize = 0;
        NQuadsParser::new(data)
            .unwrap()
            .parse_all(&mut |_| {
                count += 1;
                Ok(()) as Result<(), TurtleError>
            })
            .unwrap();
    });
}

fn parse_turtle(c: &mut Criterion, data: Vec<u8>) {
    parse_bench(c, "turtle", data, |data| {
        let mut count: usize = 0;
        TurtleParser::new(data, "http://example.com/ex")
            .unwrap()
            .parse_all(&mut |_| {
                count += 1;
                Ok(()) as Result<(), TurtleError>
            })
            .unwrap();
    });
}

fn parse_trig(c: &mut Criterion, data: Vec<u8>) {
    parse_bench(c, "trig", data, |data| {
        let mut count: usize = 0;
        TriGParser::new(data, "http://example.com/ex")
            .unwrap()
            .parse_all(&mut |_| {
                count += 1;
                Ok(()) as Result<(), TurtleError>
            })
            .unwrap();
    });
}

fn bench_parse_ntriples_with_ntriples(c: &mut Criterion) {
    parse_ntriples(
        c,
        match ntriples_test_data() {
            Ok(d) => d,
            Err(e) => {
                eprintln!("{}", e);
                return;
            }
        },
    )
}

fn bench_parse_ntriples_with_turtle(c: &mut Criterion) {
    parse_turtle(
        c,
        match ntriples_test_data() {
            Ok(d) => d,
            Err(e) => {
                eprintln!("{}", e);
                return;
            }
        },
    )
}

fn bench_parse_nquads_with_nquads(c: &mut Criterion) {
    parse_nquads(
        c,
        match nquads_test_data() {
            Ok(d) => d,
            Err(e) => {
                eprintln!("{}", e);
                return;
            }
        },
    )
}

fn bench_parse_turtle_with_turtle(c: &mut Criterion) {
    parse_turtle(
        c,
        match turtle_test_data() {
            Ok(d) => d,
            Err(e) => {
                eprintln!("{}", e);
                return;
            }
        },
    )
}

fn bench_parse_trig_with_trig(c: &mut Criterion) {
    parse_trig(
        c,
        match trig_test_data() {
            Ok(d) => d,
            Err(e) => {
                eprintln!("{}", e);
                return;
            }
        },
    )
}

criterion_group!(
    w3c_testsuite,
    bench_parse_ntriples_with_ntriples,
    bench_parse_ntriples_with_turtle,
    bench_parse_nquads_with_nquads,
    bench_parse_turtle_with_turtle,
    bench_parse_trig_with_trig
);
criterion_main!(w3c_testsuite);
