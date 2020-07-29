use criterion::{criterion_group, criterion_main, BenchmarkId, Criterion, Throughput};
use oxiri::Iri;
use rio_api::parser::*;
use rio_testsuite::manifest::TestManifest;
use rio_testsuite::parser_evaluator::*;
use rio_turtle::*;
use std::error::Error;
use std::io::Read;
use std::path::PathBuf;

fn get_test_path() -> PathBuf {
    let mut base_path = PathBuf::from(env!("CARGO_MANIFEST_DIR"));
    base_path.push("rdf-tests");
    base_path
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

fn parse_bench(
    c: &mut Criterion,
    parser_name: &str,
    data_name: &str,
    data: Vec<u8>,
    bench: impl Fn(&[u8]),
) {
    let mut group = c.benchmark_group(parser_name);
    group.throughput(Throughput::Bytes(data.len() as u64));
    group.bench_with_input(BenchmarkId::from_parameter(data_name), &data, |b, data| {
        b.iter(|| bench(&data))
    });
    group.finish();
}

fn parse_ntriples(c: &mut Criterion, name: &str, data: Vec<u8>) {
    parse_bench(c, "ntriples", name, data, |data| {
        let mut count: usize = 0;
        NTriplesParser::new(data)
            .parse_all(&mut |_| {
                count += 1;
                Ok(()) as Result<(), TurtleError>
            })
            .unwrap();
    });
}

fn parse_nquads(c: &mut Criterion, name: &str, data: Vec<u8>) {
    parse_bench(c, "nquads", name, data, |data| {
        let mut count: usize = 0;
        NQuadsParser::new(data)
            .parse_all(&mut |_| {
                count += 1;
                Ok(()) as Result<(), TurtleError>
            })
            .unwrap();
    });
}

fn parse_turtle(c: &mut Criterion, name: &str, data: Vec<u8>) {
    let base_iri = Iri::parse("http://example.com/ex".to_owned()).unwrap();
    parse_bench(c, "turtle", name, data, |data| {
        let mut count: usize = 0;
        TurtleParser::new(data, Some(base_iri.clone()))
            .parse_all(&mut |_| {
                count += 1;
                Ok(()) as Result<(), TurtleError>
            })
            .unwrap();
    });
}

fn parse_trig(c: &mut Criterion, name: &str, data: Vec<u8>) {
    let base_iri = Iri::parse("http://example.com/ex".to_owned()).unwrap();
    parse_bench(c, "trig", name, data, |data| {
        let mut count: usize = 0;
        TriGParser::new(data, Some(base_iri.clone()))
            .parse_all(&mut |_| {
                count += 1;
                Ok(()) as Result<(), TurtleError>
            })
            .unwrap();
    });
}

#[cfg(feature = "generalized")]
fn parse_gtrig(c: &mut Criterion, name: &str, data: Vec<u8>) {
    parse_bench(c, "gtrig", name, data, |data| {
        let mut count: usize = 0;
        GTriGParser::new(
            data,
            Some(Iri::parse("http://example.org/base/".to_owned()).unwrap()),
        )
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
        "ntriples",
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
        "ntriples",
        match ntriples_test_data() {
            Ok(d) => d,
            Err(e) => {
                eprintln!("{}", e);
                return;
            }
        },
    )
}

#[allow(unused_variables)]
fn bench_parse_ntriples_with_gtrig(c: &mut Criterion) {
    #[cfg(feature = "generalized")]
    parse_gtrig(
        c,
        "ntriples",
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
        "nquads",
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
        "turtle",
        match turtle_test_data() {
            Ok(d) => d,
            Err(e) => {
                eprintln!("{}", e);
                return;
            }
        },
    )
}

#[allow(unused_variables)]
fn bench_parse_turtle_with_gtrig(c: &mut Criterion) {
    #[cfg(feature = "generalized")]
    parse_gtrig(
        c,
        "turtle",
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
        "trig",
        match trig_test_data() {
            Ok(d) => d,
            Err(e) => {
                eprintln!("{}", e);
                return;
            }
        },
    )
}

#[allow(unused_variables)]
fn bench_parse_trig_with_gtrig(c: &mut Criterion) {
    #[cfg(feature = "generalized")]
    parse_gtrig(
        c,
        "trig",
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
    bench_parse_ntriples_with_gtrig,
    bench_parse_nquads_with_nquads,
    bench_parse_turtle_with_turtle,
    bench_parse_turtle_with_gtrig,
    bench_parse_trig_with_trig,
    bench_parse_trig_with_gtrig,
);

criterion_main!(w3c_testsuite);
