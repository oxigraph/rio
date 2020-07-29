use crate::isomorphism::are_datasets_isomorphic;
use crate::manifest::{Test, TestManifestError};
use crate::model::OwnedDataset;
use crate::report::{TestOutcome, TestResult};
use chrono::Utc;
use oxiri::Iri;
use rio_api::model::*;
use rio_api::parser::*;
use rio_turtle::*;
use rio_xml::RdfXmlParser;
use std::convert::TryFrom;
use std::error::Error;
use std::fmt;
use std::fs::File;
use std::io;
use std::io::BufReader;
use std::path::Path;

pub fn evaluate_parser_tests(
    manifest: impl Iterator<Item = Result<Test, Box<dyn Error>>>,
    file_reader: impl Fn(&str) -> Result<OwnedDataset, Box<dyn Error>>,
) -> Result<Vec<TestResult>, Box<dyn Error>> {
    manifest
        .map(|test| {
            let test = test?;
            let outcome = if &test.kind.iri
                == "http://www.w3.org/ns/rdftest#TestNTriplesPositiveSyntax"
                || &test.kind.iri == "http://www.w3.org/ns/rdftest#TestNQuadsPositiveSyntax"
                || &test.kind.iri == "http://www.w3.org/ns/rdftest#TestTurtlePositiveSyntax"
                || &test.kind.iri == "http://www.w3.org/ns/rdftest#TestTrigPositiveSyntax"
            {
                match file_reader(&test.action) {
                    Ok(_) => TestOutcome::Passed,
                    Err(e) => TestOutcome::Failed {
                        error: format!("Parse error on file {}: {}", &test.action, e),
                    },
                }
            } else if &test.kind.iri == "http://www.w3.org/ns/rdftest#TestNTriplesNegativeSyntax"
                || &test.kind.iri == "http://www.w3.org/ns/rdftest#TestNQuadsNegativeSyntax"
                || &test.kind.iri == "http://www.w3.org/ns/rdftest#TestTurtleNegativeSyntax"
                || &test.kind.iri == "http://www.w3.org/ns/rdftest#TestTurtleNegativeEval"
                || &test.kind.iri == "http://www.w3.org/ns/rdftest#TestTrigNegativeSyntax"
                || &test.kind.iri == "http://www.w3.org/ns/rdftest#TestTrigNegativeEval"
                || &test.kind.iri == "http://www.w3.org/ns/rdftest#TestXMLNegativeSyntax"
            {
                match file_reader(&test.action) {
                    Ok(_) => TestOutcome::Failed {
                        error: "file parsed without error even if it should not".to_owned(),
                    },
                    Err(_) => TestOutcome::Passed,
                }
            } else if &test.kind.iri == "http://www.w3.org/ns/rdftest#TestTurtleEval" || &test.kind.iri == "http://www.w3.org/ns/rdftest#TestTrigEval" || &test.kind.iri == "http://www.w3.org/ns/rdftest#TestXMLEval" {
                match file_reader(&test.action) {
                    Ok(actual_graph) => {
                        if let Some(result) = &test.result {
                            match file_reader(result) {
                                Ok(expected_graph) => if are_datasets_isomorphic(&expected_graph, &actual_graph) {
                                    TestOutcome::Passed
                                } else {
                                    TestOutcome::Failed {
                                        error: format!("The two files are not isomorphics. Expected:\n{}\nActual:\n{}", expected_graph, actual_graph),
                                    }
                                }
                                Err(e) => TestOutcome::Failed {
                                    error: format!("Parse error on file {}: {}", &test.action, e),
                                },
                            }
                        } else {
                            return Err(TestManifestError::InvalidTestResult(test.id.clone()).into())
                        }
                    }
                    Err(e) => TestOutcome::Failed {
                        error: format!("Parse error on file {}: {}", &test.action, e),
                    },
                }
            } else {
                return Err(TestManifestError::InvalidTestType(test.kind.clone()).into())
            };
            Ok(TestResult {
                test: test.id,
                outcome,
                date: Utc::now(),
            })
        })
        .collect()
}

pub fn read_w3c_rdf_test_file(
    url: &str,
    tests_path: &Path,
) -> Result<BufReader<File>, Box<dyn Error>> {
    let mut path = tests_path.to_owned();
    path.push(if url.starts_with("http://w3c.github.io/rdf-tests/") {
        Ok(url.replace("http://w3c.github.io/rdf-tests/", ""))
    } else if url.starts_with("http://www.w3.org/2013/RDFXMLTests/") {
        Ok(url.replace("http://www.w3.org/2013/RDFXMLTests/", "rdf-xml/"))
    } else {
        Err(Box::new(TestEvaluationError::UnknownTestUrl(
            url.to_owned(),
        )))
    }?);

    Ok(BufReader::new(File::open(&path).map_err(|e| {
        TestEvaluationError::IO(path.to_string_lossy().to_string(), e)
    })?))
}

pub fn parse_w3c_rdf_test_file(
    url: &str,
    tests_path: &Path,
) -> Result<OwnedDataset, Box<dyn Error>> {
    let read = read_w3c_rdf_test_file(url, tests_path)?;

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

#[cfg(feature = "generalized")]
pub fn parse_w3c_rdf_test_file_for_gtrig(
    url: &str,
    tests_path: &Path,
) -> Result<OwnedDataset, Box<dyn Error>> {
    let read = read_w3c_rdf_test_file(url, tests_path)?;
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
        GTriGParser::new(read, Some(base_iri))
            .into_iter(|t| Ok(Quad::try_from(t)?.into()))
            .collect()
    } else {
        Err(Box::new(TestEvaluationError::UnsupportedFormat(
            url.to_owned(),
        )))
    }
}

#[derive(Debug)]
pub enum TestEvaluationError {
    UnknownTestUrl(String),
    UnsupportedFormat(String),
    IO(String, io::Error),
}

impl fmt::Display for TestEvaluationError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> Result<(), fmt::Error> {
        match self {
            TestEvaluationError::UnknownTestUrl(u) => {
                write!(f, "The URL {} does not corresponds to a known RDF test", u)
            }
            TestEvaluationError::UnsupportedFormat(u) => write!(
                f,
                "The extension of {} does not match any supported format",
                u
            ),
            TestEvaluationError::IO(file, error) => {
                write!(f, "I/O error on file {}: {}", file, error)
            }
        }
    }
}

impl Error for TestEvaluationError {}
