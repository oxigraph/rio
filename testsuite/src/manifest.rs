use crate::model::*;
use crate::vocab::*;
use rio_api::model::Literal;
use rio_api::model::NamedNode;
use std::error::Error;
use std::fmt;

pub struct Test {
    pub id: OwnedNamedNode,
    pub kind: OwnedNamedNode,
    pub name: Option<String>,
    pub comment: Option<String>,
    pub action: String,
    pub result: Option<String>,
}

impl fmt::Display for Test {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.kind)?;
        for name in &self.name {
            write!(f, " named \"{}\"", name)?;
        }
        for comment in &self.comment {
            write!(f, " with comment \"{}\"", comment)?;
        }
        write!(f, " on file \"{}\"", self.action)?;
        Ok(())
    }
}

pub struct TestManifest<R: Fn(&str) -> Result<OwnedDataset, Box<dyn Error>>> {
    graph: OwnedDataset,
    tests_to_do: Vec<OwnedTerm>,
    manifests_to_do: Vec<String>,
    file_reader: R,
}

impl<R: Fn(&str) -> Result<OwnedDataset, Box<dyn Error>>> TestManifest<R> {
    pub fn new(manifest_url: String, file_reader: R) -> Self {
        Self {
            graph: OwnedDataset::default(),
            tests_to_do: Vec::default(),
            manifests_to_do: vec![manifest_url],
            file_reader,
        }
    }
}

impl<R: Fn(&str) -> Result<OwnedDataset, Box<dyn Error>>> Iterator for TestManifest<R> {
    type Item = Result<Test, Box<dyn Error>>;

    fn next(&mut self) -> Option<Result<Test, Box<dyn Error>>> {
        match self.tests_to_do.pop() {
            Some(OwnedTerm::NamedNode(test_node)) => {
                let test_subject = OwnedSubject::from(test_node.clone());
                let kind = match self
                    .graph
                    .object_for_subject_predicate(&test_subject, &rdf::TYPE)
                {
                    Some(OwnedTerm::NamedNode(c)) => c.clone(),
                    _ => return Some(Err(Box::new(TestManifestError::InvalidTestType(test_node)))),
                };
                let name = match self
                    .graph
                    .object_for_subject_predicate(&test_subject, &mf::NAME)
                {
                    Some(OwnedTerm::Literal(c)) => Some(literal_value(c).to_owned()),
                    _ => None,
                };
                let comment = match self
                    .graph
                    .object_for_subject_predicate(&test_subject, &rdfs::COMMENT)
                {
                    Some(OwnedTerm::Literal(c)) => Some(literal_value(c).to_owned()),
                    _ => None,
                };
                let action = match self
                    .graph
                    .object_for_subject_predicate(&test_subject, &mf::ACTION)
                {
                    Some(OwnedTerm::NamedNode(n)) => n.iri.clone(),
                    _ => {
                        return Some(Err(Box::new(TestManifestError::InvalidTestAction(
                            test_node,
                        ))))
                    }
                };
                let result = match self
                    .graph
                    .object_for_subject_predicate(&test_subject, &mf::RESULT)
                {
                    Some(OwnedTerm::NamedNode(n)) => Some(n.iri.clone()),
                    Some(_) => {
                        return Some(Err(Box::new(TestManifestError::InvalidTestResult(
                            test_node,
                        ))))
                    }
                    None => None,
                };
                Some(Ok(Test {
                    id: test_node,
                    kind,
                    name,
                    comment,
                    action,
                    result,
                }))
            }
            Some(_) => self.next(),
            None => {
                match self.manifests_to_do.pop() {
                    Some(url) => {
                        let manifest = OwnedNamedNode { iri: url.clone() };
                        match (self.file_reader)(&url) {
                            Ok(g) => g.into_iter().for_each(|g| self.graph.insert(g)),
                            Err(e) => return Some(Err(e)),
                        }

                        // New manifests
                        let manifest_subject = {
                            let manifest_type =
                                OwnedTerm::from(rio_api::model::Term::from(mf::MANIFEST));
                            let mut candidates: Vec<_> = self
                                .graph
                                .iter()
                                .filter_map(|q| {
                                    if rdf::TYPE == q.predicate && manifest_type == q.object {
                                        Some(q.subject.clone())
                                    } else {
                                        None
                                    }
                                })
                                .collect();
                            if candidates.is_empty() {
                                OwnedSubject::from(manifest.clone())
                            } else if candidates.len() == 1 {
                                candidates.drain(0..1).next().unwrap()
                            } else {
                                return Some(Err(Box::new(TestManifestError::AmbigiousManifest(
                                    manifest,
                                ))));
                            }
                        };
                        match self
                            .graph
                            .object_for_subject_predicate(&manifest_subject, &mf::INCLUDE)
                        {
                            Some(OwnedTerm::BlankNode(list)) => {
                                self.manifests_to_do.extend(
                                    RdfListIterator::iter(&self.graph, list.clone().into())
                                        .filter_map(|m| match m {
                                            OwnedTerm::NamedNode(nm) => Some(dbg!(nm.iri)),
                                            _ => None,
                                        }),
                                );
                            }
                            Some(_) => {
                                return Some(Err(Box::new(TestManifestError::InvalidManifestList(
                                    manifest,
                                ))))
                            }
                            None => (),
                        }

                        // New tests
                        match self
                            .graph
                            .object_for_subject_predicate(&manifest_subject, &mf::ENTRIES)
                        {
                            Some(OwnedTerm::BlankNode(list)) => {
                                self.tests_to_do.extend(RdfListIterator::iter(
                                    &self.graph,
                                    list.clone().into(),
                                ));
                            }
                            Some(_) => {
                                return Some(Err(Box::new(TestManifestError::InvalidTestList(
                                    manifest,
                                ))))
                            }
                            None => (),
                        }
                    }
                    None => return None,
                }
                self.next()
            }
        }
    }
}

#[derive(Debug, Clone)]
pub enum TestManifestError {
    AmbigiousManifest(OwnedNamedNode),
    InvalidTestType(OwnedNamedNode),
    InvalidTestAction(OwnedNamedNode),
    InvalidTestResult(OwnedNamedNode),
    InvalidManifestList(OwnedNamedNode),
    InvalidTestList(OwnedNamedNode),
}

impl fmt::Display for TestManifestError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> Result<(), fmt::Error> {
        match self {
            TestManifestError::AmbigiousManifest(t) => {
                write!(f, "Could not decide what is the manifest IRI in {}", t)
            }
            TestManifestError::InvalidTestType(t) => {
                write!(f, "The test {} has an unsupported or missing rdf:type", t)
            }
            TestManifestError::InvalidTestAction(t) => {
                write!(f, "The test {} has an invalid or missing mf:action", t)
            }
            TestManifestError::InvalidTestResult(t) => {
                write!(f, "The test {} has an invalid mf:result", t)
            }
            TestManifestError::InvalidManifestList(t) => {
                write!(f, "The manifest {} contains an invalid mf:include list", t)
            }
            TestManifestError::InvalidTestList(t) => {
                write!(f, "The manifest {} contains an invalid mf:entries list", t)
            }
        }
    }
}

impl Error for TestManifestError {}

pub struct RdfListIterator<'a> {
    graph: &'a OwnedDataset,
    current_node: Option<OwnedSubject>,
}

impl<'a> RdfListIterator<'a> {
    fn iter(graph: &'a OwnedDataset, root: OwnedSubject) -> RdfListIterator<'a> {
        RdfListIterator {
            graph,
            current_node: Some(root),
        }
    }
}

impl<'a> Iterator for RdfListIterator<'a> {
    type Item = OwnedTerm;

    fn next(&mut self) -> Option<OwnedTerm> {
        match self.current_node.clone() {
            Some(current) => {
                let result = self
                    .graph
                    .object_for_subject_predicate(&current, &rdf::FIRST)
                    .cloned();
                self.current_node = match self
                    .graph
                    .object_for_subject_predicate(&current, &rdf::REST)
                {
                    Some(OwnedTerm::NamedNode(n)) if NamedNode::from(n) == rdf::NIL => None,
                    Some(OwnedTerm::NamedNode(n)) => Some(n.clone().into()),
                    Some(OwnedTerm::BlankNode(n)) => Some(n.clone().into()),
                    _ => None,
                };
                result
            }
            None => None,
        }
    }
}

fn literal_value<'a>(l: impl Into<Literal<'a>>) -> &'a str {
    match l.into() {
        Literal::Simple { value } => value,
        Literal::LanguageTaggedString { value, .. } => value,
        Literal::Typed { value, .. } => value,
    }
}
