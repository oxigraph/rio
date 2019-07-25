use rio_api::model::*;
use std::collections::HashSet;
use std::fmt;
use std::iter::FromIterator;

#[derive(Eq, PartialEq, Ord, PartialOrd, Debug, Clone, Hash)]
pub struct OwnedNamedNode {
    pub iri: String,
}

impl fmt::Display for OwnedNamedNode {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        NamedNode::from(self).fmt(f)
    }
}

impl From<NamedNode<'_>> for OwnedNamedNode {
    fn from(n: NamedNode) -> Self {
        Self {
            iri: n.iri.to_owned(),
        }
    }
}

impl<'a> From<&'a OwnedNamedNode> for NamedNode<'a> {
    fn from(n: &'a OwnedNamedNode) -> Self {
        Self { iri: &n.iri }
    }
}

#[derive(Eq, PartialEq, Ord, PartialOrd, Debug, Clone, Hash)]
pub struct OwnedBlankNode {
    pub id: String,
}

impl fmt::Display for OwnedBlankNode {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        BlankNode::from(self).fmt(f)
    }
}

impl From<BlankNode<'_>> for OwnedBlankNode {
    fn from(n: BlankNode) -> Self {
        Self {
            id: n.id.to_owned(),
        }
    }
}

impl<'a> From<&'a OwnedBlankNode> for BlankNode<'a> {
    fn from(n: &'a OwnedBlankNode) -> Self {
        Self { id: &n.id }
    }
}

#[derive(Eq, PartialEq, Ord, PartialOrd, Debug, Clone, Hash)]
pub enum OwnedLiteral {
    Simple {
        value: String,
    },
    LanguageTaggedString {
        value: String,
        language: String,
    },
    Typed {
        value: String,
        datatype: OwnedNamedNode,
    },
}

impl fmt::Display for OwnedLiteral {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        Literal::from(self).fmt(f)
    }
}

impl From<Literal<'_>> for OwnedLiteral {
    fn from(n: Literal) -> Self {
        match n {
            Literal::Simple { value } => OwnedLiteral::Simple {
                value: value.to_owned(),
            },
            Literal::LanguageTaggedString { value, language } => {
                OwnedLiteral::LanguageTaggedString {
                    value: value.to_owned(),
                    language: language.to_owned(),
                }
            }
            Literal::Typed { value, datatype } => OwnedLiteral::Typed {
                value: value.to_owned(),
                datatype: datatype.into(),
            },
        }
    }
}

impl<'a> From<&'a OwnedLiteral> for Literal<'a> {
    fn from(n: &'a OwnedLiteral) -> Self {
        match n {
            OwnedLiteral::Simple { value } => Literal::Simple { value: &value },
            OwnedLiteral::LanguageTaggedString { value, language } => {
                Literal::LanguageTaggedString {
                    value: &value,
                    language: &language,
                }
            }
            OwnedLiteral::Typed { value, datatype } => Literal::Typed {
                value: &value,
                datatype: datatype.into(),
            },
        }
    }
}

#[derive(Eq, PartialEq, Ord, PartialOrd, Debug, Clone, Hash)]
pub enum OwnedNamedOrBlankNode {
    NamedNode(OwnedNamedNode),
    BlankNode(OwnedBlankNode),
}

impl fmt::Display for OwnedNamedOrBlankNode {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        NamedOrBlankNode::from(self).fmt(f)
    }
}

impl From<NamedOrBlankNode<'_>> for OwnedNamedOrBlankNode {
    fn from(t: NamedOrBlankNode) -> Self {
        match t {
            NamedOrBlankNode::NamedNode(n) => OwnedNamedOrBlankNode::NamedNode(n.into()),
            NamedOrBlankNode::BlankNode(n) => OwnedNamedOrBlankNode::BlankNode(n.into()),
        }
    }
}

impl<'a> From<&'a OwnedNamedOrBlankNode> for NamedOrBlankNode<'a> {
    fn from(t: &'a OwnedNamedOrBlankNode) -> Self {
        match t {
            OwnedNamedOrBlankNode::NamedNode(n) => NamedOrBlankNode::NamedNode(n.into()),
            OwnedNamedOrBlankNode::BlankNode(n) => NamedOrBlankNode::BlankNode(n.into()),
        }
    }
}

impl From<OwnedNamedNode> for OwnedNamedOrBlankNode {
    fn from(node: OwnedNamedNode) -> Self {
        OwnedNamedOrBlankNode::NamedNode(node)
    }
}

impl From<OwnedBlankNode> for OwnedNamedOrBlankNode {
    fn from(node: OwnedBlankNode) -> Self {
        OwnedNamedOrBlankNode::BlankNode(node)
    }
}

#[derive(Eq, PartialEq, Ord, PartialOrd, Debug, Clone, Hash)]
pub enum OwnedTerm {
    NamedNode(OwnedNamedNode),
    BlankNode(OwnedBlankNode),
    Literal(OwnedLiteral),
}

impl fmt::Display for OwnedTerm {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        Term::from(self).fmt(f)
    }
}

impl From<Term<'_>> for OwnedTerm {
    fn from(t: Term) -> Self {
        match t {
            Term::NamedNode(n) => OwnedTerm::NamedNode(n.into()),
            Term::BlankNode(n) => OwnedTerm::BlankNode(n.into()),
            Term::Literal(n) => OwnedTerm::Literal(n.into()),
        }
    }
}

impl<'a> From<&'a OwnedTerm> for Term<'a> {
    fn from(t: &'a OwnedTerm) -> Self {
        match t {
            OwnedTerm::NamedNode(n) => Term::NamedNode(n.into()),
            OwnedTerm::BlankNode(n) => Term::BlankNode(n.into()),
            OwnedTerm::Literal(n) => Term::Literal(n.into()),
        }
    }
}

impl From<OwnedNamedNode> for OwnedTerm {
    fn from(node: OwnedNamedNode) -> Self {
        OwnedTerm::NamedNode(node)
    }
}

impl From<OwnedBlankNode> for OwnedTerm {
    fn from(node: OwnedBlankNode) -> Self {
        OwnedTerm::BlankNode(node)
    }
}

impl From<OwnedLiteral> for OwnedTerm {
    fn from(literal: OwnedLiteral) -> Self {
        OwnedTerm::Literal(literal)
    }
}

impl From<OwnedNamedOrBlankNode> for OwnedTerm {
    fn from(resource: OwnedNamedOrBlankNode) -> Self {
        match resource {
            OwnedNamedOrBlankNode::NamedNode(node) => OwnedTerm::NamedNode(node),
            OwnedNamedOrBlankNode::BlankNode(node) => OwnedTerm::BlankNode(node),
        }
    }
}

/// A [RDF triple](https://www.w3.org/TR/rdf11-concepts/#dfn-rdf-triple)
#[derive(Eq, PartialEq, Debug, Clone, Hash)]
pub struct OwnedTriple {
    pub subject: OwnedNamedOrBlankNode,
    pub predicate: OwnedNamedNode,
    pub object: OwnedTerm,
}

impl fmt::Display for OwnedTriple {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        Triple::from(self).fmt(f)
    }
}

impl From<Triple<'_>> for OwnedTriple {
    fn from(t: Triple) -> Self {
        Self {
            subject: t.subject.into(),
            predicate: t.predicate.into(),
            object: t.object.into(),
        }
    }
}

impl<'a> From<&'a OwnedTriple> for Triple<'a> {
    fn from(t: &'a OwnedTriple) -> Self {
        Self {
            subject: (&t.subject).into(),
            predicate: (&t.predicate).into(),
            object: (&t.object).into(),
        }
    }
}

#[derive(Default, Debug, Clone)]
pub struct OwnedGraph {
    inner: HashSet<OwnedTriple>,
}

impl OwnedGraph {
    pub fn insert(&mut self, t: impl Into<OwnedTriple>) {
        self.inner.insert(t.into());
    }

    pub fn iter(&self) -> impl Iterator<Item = &OwnedTriple> {
        self.inner.iter()
    }

    pub fn len(&self) -> usize {
        self.inner.len()
    }

    pub fn contains(&self, t: &OwnedTriple) -> bool {
        self.inner.contains(t)
    }

    pub fn triples_for_subject<'a>(
        &'a self,
        subject: impl Into<NamedOrBlankNode<'a>>,
    ) -> impl Iterator<Item = &'a OwnedTriple> + 'a {
        let subject: NamedOrBlankNode = subject.into();
        self.inner
            .iter()
            .filter(move |t| NamedOrBlankNode::from(&t.subject) == subject)
    }

    pub fn triples_for_object<'a>(
        &'a self,
        object: impl Into<Term<'a>>,
    ) -> impl Iterator<Item = &'a OwnedTriple> + 'a {
        let object: Term = object.into();
        self.inner
            .iter()
            .filter(move |t| Term::from(&t.object) == object)
    }

    pub fn object_for_subject_predicate<'a>(
        &'a self,
        subject: impl Into<NamedOrBlankNode<'a>>,
        predicate: impl Into<NamedNode<'a>>,
    ) -> Option<&'a OwnedTerm> {
        let subject: NamedOrBlankNode = subject.into();
        let predicate: NamedNode = predicate.into();
        self.inner
            .iter()
            .filter(move |t| {
                NamedOrBlankNode::from(&t.subject) == subject
                    && NamedNode::from(&t.predicate) == predicate
            })
            .map(|t| &t.object)
            .next()
    }
}

impl IntoIterator for OwnedGraph {
    type Item = OwnedTriple;
    type IntoIter = <HashSet<OwnedTriple> as IntoIterator>::IntoIter;

    fn into_iter(self) -> Self::IntoIter {
        self.inner.into_iter()
    }
}

impl FromIterator<OwnedTriple> for OwnedGraph {
    fn from_iter<I: IntoIterator<Item = OwnedTriple>>(iter: I) -> Self {
        Self {
            inner: HashSet::from_iter(iter),
        }
    }
}

impl fmt::Display for OwnedGraph {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        for t in &self.inner {
            writeln!(f, "{}", t)?;
        }
        Ok(())
    }
}
