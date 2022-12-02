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
    fn from(n: NamedNode<'_>) -> Self {
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

impl PartialEq<OwnedNamedNode> for NamedNode<'_> {
    fn eq(&self, other: &OwnedNamedNode) -> bool {
        self.iri == other.iri
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
    fn from(n: BlankNode<'_>) -> Self {
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

impl PartialEq<OwnedBlankNode> for BlankNode<'_> {
    fn eq(&self, other: &OwnedBlankNode) -> bool {
        self.id == other.id
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
    fn from(n: Literal<'_>) -> Self {
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
            OwnedLiteral::Simple { value } => Literal::Simple { value },
            OwnedLiteral::LanguageTaggedString { value, language } => {
                Literal::LanguageTaggedString { value, language }
            }
            OwnedLiteral::Typed { value, datatype } => Literal::Typed {
                value,
                datatype: datatype.into(),
            },
        }
    }
}

impl PartialEq<OwnedLiteral> for Literal<'_> {
    fn eq(&self, other: &OwnedLiteral) -> bool {
        match (self, other) {
            (Literal::Simple { value }, OwnedLiteral::Simple { value: v2 }) => value == v2,
            (
                Literal::LanguageTaggedString { value, language },
                OwnedLiteral::LanguageTaggedString {
                    value: v2,
                    language: l2,
                },
            ) => value == v2 && language == l2,
            (
                Literal::Typed { value, datatype },
                OwnedLiteral::Typed {
                    value: v2,
                    datatype: d2,
                },
            ) => value == v2 && datatype == d2,
            _ => false,
        }
    }
}

#[derive(Eq, PartialEq, Ord, PartialOrd, Debug, Clone, Hash)]
pub enum OwnedSubject {
    NamedNode(OwnedNamedNode),
    BlankNode(OwnedBlankNode),
    Triple(Box<OwnedTriple>),
}

impl fmt::Display for OwnedSubject {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            OwnedSubject::NamedNode(n) => n.fmt(f),
            OwnedSubject::BlankNode(n) => n.fmt(f),
            OwnedSubject::Triple(t) => write!(f, "<< {} >>", t),
        }
    }
}

impl From<Subject<'_>> for OwnedSubject {
    fn from(t: Subject<'_>) -> Self {
        match t {
            Subject::NamedNode(n) => OwnedSubject::NamedNode(n.into()),
            Subject::BlankNode(n) => OwnedSubject::BlankNode(n.into()),
            Subject::Triple(t) => OwnedSubject::Triple(Box::new(OwnedTriple::from(*t))),
        }
    }
}

impl PartialEq<OwnedSubject> for Subject<'_> {
    fn eq(&self, other: &OwnedSubject) -> bool {
        match (self, other) {
            (Subject::NamedNode(n1), OwnedSubject::NamedNode(n2)) => n1 == n2,
            (Subject::BlankNode(n1), OwnedSubject::BlankNode(n2)) => n1 == n2,
            (Subject::Triple(t1), OwnedSubject::Triple(t2)) => *t1 == &**t2,
            _ => false,
        }
    }
}

impl From<OwnedNamedNode> for OwnedSubject {
    fn from(node: OwnedNamedNode) -> Self {
        OwnedSubject::NamedNode(node)
    }
}

impl From<OwnedBlankNode> for OwnedSubject {
    fn from(node: OwnedBlankNode) -> Self {
        OwnedSubject::BlankNode(node)
    }
}

#[derive(Eq, PartialEq, Ord, PartialOrd, Debug, Clone, Hash)]
pub enum OwnedGraphName {
    NamedNode(OwnedNamedNode),
    BlankNode(OwnedBlankNode),
}

impl fmt::Display for OwnedGraphName {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        GraphName::from(self).fmt(f)
    }
}

impl From<GraphName<'_>> for OwnedGraphName {
    fn from(t: GraphName<'_>) -> Self {
        match t {
            GraphName::NamedNode(n) => OwnedGraphName::NamedNode(n.into()),
            GraphName::BlankNode(n) => OwnedGraphName::BlankNode(n.into()),
        }
    }
}

impl<'a> From<&'a OwnedGraphName> for GraphName<'a> {
    fn from(t: &'a OwnedGraphName) -> Self {
        match t {
            OwnedGraphName::NamedNode(n) => GraphName::NamedNode(n.into()),
            OwnedGraphName::BlankNode(n) => GraphName::BlankNode(n.into()),
        }
    }
}

impl From<OwnedNamedNode> for OwnedGraphName {
    fn from(node: OwnedNamedNode) -> Self {
        OwnedGraphName::NamedNode(node)
    }
}

impl From<OwnedBlankNode> for OwnedGraphName {
    fn from(node: OwnedBlankNode) -> Self {
        OwnedGraphName::BlankNode(node)
    }
}

impl PartialEq<OwnedGraphName> for GraphName<'_> {
    fn eq(&self, other: &OwnedGraphName) -> bool {
        match (self, other) {
            (GraphName::NamedNode(n1), OwnedGraphName::NamedNode(n2)) => n1 == n2,
            (GraphName::BlankNode(n1), OwnedGraphName::BlankNode(n2)) => n1 == n2,
            _ => false,
        }
    }
}

fn same_graph_name(gn1: &Option<GraphName<'_>>, gn2: &Option<OwnedGraphName>) -> bool {
    match (gn1, gn2) {
        (Some(n1), Some(n2)) => n1 == n2,
        (None, None) => true,
        _ => false,
    }
}

#[derive(Eq, PartialEq, Ord, PartialOrd, Debug, Clone, Hash)]
pub enum OwnedTerm {
    NamedNode(OwnedNamedNode),
    BlankNode(OwnedBlankNode),
    Literal(OwnedLiteral),
    Triple(Box<OwnedTriple>),
}

impl fmt::Display for OwnedTerm {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            OwnedTerm::NamedNode(n) => n.fmt(f),
            OwnedTerm::BlankNode(n) => n.fmt(f),
            OwnedTerm::Literal(n) => n.fmt(f),
            OwnedTerm::Triple(t) => write!(f, "<< {} >>", t),
        }
    }
}

impl From<Term<'_>> for OwnedTerm {
    fn from(t: Term<'_>) -> Self {
        match t {
            Term::NamedNode(n) => OwnedTerm::NamedNode(n.into()),
            Term::BlankNode(n) => OwnedTerm::BlankNode(n.into()),
            Term::Literal(n) => OwnedTerm::Literal(n.into()),
            Term::Triple(t) => OwnedTerm::Triple(Box::new(OwnedTriple::from(*t))),
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

impl From<OwnedSubject> for OwnedTerm {
    fn from(other: OwnedSubject) -> Self {
        match other {
            OwnedSubject::NamedNode(node) => OwnedTerm::NamedNode(node),
            OwnedSubject::BlankNode(node) => OwnedTerm::BlankNode(node),
            OwnedSubject::Triple(triple) => OwnedTerm::Triple(triple),
        }
    }
}

impl PartialEq<OwnedTerm> for Term<'_> {
    fn eq(&self, other: &OwnedTerm) -> bool {
        match (self, other) {
            (Term::NamedNode(n1), OwnedTerm::NamedNode(n2)) => n1 == n2,
            (Term::BlankNode(n1), OwnedTerm::BlankNode(n2)) => n1 == n2,
            (Term::Literal(n1), OwnedTerm::Literal(n2)) => n1 == n2,
            (Term::Triple(t1), OwnedTerm::Triple(t2)) => *t1 == &**t2,
            _ => false,
        }
    }
}

/// A [RDF triple](https://www.w3.org/TR/rdf11-concepts/#dfn-rdf-triple)
#[derive(Eq, PartialEq, Debug, Clone, Hash, Ord, PartialOrd)]
pub struct OwnedTriple {
    pub subject: OwnedSubject,
    pub predicate: OwnedNamedNode,
    pub object: OwnedTerm,
}

impl fmt::Display for OwnedTriple {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{} {} {}", self.subject, self.predicate, self.object)
    }
}

impl From<Triple<'_>> for OwnedTriple {
    fn from(t: Triple<'_>) -> Self {
        Self {
            subject: t.subject.into(),
            predicate: t.predicate.into(),
            object: t.object.into(),
        }
    }
}

impl PartialEq<OwnedTriple> for Triple<'_> {
    fn eq(&self, other: &OwnedTriple) -> bool {
        self.subject == other.subject
            && self.predicate == other.predicate
            && self.object == other.object
    }
}

/// A [RDF triple](https://www.w3.org/TR/rdf11-concepts/#dfn-rdf-triple)
#[derive(Eq, PartialEq, Debug, Clone, Hash)]
pub struct OwnedQuad {
    pub subject: OwnedSubject,
    pub predicate: OwnedNamedNode,
    pub object: OwnedTerm,
    pub graph_name: Option<OwnedGraphName>,
}

impl fmt::Display for OwnedQuad {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{} {} {}", self.subject, self.predicate, self.object)?;
        if let Some(n) = &self.graph_name {
            ' '.fmt(f)?;
            n.fmt(f)?;
        }
        Ok(())
    }
}

impl From<Triple<'_>> for OwnedQuad {
    fn from(t: Triple<'_>) -> Self {
        Self {
            subject: t.subject.into(),
            predicate: t.predicate.into(),
            object: t.object.into(),
            graph_name: None,
        }
    }
}

impl From<Quad<'_>> for OwnedQuad {
    fn from(q: Quad<'_>) -> Self {
        Self {
            subject: q.subject.into(),
            predicate: q.predicate.into(),
            object: q.object.into(),
            graph_name: q.graph_name.map(|g| g.into()),
        }
    }
}

impl PartialEq<OwnedQuad> for Quad<'_> {
    fn eq(&self, other: &OwnedQuad) -> bool {
        self.subject == other.subject
            && self.predicate == other.predicate
            && self.object == other.object
            && same_graph_name(&self.graph_name, &other.graph_name)
    }
}

#[derive(Default, Debug, Clone)]
pub struct OwnedDataset {
    inner: HashSet<OwnedQuad>,
}

impl OwnedDataset {
    pub fn insert(&mut self, t: impl Into<OwnedQuad>) {
        self.inner.insert(t.into());
    }

    pub fn iter(&self) -> impl Iterator<Item = &OwnedQuad> {
        self.inner.iter()
    }

    pub fn len(&self) -> usize {
        self.inner.len()
    }

    pub fn is_empty(&self) -> bool {
        self.inner.is_empty()
    }

    pub fn contains(&self, t: &OwnedQuad) -> bool {
        self.inner.contains(t)
    }

    pub fn triples_for_subject<'a, T>(
        &'a self,
        subject: &'a T,
    ) -> impl Iterator<Item = &'a OwnedQuad> + 'a
    where
        T: PartialEq<OwnedSubject> + 'a,
    {
        self.inner.iter().filter(move |t| subject == &t.subject)
    }

    pub fn triples_for_object<'a, T>(
        &'a self,
        object: &'a T,
    ) -> impl Iterator<Item = &'a OwnedQuad> + 'a
    where
        T: PartialEq<OwnedTerm> + 'a,
    {
        self.inner.iter().filter(move |t| *object == t.object)
    }

    pub fn object_for_subject_predicate<'a, T, U>(
        &'a self,
        subject: &T,
        predicate: &U,
    ) -> Option<&'a OwnedTerm>
    where
        T: PartialEq<OwnedSubject> + 'a,
        U: PartialEq<OwnedNamedNode> + 'a,
    {
        self.inner
            .iter()
            .filter(move |t| subject == &t.subject && predicate == &t.predicate)
            .map(|t| &t.object)
            .next()
    }

    pub fn subject_for_predicate_object<'a, T, U>(
        &'a self,
        predicate: &T,
        object: &U,
    ) -> Option<&'a OwnedSubject>
    where
        T: PartialEq<OwnedNamedNode> + 'a,
        U: PartialEq<OwnedTerm> + 'a,
    {
        self.inner
            .iter()
            .filter(move |t| predicate == &t.predicate && object == &t.object)
            .map(|t| &t.subject)
            .next()
    }
}

impl IntoIterator for OwnedDataset {
    type Item = OwnedQuad;
    type IntoIter = <HashSet<OwnedQuad> as IntoIterator>::IntoIter;

    fn into_iter(self) -> Self::IntoIter {
        self.inner.into_iter()
    }
}

impl FromIterator<OwnedQuad> for OwnedDataset {
    fn from_iter<I: IntoIterator<Item = OwnedQuad>>(iter: I) -> Self {
        Self {
            inner: HashSet::from_iter(iter),
        }
    }
}

impl fmt::Display for OwnedDataset {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        for t in &self.inner {
            writeln!(f, "{}", t)?;
        }
        Ok(())
    }
}

#[cfg(feature = "generalized")]
mod generalized {
    use super::*;
    use std::convert::{TryFrom, TryInto};

    impl<'a> TryFrom<GeneralizedQuad<'a>> for OwnedQuad {
        type Error = StrictRdfError;

        #[inline]
        fn try_from(other: GeneralizedQuad<'a>) -> Result<OwnedQuad, StrictRdfError> {
            Ok(OwnedQuad {
                subject: other.subject.try_into()?,
                predicate: other.predicate.try_into()?,
                object: other.object.try_into()?,
                graph_name: other
                    .graph_name
                    .map(GeneralizedTerm::try_into)
                    .transpose()?,
            })
        }
    }

    impl<'a> TryFrom<GeneralizedTerm<'a>> for OwnedSubject {
        type Error = StrictRdfError;

        #[inline]
        fn try_from(other: GeneralizedTerm<'a>) -> Result<OwnedSubject, StrictRdfError> {
            match other {
                GeneralizedTerm::NamedNode(inner) => Ok(OwnedSubject::NamedNode(inner.into())),
                GeneralizedTerm::BlankNode(inner) => Ok(OwnedSubject::BlankNode(inner.into())),
                GeneralizedTerm::Literal(_) => Err(StrictRdfError {
                    message: "Literal cannot be used as a subject",
                }),
                GeneralizedTerm::Variable(_) => Err(StrictRdfError {
                    message: "Variable cannot be converted to strict RDF term",
                }),
                GeneralizedTerm::Triple(triple) => {
                    Ok(OwnedSubject::Triple(Box::new(OwnedTriple {
                        subject: triple[0].try_into()?,
                        predicate: triple[1].try_into()?,
                        object: triple[2].try_into()?,
                    })))
                }
            }
        }
    }

    impl<'a> TryFrom<GeneralizedTerm<'a>> for OwnedNamedNode {
        type Error = StrictRdfError;

        #[inline]
        fn try_from(other: GeneralizedTerm<'a>) -> Result<OwnedNamedNode, StrictRdfError> {
            match other {
                GeneralizedTerm::NamedNode(inner) => Ok(inner.into()),
                GeneralizedTerm::BlankNode(_) => Err(StrictRdfError {
                    message: "BlankNode cannot be used as a predicate",
                }),
                GeneralizedTerm::Literal(_) => Err(StrictRdfError {
                    message: "Literal cannot be used as a predicate",
                }),
                GeneralizedTerm::Variable(_) => Err(StrictRdfError {
                    message: "Variable cannot be converted to strict RDF term",
                }),
                GeneralizedTerm::Triple(_) => Err(StrictRdfError {
                    message: "Quoted triple cannot be used as a predicate",
                }),
            }
        }
    }

    impl<'a> TryFrom<GeneralizedTerm<'a>> for OwnedTerm {
        type Error = StrictRdfError;

        #[inline]
        fn try_from(other: GeneralizedTerm<'a>) -> Result<OwnedTerm, StrictRdfError> {
            match other {
                GeneralizedTerm::NamedNode(inner) => Ok(OwnedTerm::NamedNode(inner.into())),
                GeneralizedTerm::BlankNode(inner) => Ok(OwnedTerm::BlankNode(inner.into())),
                GeneralizedTerm::Literal(inner) => Ok(OwnedTerm::Literal(inner.into())),
                GeneralizedTerm::Variable(_) => Err(StrictRdfError {
                    message: "Variable cannot be converted to struct RDF term",
                }),
                GeneralizedTerm::Triple(triple) => Ok(OwnedTerm::Triple(Box::new(OwnedTriple {
                    subject: triple[0].try_into()?,
                    predicate: triple[1].try_into()?,
                    object: triple[2].try_into()?,
                }))),
            }
        }
    }

    impl<'a> TryFrom<GeneralizedTerm<'a>> for OwnedGraphName {
        type Error = StrictRdfError;

        #[inline]
        fn try_from(other: GeneralizedTerm<'a>) -> Result<OwnedGraphName, StrictRdfError> {
            match other {
                GeneralizedTerm::NamedNode(inner) => Ok(OwnedGraphName::NamedNode(inner.into())),
                GeneralizedTerm::BlankNode(inner) => Ok(OwnedGraphName::BlankNode(inner.into())),
                GeneralizedTerm::Literal(_) => Err(StrictRdfError {
                    message: "Literal cannot be used as a graph name",
                }),
                GeneralizedTerm::Variable(_) => Err(StrictRdfError {
                    message: "Variable cannot be converted to strict RDF term",
                }),
                GeneralizedTerm::Triple(_) => Err(StrictRdfError {
                    message: "Quoted triple cannot be used as a graph name",
                }),
            }
        }
    }
}
