use rio_api::model::*;

#[derive(Eq, PartialEq, Ord, PartialOrd, Debug, Clone, Hash)]
pub struct OwnedNamedNode {
    pub iri: String,
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

#[derive(Eq, PartialEq, Ord, PartialOrd, Debug, Clone, Hash)]
pub struct OwnedBlankNode {
    pub id: String,
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

#[derive(Eq, PartialEq, Ord, PartialOrd, Debug, Clone, Hash)]
enum OwnedLiteral {
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

#[derive(Eq, PartialEq, Ord, PartialOrd, Debug, Clone, Hash)]
pub enum OwnedSubject {
    NamedNode(OwnedNamedNode),
    BlankNode(OwnedBlankNode),
}

impl std::convert::TryFrom<Subject<'_>> for OwnedSubject {
    type Error = crate::RdfXmlError;
    fn try_from(t: Subject<'_>) -> Result<Self, Self::Error> {
        match t {
            Subject::NamedNode(n) => Ok(OwnedSubject::NamedNode(n.into())),
            Subject::BlankNode(n) => Ok(OwnedSubject::BlankNode(n.into())),
            _ => Err(crate::RdfXmlError::msg(
                "RDF/XML only supports named or blank subject",
            )),
        }
    }
}

impl<'a> From<&'a OwnedSubject> for Subject<'a> {
    fn from(t: &'a OwnedSubject) -> Self {
        match t {
            OwnedSubject::NamedNode(n) => Subject::NamedNode(n.into()),
            OwnedSubject::BlankNode(n) => Subject::BlankNode(n.into()),
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
enum OwnedTerm {
    NamedNode(OwnedNamedNode),
    BlankNode(OwnedBlankNode),
    Literal(OwnedLiteral),
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

impl From<OwnedSubject> for OwnedTerm {
    fn from(resource: OwnedSubject) -> Self {
        match resource {
            OwnedSubject::NamedNode(node) => OwnedTerm::NamedNode(node),
            OwnedSubject::BlankNode(node) => OwnedTerm::BlankNode(node),
        }
    }
}
