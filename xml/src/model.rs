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

impl From<NamedOrBlankNode<'_>> for OwnedNamedOrBlankNode {
    fn from(t: NamedOrBlankNode<'_>) -> Self {
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
enum OwnedTerm {
    NamedNode(OwnedNamedNode),
    BlankNode(OwnedBlankNode),
    Literal(OwnedLiteral),
}

impl From<Term<'_>> for OwnedTerm {
    fn from(t: Term<'_>) -> Self {
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
