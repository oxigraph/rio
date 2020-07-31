//! Provide implementation of Sophia traits for Rio types.
use crate::model::*;
use sophia_api::ns::{rdf, xsd};
#[cfg(feature = "generalized")]
use sophia_api::quad::Quad as SophiaQuad;
use sophia_api::term::{RawValue, SimpleIri, TTerm, TermKind};

impl<'a> TTerm for NamedNode<'a> {
    #[inline]
    fn kind(&self) -> TermKind {
        TermKind::Iri
    }

    #[inline]
    fn value_raw(&self) -> RawValue<'_> {
        self.iri.into()
    }

    #[inline]
    fn as_dyn(&self) -> &dyn TTerm {
        self
    }
}

impl<'a> From<&NamedNode<'a>> for SimpleIri<'a> {
    #[inline]
    fn from(other: &NamedNode<'a>) -> SimpleIri<'a> {
        SimpleIri::new_unchecked(other.iri, None)
    }
}

impl<'a> TTerm for BlankNode<'a> {
    #[inline]
    fn kind(&self) -> TermKind {
        TermKind::BlankNode
    }

    #[inline]
    fn value_raw(&self) -> RawValue<'_> {
        self.id.into()
    }

    #[inline]
    fn as_dyn(&self) -> &dyn TTerm {
        self
    }
}

impl<'a> TTerm for Literal<'a> {
    #[inline]
    fn kind(&self) -> TermKind {
        TermKind::Literal
    }
    #[inline]
    fn value_raw(&self) -> RawValue<'_> {
        match self {
            Literal::Simple { value } => *value,
            Literal::LanguageTaggedString { value, .. } => *value,
            Literal::Typed { value, .. } => *value,
        }
        .into()
    }

    #[inline]
    fn datatype(&self) -> Option<SimpleIri<'_>> {
        match self {
            Literal::Simple { .. } => Some(xsd::string),
            Literal::LanguageTaggedString { .. } => Some(rdf::langString),
            Literal::Typed { datatype, .. } => Some(datatype.into()),
        }
    }

    #[inline]
    fn language(&self) -> Option<&str> {
        if let Literal::LanguageTaggedString { language, .. } = self {
            Some(language)
        } else {
            None
        }
    }

    #[inline]
    fn as_dyn(&self) -> &dyn TTerm {
        self
    }
}

impl<'a> TTerm for NamedOrBlankNode<'a> {
    #[inline]
    fn kind(&self) -> TermKind {
        match self {
            NamedOrBlankNode::NamedNode(_) => TermKind::Iri,
            NamedOrBlankNode::BlankNode(_) => TermKind::BlankNode,
        }
    }

    #[inline]
    fn value_raw(&self) -> RawValue<'_> {
        match self {
            NamedOrBlankNode::NamedNode(n) => n.value_raw(),
            NamedOrBlankNode::BlankNode(n) => n.value_raw(),
        }
    }

    #[inline]
    fn as_dyn(&self) -> &dyn TTerm {
        self
    }
}

impl<'a> TTerm for Term<'a> {
    #[inline]
    fn kind(&self) -> TermKind {
        match self {
            Term::NamedNode(_) => TermKind::Iri,
            Term::BlankNode(_) => TermKind::BlankNode,
            Term::Literal(_) => TermKind::Literal,
        }
    }

    #[inline]
    fn value_raw(&self) -> RawValue<'_> {
        match self {
            Term::NamedNode(n) => n.value_raw(),
            Term::BlankNode(n) => n.value_raw(),
            Term::Literal(l) => l.value_raw(),
        }
    }

    #[inline]
    fn datatype(&self) -> Option<SimpleIri<'_>> {
        match self {
            Term::Literal(l) => l.datatype(),
            _ => None,
        }
    }

    #[inline]
    fn language(&self) -> Option<&str> {
        match self {
            Term::Literal(l) => l.language(),
            _ => None,
        }
    }

    #[inline]
    fn as_dyn(&self) -> &dyn TTerm {
        self
    }
}

#[cfg(feature = "generalized")]
impl<'a> TTerm for Variable<'a> {
    #[inline]
    fn kind(&self) -> TermKind {
        TermKind::Variable
    }

    #[inline]
    fn value_raw(&self) -> RawValue<'_> {
        self.name.into()
    }

    #[inline]
    fn as_dyn(&self) -> &dyn TTerm {
        self
    }
}

#[cfg(feature = "generalized")]
impl<'a> TTerm for GeneralizedTerm<'a> {
    #[inline]
    fn kind(&self) -> TermKind {
        match self {
            GeneralizedTerm::NamedNode(_) => TermKind::Iri,
            GeneralizedTerm::BlankNode(_) => TermKind::BlankNode,
            GeneralizedTerm::Literal(_) => TermKind::Literal,
            GeneralizedTerm::Variable(_) => TermKind::Variable,
        }
    }

    #[inline]
    fn value_raw(&self) -> RawValue<'_> {
        match self {
            GeneralizedTerm::NamedNode(n) => n.value_raw(),
            GeneralizedTerm::BlankNode(n) => n.value_raw(),
            GeneralizedTerm::Literal(l) => l.value_raw(),
            GeneralizedTerm::Variable(v) => v.value_raw(),
        }
    }

    #[inline]
    fn datatype(&self) -> Option<SimpleIri<'_>> {
        match self {
            GeneralizedTerm::Literal(l) => l.datatype(),
            _ => None,
        }
    }

    #[inline]
    fn language(&self) -> Option<&str> {
        match self {
            GeneralizedTerm::Literal(l) => l.language(),
            _ => None,
        }
    }

    #[inline]
    fn as_dyn(&self) -> &dyn TTerm {
        self
    }
}

// NB: Triple and Quad can not implement sophia's counterparts,
// because they have different types for s, p and o.

#[cfg(feature = "generalized")]
impl<'a> SophiaQuad for GeneralizedQuad<'a> {
    type Term = GeneralizedTerm<'a>;

    fn s(&self) -> &Self::Term {
        &self.subject
    }

    fn p(&self) -> &Self::Term {
        &self.predicate
    }

    fn o(&self) -> &Self::Term {
        &self.object
    }

    fn g(&self) -> Option<&Self::Term> {
        self.graph_name.as_ref()
    }
}
