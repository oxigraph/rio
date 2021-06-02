//! Data structures for [RDF 1.1 Concepts](https://www.w3.org/TR/rdf11-concepts/) like IRI, literal or triples.
//!
//! If the `sophia` feature is enabled, the types defined in [`model`](super::model) implement the appropriate trait from [Sophia]( https://crates.io/crates/sophia_api).

#[cfg(feature = "generalized")]
pub use crate::generalized::model::*;
use std::fmt;
use std::fmt::Write;

/// An RDF [IRI](https://www.w3.org/TR/rdf11-concepts/#dfn-iri).
///
/// The default string formatter is returning an N-Triples, Turtle and SPARQL compatible representation.
///
/// ```
/// use rio_api::model::NamedNode;
///
/// assert_eq!(
///     "<http://example.com/foo>",
///     NamedNode { iri: "http://example.com/foo" }.to_string()
/// )
/// ```
#[derive(Eq, PartialEq, Ord, PartialOrd, Debug, Clone, Copy, Hash)]
pub struct NamedNode<'a> {
    /// The [IRI](https://www.w3.org/TR/rdf11-concepts/#dfn-iri) itself.
    pub iri: &'a str,
}

impl<'a> fmt::Display for NamedNode<'a> {
    #[inline]
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "<{}>", self.iri)
    }
}

/// An RDF [blank node](https://www.w3.org/TR/rdf11-concepts/#dfn-blank-node).
///
///
/// The default string formatter is returning an N-Triples, Turtle and SPARQL compatible representation.
///
/// ```
/// use rio_api::model::BlankNode;
///
/// assert_eq!(
///     "_:a1",
///     BlankNode { id: "a1" }.to_string()
/// )
/// ```
#[derive(Eq, PartialEq, Debug, Clone, Copy, Hash)]
pub struct BlankNode<'a> {
    /// The [blank node identifier](https://www.w3.org/TR/rdf11-concepts/#dfn-blank-node-identifier).
    pub id: &'a str,
}

impl<'a> fmt::Display for BlankNode<'a> {
    #[inline]
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "_:{}", self.id)
    }
}

/// An RDF [literal](https://www.w3.org/TR/rdf11-concepts/#dfn-literal).
///
/// The default string formatter is returning an N-Triples, Turtle and SPARQL compatible representation.
///
/// The language tags should be lowercased  [as suggested by the RDF specification](https://www.w3.org/TR/rdf11-concepts/#dfn-language-tagged-string).
///
/// ```
/// use rio_api::model::NamedNode;
/// use rio_api::model::Literal;
///
/// assert_eq!(
///     "\"foo\\nbar\"",
///     Literal::Simple { value: "foo\nbar" }.to_string()
/// );
///
/// assert_eq!(
///     "\"1999-01-01\"^^<http://www.w3.org/2001/XMLSchema#date>",
///     Literal::Typed { value: "1999-01-01", datatype: NamedNode {iri: "http://www.w3.org/2001/XMLSchema#date" }}.to_string()
/// );
///
/// assert_eq!(
///     "\"foo\"@en",
///     Literal::LanguageTaggedString { value: "foo", language: "en" }.to_string()
/// );
/// ```
#[derive(Eq, PartialEq, Debug, Clone, Copy, Hash)]
pub enum Literal<'a> {
    /// A [simple literal](https://www.w3.org/TR/rdf11-concepts/#dfn-simple-literal) without datatype or language form.
    Simple {
        /// The [lexical form](https://www.w3.org/TR/rdf11-concepts/#dfn-lexical-form).
        value: &'a str,
    },
    /// A [language-tagged string](https://www.w3.org/TR/rdf11-concepts/#dfn-language-tagged-string)
    LanguageTaggedString {
        /// The [lexical form](https://www.w3.org/TR/rdf11-concepts/#dfn-lexical-form).
        value: &'a str,
        /// The [language tag](https://www.w3.org/TR/rdf11-concepts/#dfn-language-tag).
        language: &'a str,
    },
    /// A literal with an explicit datatype
    Typed {
        /// The [lexical form](https://www.w3.org/TR/rdf11-concepts/#dfn-lexical-form).
        value: &'a str,
        /// The [datatype IRI](https://www.w3.org/TR/rdf11-concepts/#dfn-datatype-iri).
        datatype: NamedNode<'a>,
    },
}

impl<'a> fmt::Display for Literal<'a> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Literal::Simple { value } => fmt_quoted_str(value, f),
            Literal::LanguageTaggedString { value, language } => {
                fmt_quoted_str(value, f)?;
                write!(f, "@{}", language)
            }
            Literal::Typed { value, datatype } => {
                fmt_quoted_str(value, f)?;
                write!(f, "^^{}", datatype)
            }
        }
    }
}

/// The union of [IRIs](https://www.w3.org/TR/rdf11-concepts/#dfn-iri) and [blank nodes](https://www.w3.org/TR/rdf11-concepts/#dfn-blank-node).
///
/// The default string formatter is returning an N-Triples, Turtle and SPARQL compatible representation.
#[derive(Eq, PartialEq, Debug, Clone, Copy, Hash)]
pub enum NamedOrBlankNode<'a> {
    NamedNode(NamedNode<'a>),
    BlankNode(BlankNode<'a>),
}

impl<'a> fmt::Display for NamedOrBlankNode<'a> {
    #[inline]
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            NamedOrBlankNode::NamedNode(node) => node.fmt(f),
            NamedOrBlankNode::BlankNode(node) => node.fmt(f),
        }
    }
}

impl<'a> From<NamedNode<'a>> for NamedOrBlankNode<'a> {
    #[inline]
    fn from(node: NamedNode<'a>) -> Self {
        NamedOrBlankNode::NamedNode(node)
    }
}

impl<'a> From<BlankNode<'a>> for NamedOrBlankNode<'a> {
    #[inline]
    fn from(node: BlankNode<'a>) -> Self {
        NamedOrBlankNode::BlankNode(node)
    }
}

/// An RDF [term](https://www.w3.org/TR/rdf11-concepts/#dfn-rdf-term).
///
/// It is the union of [IRIs](https://www.w3.org/TR/rdf11-concepts/#dfn-iri), [blank nodes](https://www.w3.org/TR/rdf11-concepts/#dfn-blank-node) and [literals](https://www.w3.org/TR/rdf11-concepts/#dfn-literal).
///
/// The default string formatter is returning an N-Triples, Turtle and SPARQL compatible representation.
#[derive(Eq, PartialEq, Debug, Clone, Copy, Hash)]
pub enum Term<'a> {
    NamedNode(NamedNode<'a>),
    BlankNode(BlankNode<'a>),
    Literal(Literal<'a>),
}

impl<'a> fmt::Display for Term<'a> {
    #[inline]
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Term::NamedNode(node) => node.fmt(f),
            Term::BlankNode(node) => node.fmt(f),
            Term::Literal(literal) => literal.fmt(f),
        }
    }
}

impl<'a> From<NamedNode<'a>> for Term<'a> {
    #[inline]
    fn from(node: NamedNode<'a>) -> Self {
        Term::NamedNode(node)
    }
}

impl<'a> From<BlankNode<'a>> for Term<'a> {
    #[inline]
    fn from(node: BlankNode<'a>) -> Self {
        Term::BlankNode(node)
    }
}

impl<'a> From<Literal<'a>> for Term<'a> {
    #[inline]
    fn from(literal: Literal<'a>) -> Self {
        Term::Literal(literal)
    }
}

impl<'a> From<NamedOrBlankNode<'a>> for Term<'a> {
    #[inline]
    fn from(resource: NamedOrBlankNode<'a>) -> Self {
        match resource {
            NamedOrBlankNode::NamedNode(node) => Term::NamedNode(node),
            NamedOrBlankNode::BlankNode(node) => Term::BlankNode(node),
        }
    }
}

/// A [RDF triple](https://www.w3.org/TR/rdf11-concepts/#dfn-rdf-triple).
///
/// The default string formatter is returning a N-Triples, Turtle and SPARQL compatible representation.
///
/// ```
/// use rio_api::model::NamedNode;
/// use rio_api::model::Triple;
///
/// assert_eq!(
///     "<http://example.com/foo> <http://schema.org/sameAs> <http://example.com/foo> .",
///     Triple {
///         subject: NamedNode { iri: "http://example.com/foo" }.into(),
///         predicate: NamedNode { iri: "http://schema.org/sameAs" },
///         object: NamedNode { iri: "http://example.com/foo" }.into(),
///     }.to_string()
/// )
/// ```
#[derive(Eq, PartialEq, Debug, Clone, Hash)]
pub struct Triple<'a> {
    pub subject: NamedOrBlankNode<'a>,
    pub predicate: NamedNode<'a>,
    pub object: Term<'a>,
}

impl<'a> fmt::Display for Triple<'a> {
    #[inline]
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{} {} {} .", self.subject, self.predicate, self.object)
    }
}

/// A [RDF triple](https://www.w3.org/TR/rdf11-concepts/#dfn-rdf-triple) in a [RDF dataset](https://www.w3.org/TR/rdf11-concepts/#dfn-rdf-dataset).
///
/// The default string formatter is returning a N-Quads representation.
///
/// ```
/// use rio_api::model::NamedNode;
/// use rio_api::model::Quad;
///
/// assert_eq!(
///     "<http://example.com/foo> <http://schema.org/sameAs> <http://example.com/foo> <http://example.com/> .",
///     Quad {
///         subject: NamedNode { iri: "http://example.com/foo" }.into(),
///         predicate: NamedNode { iri: "http://schema.org/sameAs" },
///         object: NamedNode { iri: "http://example.com/foo" }.into(),
///         graph_name: Some(NamedNode { iri: "http://example.com/" }.into()),
///     }.to_string()
/// )
/// ```
#[derive(Eq, PartialEq, Debug, Clone, Hash)]
pub struct Quad<'a> {
    pub subject: NamedOrBlankNode<'a>,
    pub predicate: NamedNode<'a>,
    pub object: Term<'a>,
    pub graph_name: Option<NamedOrBlankNode<'a>>,
}

impl<'a> fmt::Display for Quad<'a> {
    #[inline]
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self.graph_name {
            Some(graph_name) => write!(
                f,
                "{} {} {} {} .",
                self.subject, self.predicate, self.object, graph_name
            ),
            None => write!(f, "{} {} {} .", self.subject, self.predicate, self.object),
        }
    }
}

#[inline]
fn fmt_quoted_str(string: &str, f: &mut fmt::Formatter<'_>) -> fmt::Result {
    f.write_char('"')?;
    for c in string.chars() {
        match c {
            '\n' => f.write_str("\\n"),
            '\r' => f.write_str("\\r"),
            '"' => f.write_str("\\\""),
            '\\' => f.write_str("\\\\"),
            c => f.write_char(c),
        }?;
    }
    f.write_char('"')
}
