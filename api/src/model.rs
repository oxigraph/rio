//! Data structures for [RDF 1.1](https://www.w3.org/TR/rdf11-concepts/) and [RDF-star](https://w3c.github.io/rdf-star/cg-spec/) concepts like IRI, literal or triples.
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
    #[inline]
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

/// A restriction of [Term] that can be used as the [subject of an RDF triple](https://www.w3.org/TR/rdf11-concepts/#dfn-subject).
///
/// The default string formatter is returning an N-Triples, Turtle and SPARQL compatible representation.
#[derive(Eq, PartialEq, Debug, Clone, Copy, Hash)]
pub enum Subject<'a> {
    NamedNode(NamedNode<'a>),
    BlankNode(BlankNode<'a>),
    /// Rio does support [RDF-star](https://w3c.github.io/rdf-star/cg-spec/2021-07-01.html#dfn-triple), which allows triples to be the subject of other triples.
    Triple(&'a Triple<'a>),
}

impl<'a> fmt::Display for Subject<'a> {
    #[inline]
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Subject::NamedNode(node) => node.fmt(f),
            Subject::BlankNode(node) => node.fmt(f),
            Subject::Triple(triple) => write!(f, "<< {} >>", triple),
        }
    }
}

impl<'a> From<NamedNode<'a>> for Subject<'a> {
    #[inline]
    fn from(node: NamedNode<'a>) -> Self {
        Subject::NamedNode(node)
    }
}

impl<'a> From<BlankNode<'a>> for Subject<'a> {
    #[inline]
    fn from(node: BlankNode<'a>) -> Self {
        Subject::BlankNode(node)
    }
}

impl<'a> From<&'a Triple<'a>> for Subject<'a> {
    #[inline]
    fn from(triple: &'a Triple<'a>) -> Self {
        Subject::Triple(triple)
    }
}

impl<'a> From<GraphName<'a>> for Subject<'a> {
    #[inline]
    fn from(node: GraphName<'a>) -> Self {
        match node {
            GraphName::BlankNode(node) => Subject::BlankNode(node),
            GraphName::NamedNode(node) => Subject::NamedNode(node),
        }
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
    /// Rio does support [RDF-star](https://w3c.github.io/rdf-star/cg-spec/2021-07-01.html#dfn-rdf-star-terms), which allows triples to be terms inside of other triples.
    Triple(&'a Triple<'a>),
}

impl<'a> fmt::Display for Term<'a> {
    #[inline]
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Term::NamedNode(node) => node.fmt(f),
            Term::BlankNode(node) => node.fmt(f),
            Term::Literal(literal) => literal.fmt(f),
            Term::Triple(triple) => write!(f, "<< {} >>", triple),
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

impl<'a> From<&'a Triple<'a>> for Term<'a> {
    #[inline]
    fn from(triple: &'a Triple<'a>) -> Self {
        Term::Triple(triple)
    }
}

impl<'a> From<Subject<'a>> for Term<'a> {
    #[inline]
    fn from(resource: Subject<'a>) -> Self {
        match resource {
            Subject::NamedNode(node) => Term::NamedNode(node),
            Subject::BlankNode(node) => Term::BlankNode(node),
            Subject::Triple(triple) => Term::Triple(triple),
        }
    }
}

impl<'a> From<GraphName<'a>> for Term<'a> {
    #[inline]
    fn from(resource: GraphName<'a>) -> Self {
        match resource {
            GraphName::NamedNode(node) => Term::NamedNode(node),
            GraphName::BlankNode(node) => Term::BlankNode(node),
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
///     "<http://example.com/foo> <http://schema.org/sameAs> <http://example.com/foo>",
///     Triple {
///         subject: NamedNode { iri: "http://example.com/foo" }.into(),
///         predicate: NamedNode { iri: "http://schema.org/sameAs" },
///         object: NamedNode { iri: "http://example.com/foo" }.into(),
///     }.to_string()
/// )
/// ```
#[derive(Eq, PartialEq, Debug, Clone, Copy, Hash)]
pub struct Triple<'a> {
    pub subject: Subject<'a>,
    pub predicate: NamedNode<'a>,
    pub object: Term<'a>,
}

impl<'a> fmt::Display for Triple<'a> {
    #[inline]
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{} {} {}", self.subject, self.predicate, self.object)
    }
}

/// A restriction of [Term] that can be used in the graph name position.
///
/// The default string formatter is returning an N-Triples, Turtle and SPARQL compatible representation.
#[derive(Eq, PartialEq, Debug, Clone, Copy, Hash)]
pub enum GraphName<'a> {
    NamedNode(NamedNode<'a>),
    BlankNode(BlankNode<'a>),
}

impl<'a> fmt::Display for GraphName<'a> {
    #[inline]
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            GraphName::NamedNode(node) => node.fmt(f),
            GraphName::BlankNode(node) => node.fmt(f),
        }
    }
}

impl<'a> From<NamedNode<'a>> for GraphName<'a> {
    #[inline]
    fn from(node: NamedNode<'a>) -> Self {
        GraphName::NamedNode(node)
    }
}

impl<'a> From<BlankNode<'a>> for GraphName<'a> {
    #[inline]
    fn from(node: BlankNode<'a>) -> Self {
        GraphName::BlankNode(node)
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
///     "<http://example.com/foo> <http://schema.org/sameAs> <http://example.com/foo> <http://example.com/>",
///     Quad {
///         subject: NamedNode { iri: "http://example.com/foo" }.into(),
///         predicate: NamedNode { iri: "http://schema.org/sameAs" },
///         object: NamedNode { iri: "http://example.com/foo" }.into(),
///         graph_name: Some(NamedNode { iri: "http://example.com/" }.into()),
///     }.to_string()
/// )
/// ```
#[derive(Eq, PartialEq, Debug, Clone, Copy, Hash)]
pub struct Quad<'a> {
    pub subject: Subject<'a>,
    pub predicate: NamedNode<'a>,
    pub object: Term<'a>,
    pub graph_name: Option<GraphName<'a>>,
}

impl<'a> fmt::Display for Quad<'a> {
    #[inline]
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        if let Some(graph_name) = self.graph_name {
            write!(
                f,
                "{} {} {} {}",
                self.subject, self.predicate, self.object, graph_name
            )
        } else {
            write!(f, "{} {} {}", self.subject, self.predicate, self.object)
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
