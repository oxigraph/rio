//! This module contains extensions for generalized RDF.
//! Its elements are re-exported by ``model`` and ``parser``, respectively.

/// Data structures for generalized [RDF 1.1 Concepts](https://www.w3.org/TR/rdf11-concepts/),
/// allowing variables, and any kind of node in any Triple/Quad position.
pub mod model {
    use std::convert;
    use std::convert::TryInto;
    use std::error::Error;
    use std::fmt;

    pub use crate::model::*;

    /// A SPARQL [variable](https://www.w3.org/TR/2013/REC-sparql11-query-20130321/#QSynVariables).
    ///
    /// The default string formatter is returning a SPARQL compatible representation.
    ///
    /// ```
    /// use rio_api::model::Variable;
    ///
    /// assert_eq!(
    ///     "?foobar",
    ///     Variable { name: "foobar" }.to_string()
    /// )
    /// ```
    #[derive(Eq, PartialEq, Ord, PartialOrd, Debug, Clone, Copy, Hash)]
    pub struct Variable<'a> {
        /// The name of  the [variable](https://www.w3.org/TR/2013/REC-sparql11-query-20130321/#QSynVariables) itself.
        pub name: &'a str,
    }

    impl<'a> fmt::Display for Variable<'a> {
        fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
            write!(f, "?{}", self.name)
        }
    }

    //

    /// A generalized RDF [term](https://www.w3.org/TR/rdf11-concepts/#dfn-rdf-term).
    ///
    /// It is the union of
    /// * [IRIs](https://www.w3.org/TR/rdf11-concepts/#dfn-iri),
    /// * [blank nodes](https://www.w3.org/TR/rdf11-concepts/#dfn-blank-node)
    /// * [literals](https://www.w3.org/TR/rdf11-concepts/#dfn-literal) and
    /// * [variable](https://www.w3.org/TR/2013/REC-sparql11-query-20130321/#QSynVariables).
    ///
    /// The default string formatter is returning a N-Triples, Turtle and SPARQL compatible representation.
    #[derive(Eq, PartialEq, Debug, Clone, Copy, Hash)]
    pub enum GeneralizedTerm<'a> {
        NamedNode(NamedNode<'a>),
        BlankNode(BlankNode<'a>),
        Literal(Literal<'a>),
        Variable(Variable<'a>),
    }

    impl<'a> From<NamedNode<'a>> for GeneralizedTerm<'a> {
        fn from(other: NamedNode<'a>) -> GeneralizedTerm<'a> {
            GeneralizedTerm::NamedNode(other)
        }
    }

    impl<'a> From<BlankNode<'a>> for GeneralizedTerm<'a> {
        fn from(other: BlankNode<'a>) -> GeneralizedTerm<'a> {
            GeneralizedTerm::BlankNode(other)
        }
    }

    impl<'a> From<Literal<'a>> for GeneralizedTerm<'a> {
        fn from(other: Literal<'a>) -> GeneralizedTerm<'a> {
            GeneralizedTerm::Literal(other)
        }
    }

    impl<'a> From<Variable<'a>> for GeneralizedTerm<'a> {
        fn from(other: Variable<'a>) -> GeneralizedTerm<'a> {
            GeneralizedTerm::Variable(other)
        }
    }

    impl<'a> From<NamedOrBlankNode<'a>> for GeneralizedTerm<'a> {
        fn from(other: NamedOrBlankNode<'a>) -> GeneralizedTerm<'a> {
            match other {
                NamedOrBlankNode::NamedNode(inner) => GeneralizedTerm::NamedNode(inner),
                NamedOrBlankNode::BlankNode(inner) => GeneralizedTerm::BlankNode(inner),
            }
        }
    }

    impl<'a> From<Term<'a>> for GeneralizedTerm<'a> {
        fn from(other: Term<'a>) -> GeneralizedTerm<'a> {
            match other {
                Term::NamedNode(inner) => GeneralizedTerm::NamedNode(inner),
                Term::BlankNode(inner) => GeneralizedTerm::BlankNode(inner),
                Term::Literal(inner) => GeneralizedTerm::Literal(inner),
            }
        }
    }

    impl<'a> convert::TryFrom<GeneralizedTerm<'a>> for NamedNode<'a> {
        type Error = StrictRdfError;
        fn try_from(other: GeneralizedTerm<'a>) -> Result<NamedNode<'a>, StrictRdfError> {
            match other {
                GeneralizedTerm::NamedNode(inner) => Ok(inner),
                GeneralizedTerm::BlankNode(_) => Err("Blankd node can not be used as predicate".into()),
                GeneralizedTerm::Literal(_) => Err("Literal can not be used as predicate".into()),
                GeneralizedTerm::Variable(_) => Err("Variable can not be converted to Term".into()),
            }
        }
    }

    impl<'a> convert::TryFrom<GeneralizedTerm<'a>> for NamedOrBlankNode<'a> {
        type Error = StrictRdfError;
        fn try_from(other: GeneralizedTerm<'a>) -> Result<NamedOrBlankNode<'a>, StrictRdfError> {
            match other {
                GeneralizedTerm::NamedNode(inner) => Ok(NamedOrBlankNode::NamedNode(inner)),
                GeneralizedTerm::BlankNode(inner) => Ok(NamedOrBlankNode::BlankNode(inner)),
                GeneralizedTerm::Literal(_) => Err("Literal can not be used a subject".into()),
                GeneralizedTerm::Variable(_) => Err("Variable can not be converted to Term".into()),
            }
        }
    }

    impl<'a> convert::TryFrom<GeneralizedTerm<'a>> for Term<'a> {
        type Error = StrictRdfError;
        fn try_from(other: GeneralizedTerm<'a>) -> Result<Term<'a>, StrictRdfError> {
            match other {
                GeneralizedTerm::NamedNode(inner) => Ok(Term::NamedNode(inner)),
                GeneralizedTerm::BlankNode(inner) => Ok(Term::BlankNode(inner)),
                GeneralizedTerm::Literal(inner) => Ok(Term::Literal(inner)),
                GeneralizedTerm::Variable(_) => Err("Variable can not be converted to Term".into()),
            }
        }
    }

    impl<'a> fmt::Display for GeneralizedTerm<'a> {
        fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
            match self {
                GeneralizedTerm::NamedNode(node) => node.fmt(f),
                GeneralizedTerm::BlankNode(node) => node.fmt(f),
                GeneralizedTerm::Literal(literal) => literal.fmt(f),
                GeneralizedTerm::Variable(variable) => variable.fmt(f),
            }
        }
    }

    //

    /// A generalized [RDF triple](https://www.w3.org/TR/rdf11-concepts/#dfn-rdf-triple) in a [RDF dataset](https://www.w3.org/TR/rdf11-concepts/#dfn-rdf-dataset).
    ///
    /// The default string formatter is returning a SPARQL representation.
    ///
    /// ```
    /// use rio_api::model::{GeneralizedQuad, Variable};
    ///
    /// assert_eq!(
    ///     "?s ?p ?o .",
    ///     GeneralizedQuad {
    ///         subject: Variable { name: "s" }.into(),
    ///         predicate: Variable { name: "p" }.into(),
    ///         object: Variable { name: "o" }.into(),
    ///         graph_name: None,
    ///     }.to_string()
    /// );

    /// assert_eq!(
    ///     "GRAPH ?g { ?s ?p ?o .}",
    ///     GeneralizedQuad {
    ///         subject: Variable { name: "s" }.into(),
    ///         predicate: Variable { name: "p" }.into(),
    ///         object: Variable { name: "o" }.into(),
    ///         graph_name: Some(Variable { name: "g" }.into()),
    ///     }.to_string()
    /// );
    /// ```
    #[derive(Eq, PartialEq, Debug, Clone, Hash)]
    pub struct GeneralizedQuad<'a> {
        pub subject: GeneralizedTerm<'a>,
        pub predicate: GeneralizedTerm<'a>,
        pub object: GeneralizedTerm<'a>,
        pub graph_name: Option<GeneralizedTerm<'a>>,
    }

    impl<'a> From<Quad<'a>> for GeneralizedQuad<'a> {
        fn from(other: Quad<'a>) -> GeneralizedQuad<'a> {
            GeneralizedQuad {
                subject: other.subject.into(),
                predicate: other.predicate.into(),
                object: other.object.into(),
                graph_name: other.graph_name.map(GeneralizedTerm::from),
            }
        }
    }

    impl<'a> convert::TryFrom<GeneralizedQuad<'a>> for Quad<'a> {
        type Error = StrictRdfError;

        fn try_from(other: GeneralizedQuad<'a>) -> Result<Quad<'a>, StrictRdfError> {
            Ok(Quad {
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

    impl<'a> convert::TryFrom<GeneralizedQuad<'a>> for Triple<'a> {
        type Error = StrictRdfError;

        fn try_from(other: GeneralizedQuad<'a>) -> Result<Triple<'a>, StrictRdfError> {
            match other.graph_name {
                Some(_) => Err("Quad in named graph can not be converted to Triple".into()),
                None => Ok(Triple {
                    subject: other.subject.try_into()?,
                    predicate: other.predicate.try_into()?,
                    object: other.object.try_into()?,
                }),
            }
        }
    }

    impl<'a> fmt::Display for GeneralizedQuad<'a> {
        fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
            if let Some(graph_name) = self.graph_name {
                write!(f, "GRAPH {} {{ ", graph_name)?;
            }
            write!(f, "{} {} {} .", self.subject, self.predicate, self.object)?;
            if self.graph_name.is_some() {
                write!(f, "}}")?;
            }
            Ok(())
        }
    }

    //

    /// An error raised when generalized RDF can not be converted to strict RDF.
    #[derive(Debug, Clone)]
    pub struct StrictRdfError {
        message: Box<str>,
    }

    impl<'a> fmt::Display for StrictRdfError {
        fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
            write!(f, "StrictRdfError: ?{}", self.message)
        }
    }

    impl Error for StrictRdfError {}

    impl From<&str> for StrictRdfError {
        fn from(message: &str) -> StrictRdfError {
            StrictRdfError {
                message: message.into(),
            }
        }
    }
}

/// Interface for generalized RDF parsers.
pub mod parser {
    use std::error::Error;
    use super::model::GeneralizedQuad;

    /// A parser returning generalized [`Quad`](../model/struct.Quad.html).
    pub trait GeneralizedQuadsParser {
        type Error: Error;

        /// Parses the complete file and calls `on_quad` each time a new quad is read.
        ///
        /// May fail on errors caused by the parser itself or by the callback function ``on_quad``.
        fn parse_all<E: From<Self::Error>>(
            &mut self,
            on_quad: &mut impl FnMut(GeneralizedQuad) -> Result<(), E>,
        ) -> Result<(), E> {
            while !self.is_end() {
                self.parse_step(on_quad)?
            }
            Ok(())
        }

        /// Parses a small chunk of the file and calls `on_quad` each time a new quad is read.
        /// (A "small chunk" could be a line for an N-Quads parser.)
        ///
        /// This method should be called as long as [`is_end`](#tymethod.is_end) returns false.
        ///
        /// May fail on errors caused by the parser itself or by the callback function ``on_quad``.
        fn parse_step<E: From<Self::Error>>(
            &mut self,
            on_quad: &mut impl FnMut(GeneralizedQuad) -> Result<(), E>,
        ) -> Result<(), E>;

        /// Returns `true` if the file has been completely consumed by the parser.
        fn is_end(&self) -> bool;

        /// Converts the parser into a `Result<T, E>` iterator.
        ///
        /// `convert_quad` is a function converting Rio [`GeneralizedQuad`](../gmodel/struct.GeneralizedQuad.html)s to `T`.
        fn into_iter<T, E, F>(self, convert_quad: F) -> GeneralizedQuadsParserIterator<T, E, F, Self>
        where
            E: From<Self::Error>,
            F: FnMut(GeneralizedQuad) -> Result<T, E>,
            Self: Sized,
        {
            GeneralizedQuadsParserIterator {
                parser: self,
                buffer: Vec::default(),
                convert_quad,
            }
        }
    }


    /// Created with the method [`into_iter`](trait.GeneralizedQuadsParser.html#method.into_iter).
    pub struct GeneralizedQuadsParserIterator<T, E: From<P::Error>, F: FnMut(GeneralizedQuad) -> Result<T, E>, P: GeneralizedQuadsParser>
    {
        parser: P,
        buffer: Vec<T>,
        convert_quad: F,
    }

    impl<T, E, F, P> Iterator
        for GeneralizedQuadsParserIterator<T, E, F, P>
    where
        E: From<P::Error>,
        F: FnMut(GeneralizedQuad) -> Result<T, E>,
        P: GeneralizedQuadsParser + Sized,
    {
        type Item = Result<T, E>;

        fn next(&mut self) -> Option<Result<T, E>> {
            loop {
                if let Some(r) = self.buffer.pop() {
                    return Some(Ok(r));
                }
                if self.parser.is_end() {
                    return None;
                }

                let buffer = &mut self.buffer;
                let convert_quad = &mut self.convert_quad;
                if let Err(e) = self
                    .parser
                    .parse_step(&mut |q| convert_quad(q).map(|q| buffer.push(q)))
                {
                    return Some(Err(e));
                }
            }
        }
    }
}