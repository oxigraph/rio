//! This module contains extensions for generalized RDF.
//! Its elements are re-exported by `model` and `parser`, respectively.

/// Data structures for generalized [RDF 1.1 Concepts](https://www.w3.org/TR/rdf11-concepts/),
/// allowing variables, and any kind of node in any Triple/Quad position.
pub mod model {
    use std::convert::{TryFrom, TryInto};
    use std::error::Error;
    use std::fmt;

    pub use crate::model::*;

    /// A SPARQL [variable](https://www.w3.org/TR/sparql11-query/#QSynVariables).
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
    ///
    /// Using it requires to enable the `generalized` feature.
    #[derive(Eq, PartialEq, Ord, PartialOrd, Debug, Clone, Copy, Hash)]
    pub struct Variable<'a> {
        /// The name of  the [variable](https://www.w3.org/TR/sparql11-query/#QSynVariables) itself.
        pub name: &'a str,
    }

    impl<'a> fmt::Display for Variable<'a> {
        #[inline]
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
    /// * [variable](https://www.w3.org/TR/sparql11-query/#QSynVariables).
    ///
    /// The default string formatter is returning an N-Triples, Turtle and SPARQL compatible representation.
    ///
    /// Using it requires to enable the `generalized` feature.
    #[derive(Eq, PartialEq, Debug, Clone, Copy, Hash)]
    pub enum GeneralizedTerm<'a> {
        NamedNode(NamedNode<'a>),
        BlankNode(BlankNode<'a>),
        Literal(Literal<'a>),
        Variable(Variable<'a>),
        Triple(&'a Triple<'a>),
    }

    impl<'a> From<NamedNode<'a>> for GeneralizedTerm<'a> {
        #[inline]
        fn from(other: NamedNode<'a>) -> GeneralizedTerm<'a> {
            GeneralizedTerm::NamedNode(other)
        }
    }

    impl<'a> From<BlankNode<'a>> for GeneralizedTerm<'a> {
        #[inline]
        fn from(other: BlankNode<'a>) -> GeneralizedTerm<'a> {
            GeneralizedTerm::BlankNode(other)
        }
    }

    impl<'a> From<Literal<'a>> for GeneralizedTerm<'a> {
        #[inline]
        fn from(other: Literal<'a>) -> GeneralizedTerm<'a> {
            GeneralizedTerm::Literal(other)
        }
    }

    impl<'a> From<Variable<'a>> for GeneralizedTerm<'a> {
        #[inline]
        fn from(other: Variable<'a>) -> GeneralizedTerm<'a> {
            GeneralizedTerm::Variable(other)
        }
    }

    impl<'a> From<Subject<'a>> for GeneralizedTerm<'a> {
        #[inline]
        fn from(other: Subject<'a>) -> GeneralizedTerm<'a> {
            match other {
                Subject::NamedNode(inner) => GeneralizedTerm::NamedNode(inner),
                Subject::BlankNode(inner) => GeneralizedTerm::BlankNode(inner),
                Subject::Triple(inner) => GeneralizedTerm::Triple(inner),
            }
        }
    }

    impl<'a> From<GraphName<'a>> for GeneralizedTerm<'a> {
        #[inline]
        fn from(other: GraphName<'a>) -> GeneralizedTerm<'a> {
            match other {
                GraphName::NamedNode(inner) => GeneralizedTerm::NamedNode(inner),
                GraphName::BlankNode(inner) => GeneralizedTerm::BlankNode(inner),
            }
        }
    }

    impl<'a> From<Term<'a>> for GeneralizedTerm<'a> {
        #[inline]
        fn from(other: Term<'a>) -> GeneralizedTerm<'a> {
            match other {
                Term::NamedNode(inner) => GeneralizedTerm::NamedNode(inner),
                Term::BlankNode(inner) => GeneralizedTerm::BlankNode(inner),
                Term::Literal(inner) => GeneralizedTerm::Literal(inner),
                Term::Triple(inner) => GeneralizedTerm::Triple(inner),
            }
        }
    }

    impl<'a> TryFrom<GeneralizedTerm<'a>> for NamedNode<'a> {
        type Error = StrictRdfError;

        #[inline]
        fn try_from(other: GeneralizedTerm<'a>) -> Result<NamedNode<'a>, StrictRdfError> {
            match other {
                GeneralizedTerm::NamedNode(inner) => Ok(inner),
                GeneralizedTerm::BlankNode(_) => Err(StrictRdfError {
                    message: "Blank node cannot be used as predicate",
                }),
                GeneralizedTerm::Literal(_) => Err(StrictRdfError {
                    message: "Literal cannot be used as predicate",
                }),
                GeneralizedTerm::Variable(_) => Err(StrictRdfError {
                    message: "Variable cannot be converted to Term",
                }),
                GeneralizedTerm::Triple(_) => Err(StrictRdfError {
                    message: "Triple cannot be used as predicate",
                }),
            }
        }
    }

    impl<'a> TryFrom<GeneralizedTerm<'a>> for Subject<'a> {
        type Error = StrictRdfError;

        #[inline]
        fn try_from(other: GeneralizedTerm<'a>) -> Result<Subject<'a>, StrictRdfError> {
            match other {
                GeneralizedTerm::NamedNode(inner) => Ok(Subject::NamedNode(inner)),
                GeneralizedTerm::BlankNode(inner) => Ok(Subject::BlankNode(inner)),
                GeneralizedTerm::Literal(_) => Err(StrictRdfError {
                    message: "Literal cannot be used a subject",
                }),
                GeneralizedTerm::Variable(_) => Err(StrictRdfError {
                    message: "Variable cannot be converted to Term",
                }),
                GeneralizedTerm::Triple(triple) => Ok(Subject::Triple(triple)),
            }
        }
    }

    impl<'a> TryFrom<GeneralizedTerm<'a>> for GraphName<'a> {
        type Error = StrictRdfError;

        #[inline]
        fn try_from(other: GeneralizedTerm<'a>) -> Result<GraphName<'a>, StrictRdfError> {
            match other {
                GeneralizedTerm::NamedNode(inner) => Ok(GraphName::NamedNode(inner)),
                GeneralizedTerm::BlankNode(inner) => Ok(GraphName::BlankNode(inner)),
                GeneralizedTerm::Literal(_) => Err(StrictRdfError {
                    message: "Literal cannot be used a graph name",
                }),
                GeneralizedTerm::Variable(_) => Err(StrictRdfError {
                    message: "Variable cannot be converted to Term",
                }),
                GeneralizedTerm::Triple(_) => Err(StrictRdfError {
                    message: "Triple cannot be used as a graph name",
                }),
            }
        }
    }

    impl<'a> TryFrom<GeneralizedTerm<'a>> for Term<'a> {
        type Error = StrictRdfError;

        #[inline]
        fn try_from(other: GeneralizedTerm<'a>) -> Result<Term<'a>, StrictRdfError> {
            match other {
                GeneralizedTerm::NamedNode(inner) => Ok(Term::NamedNode(inner)),
                GeneralizedTerm::BlankNode(inner) => Ok(Term::BlankNode(inner)),
                GeneralizedTerm::Literal(inner) => Ok(Term::Literal(inner)),
                GeneralizedTerm::Variable(_) => Err(StrictRdfError {
                    message: "Variable cannot be converted to Term",
                }),
                GeneralizedTerm::Triple(inner) => Ok(Term::Triple(inner)),
            }
        }
    }

    impl<'a> fmt::Display for GeneralizedTerm<'a> {
        #[inline]
        fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
            match self {
                GeneralizedTerm::NamedNode(node) => node.fmt(f),
                GeneralizedTerm::BlankNode(node) => node.fmt(f),
                GeneralizedTerm::Literal(literal) => literal.fmt(f),
                GeneralizedTerm::Variable(variable) => variable.fmt(f),
                GeneralizedTerm::Triple(triple) => triple.fmt(f),
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
    ///
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
    ///
    /// Using it requires to enable the `generalized` feature.
    #[derive(Eq, PartialEq, Debug, Clone, Hash)]
    pub struct GeneralizedQuad<'a> {
        pub subject: GeneralizedTerm<'a>,
        pub predicate: GeneralizedTerm<'a>,
        pub object: GeneralizedTerm<'a>,
        pub graph_name: Option<GeneralizedTerm<'a>>,
    }

    impl<'a> From<Quad<'a>> for GeneralizedQuad<'a> {
        #[inline]
        fn from(other: Quad<'a>) -> GeneralizedQuad<'a> {
            GeneralizedQuad {
                subject: other.subject.into(),
                predicate: other.predicate.into(),
                object: other.object.into(),
                graph_name: other.graph_name.map(GeneralizedTerm::from),
            }
        }
    }

    impl<'a> TryFrom<GeneralizedQuad<'a>> for Quad<'a> {
        type Error = StrictRdfError;

        #[inline]
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

    impl<'a> TryFrom<GeneralizedQuad<'a>> for Triple<'a> {
        type Error = StrictRdfError;

        #[inline]
        fn try_from(other: GeneralizedQuad<'a>) -> Result<Triple<'a>, StrictRdfError> {
            match other.graph_name {
                Some(_) => Err(StrictRdfError {
                    message: "Quad in named graph cannot be converted to Triple",
                }),
                None => Ok(Triple {
                    subject: other.subject.try_into()?,
                    predicate: other.predicate.try_into()?,
                    object: other.object.try_into()?,
                }),
            }
        }
    }

    impl<'a> fmt::Display for GeneralizedQuad<'a> {
        #[inline]
        fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
            if let Some(graph_name) = &self.graph_name {
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

    /// An error raised when generalized RDF cannot be converted to strict RDF.
    #[derive(Debug, Clone, Copy)]
    pub struct StrictRdfError {
        message: &'static str,
    }

    impl<'a> fmt::Display for StrictRdfError {
        #[inline]
        fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
            self.message.fmt(f)
        }
    }

    impl Error for StrictRdfError {}
}

/// Interface for generalized RDF parsers.
pub mod parser {
    use super::model::GeneralizedQuad;
    use std::error::Error;

    /// A parser returning generalized [`Quad`](super::model::Quad).
    ///
    /// Using it requires to enable the `generalized` feature.
    pub trait GeneralizedQuadsParser {
        type Error: Error;

        /// Parses the complete file and calls `on_quad` each time a new quad is read.
        ///
        /// May fails on errors caused by the parser itself or by the callback function `on_quad`.
        fn parse_all<E: From<Self::Error>>(
            &mut self,
            on_quad: &mut impl FnMut(GeneralizedQuad<'_>) -> Result<(), E>,
        ) -> Result<(), E> {
            while !self.is_end() {
                self.parse_step(on_quad)?
            }
            Ok(())
        }

        /// Parses a small chunk of the file and calls `on_quad` each time a new quad is read.
        /// (A "small chunk" could be a line for an N-Quads parser.)
        ///
        /// This method should be called as long as [`is_end`](GeneralizedQuadsParser::is_end) returns false.
        ///
        /// May fails on errors caused by the parser itself or by the callback function `on_quad`.
        fn parse_step<E: From<Self::Error>>(
            &mut self,
            on_quad: &mut impl FnMut(GeneralizedQuad<'_>) -> Result<(), E>,
        ) -> Result<(), E>;

        /// Returns `true` if the file has been completely consumed by the parser.
        fn is_end(&self) -> bool;

        /// Converts the parser into a `Result<T, E>` iterator.
        ///
        /// `convert_quad` is a function converting Rio [`GeneralizedQuad`]s to `T`.
        fn into_iter<T, E, F>(
            self,
            convert_quad: F,
        ) -> GeneralizedQuadsParserIterator<T, E, F, Self>
        where
            E: From<Self::Error>,
            F: FnMut(GeneralizedQuad<'_>) -> Result<T, E>,
            Self: Sized,
        {
            GeneralizedQuadsParserIterator {
                parser: self,
                buffer: Vec::default(),
                convert_quad,
            }
        }
    }

    /// Created with the method [`into_iter`](GeneralizedQuadsParser::into_iter()).
    ///
    /// Using it requires to enable the `generalized` feature.
    pub struct GeneralizedQuadsParserIterator<
        T,
        E: From<P::Error>,
        F: FnMut(GeneralizedQuad<'_>) -> Result<T, E>,
        P: GeneralizedQuadsParser,
    > {
        parser: P,
        buffer: Vec<T>,
        convert_quad: F,
    }

    impl<T, E, F, P> Iterator for GeneralizedQuadsParserIterator<T, E, F, P>
    where
        E: From<P::Error>,
        F: FnMut(GeneralizedQuad<'_>) -> Result<T, E>,
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
