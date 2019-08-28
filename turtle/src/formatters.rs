use rio_api::formatter::{QuadsFormatter, TriplesFormatter};
use rio_api::model::*;
use std::io;
use std::io::Write;

/// A [Canonical N-Triples](https://www.w3.org/TR/n-triples/#canonical-ntriples) formatter.
///
/// It implements the `TriplesFormatter` trait.
///
/// Write some triples using the `TriplesFormatter` API into a `Vec` buffer:
/// ```
/// use rio_turtle::NTriplesFormatter;
/// use rio_api::formatter::TriplesFormatter;
/// use rio_api::model::{NamedNode, Triple};
///
/// let mut formatter = NTriplesFormatter::new(Vec::default());
/// formatter.format(&Triple {
///     subject: NamedNode { iri: "http://example.com/foo" }.into(),
///     predicate: NamedNode { iri: "http://www.w3.org/1999/02/22-rdf-syntax-ns#type" }.into(),
///     object: NamedNode { iri: "http://schema.org/Person" }.into()
/// }).unwrap();
/// let ntriples = formatter.finish();
/// ```
pub struct NTriplesFormatter<W: Write> {
    write: W,
}

impl<W: Write> NTriplesFormatter<W> {
    /// Builds a new formatter from a `Write` implementation
    pub fn new(write: W) -> Self {
        Self { write }
    }

    /// Finishes to write and returns the underlying `Write`
    pub fn finish(self) -> W {
        self.write
    }
}

impl<W: Write> TriplesFormatter for NTriplesFormatter<W> {
    type Error = io::Error;

    fn format(&mut self, triple: &Triple) -> Result<(), io::Error> {
        writeln!(self.write, "{}", triple)
    }
}

/// A [N-Quads](https://www.w3.org/TR/n-quads/) formatter.
///
/// It implements the `QuadsFormatter` trait.
///
/// Write some triples using the `QuadsFormatter` API into a `Vec` buffer:
/// ```
/// use rio_turtle::NQuadsFormatter;
/// use rio_api::formatter::QuadsFormatter;
/// use rio_api::model::{NamedNode, Triple, Quad};
///
/// let mut formatter = NQuadsFormatter::new(Vec::default());
/// formatter.format(&Quad {
///     subject: NamedNode { iri: "http://example.com/foo" }.into(),
///     predicate: NamedNode { iri: "http://www.w3.org/1999/02/22-rdf-syntax-ns#type" }.into(),
///     object: NamedNode { iri: "http://schema.org/Person" }.into(),
///     graph_name: Some(NamedNode { iri: "http://example.com/" }.into())
/// }).unwrap();
/// let nquads = formatter.finish();
/// ```
pub struct NQuadsFormatter<W: Write> {
    write: W,
}

impl<W: Write> NQuadsFormatter<W> {
    /// Builds a new formatter from a `Write` implementation
    pub fn new(write: W) -> Self {
        Self { write }
    }

    /// Finishes to write and returns the underlying `Write`
    pub fn finish(self) -> W {
        self.write
    }
}

impl<W: Write> QuadsFormatter for NQuadsFormatter<W> {
    type Error = io::Error;

    fn format(&mut self, quad: &Quad) -> Result<(), io::Error> {
        writeln!(self.write, "{}", quad)
    }
}

/// A [Turtle](https://www.w3.org/TR/turtle/) formatter.
///
/// It implements the `TriplesFormatter` trait.
///
/// Write some triples using the `TriplesFormatter` API into a `Vec` buffer:
/// ```
/// use rio_turtle::TurtleFormatter;
/// use rio_api::formatter::TriplesFormatter;
/// use rio_api::model::{NamedNode, Triple};
///
/// let mut formatter = TurtleFormatter::new(Vec::default());
/// formatter.format(&Triple {
///     subject: NamedNode { iri: "http://example.com/foo" }.into(),
///     predicate: NamedNode { iri: "http://www.w3.org/1999/02/22-rdf-syntax-ns#type" }.into(),
///     object: NamedNode { iri: "http://schema.org/Person" }.into()
/// }).unwrap();
/// let ntriples = formatter.finish();
/// ```
pub struct TurtleFormatter<W: Write> {
    write: W,
}

impl<W: Write> TurtleFormatter<W> {
    /// Builds a new formatter from a `Write` implementation
    pub fn new(write: W) -> Self {
        Self { write }
    }

    /// Finishes to write and returns the underlying `Write`
    pub fn finish(self) -> W {
        self.write
    }
}

impl<W: Write> TriplesFormatter for TurtleFormatter<W> {
    type Error = io::Error;

    fn format(&mut self, triple: &Triple) -> Result<(), io::Error> {
        writeln!(self.write, "{}", triple)
    }
}

/// A [TriG](https://www.w3.org/TR/trig/) formatter.
///
/// It implements the `QuadsFormatter` trait.
///
/// Write some triples using the `QuadsFormatter` API into a `Vec` buffer:
/// ```
/// use rio_turtle::TriGFormatter;
/// use rio_api::formatter::QuadsFormatter;
/// use rio_api::model::{NamedNode, Triple, Quad};
///
/// let mut formatter = TriGFormatter::new(Vec::default());
/// formatter.format(&Quad {
///     subject: NamedNode { iri: "http://example.com/foo" }.into(),
///     predicate: NamedNode { iri: "http://www.w3.org/1999/02/22-rdf-syntax-ns#type" }.into(),
///     object: NamedNode { iri: "http://schema.org/Person" }.into(),
///     graph_name: Some(NamedNode { iri: "http://example.com/" }.into())
/// }).unwrap();
/// let nquads = formatter.finish();
/// ```
pub struct TriGFormatter<W: Write> {
    write: W,
}

impl<W: Write> TriGFormatter<W> {
    /// Builds a new formatter from a `Write` implementation
    pub fn new(write: W) -> Self {
        Self { write }
    }

    /// Finishes to write and returns the underlying `Write`
    pub fn finish(self) -> W {
        self.write
    }
}

impl<W: Write> QuadsFormatter for TriGFormatter<W> {
    type Error = io::Error;

    fn format(&mut self, quad: &Quad) -> Result<(), io::Error> {
        let triple = Triple {
            subject: quad.subject,
            predicate: quad.predicate,
            object: quad.object,
        };
        if let Some(graph_name) = quad.graph_name {
            writeln!(self.write, "{} {{ {} }}", graph_name, triple)
        } else {
            writeln!(self.write, "{}", triple)
        }
    }
}
