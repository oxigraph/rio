//! Interfaces for RDF parsers.

use crate::model::{Quad, Triple};
use std::error::Error;

/// A parser returning [`Triple`](../model/struct.Triple.html).
pub trait TripleParser: Sized {
    type Error: Error;

    /// Parses the complete file and calls `on_triple` each time a new triple is read.
    fn parse_all(&mut self, on_triple: &mut impl FnMut(Triple) -> ()) -> Result<(), Self::Error> {
        while !self.is_end() {
            if let Err(error) = self.parse_step(on_triple) {
                return Err(error);
            }
        }
        Ok(())
    }

    /// Parses a small chunk of the file and calls `on_triple` each time a new triple is read.
    ///
    /// This method should be called as long as `is_end` returns false.
    ///
    /// I could be a line for line based formats like N-Triples...
    fn parse_step(&mut self, on_triple: &mut impl FnMut(Triple) -> ()) -> Result<(), Self::Error>;

    /// Return `true` if the complete file has been consumed by the parser.
    fn is_end(&self) -> bool;

    /// Converts the parser into a `Result<T, E>` iterator.
    ///
    /// `convert_triple` is a function converting Rio [`Triple`](../model/struct.Triple.html) to `T`.
    fn into_iter<T, E: From<Self::Error>, F: FnMut(Triple) -> Result<T, E>>(
        self,
        convert_triple: F,
    ) -> TriplesParserIterator<T, E, F, Self> {
        TriplesParserIterator {
            parser: self,
            buffer: Vec::default(),
            convert_triple,
        }
    }
}

/// Created with the method [`into_iter`](trait.TripleParser.html#method.into_iter).
pub struct TriplesParserIterator<
    T,
    E: From<P::Error>,
    F: FnMut(Triple) -> Result<T, E>,
    P: TripleParser,
> {
    parser: P,
    buffer: Vec<Result<T, E>>,
    convert_triple: F,
}

impl<T, E: From<P::Error>, F: FnMut(Triple) -> Result<T, E>, P: TripleParser> Iterator
    for TriplesParserIterator<T, E, F, P>
{
    type Item = Result<T, E>;

    fn next(&mut self) -> Option<Result<T, E>> {
        loop {
            if let Some(r) = self.buffer.pop() {
                return Some(r);
            }
            if self.parser.is_end() {
                return None;
            }

            let buffer = &mut self.buffer;
            let convert_triple = &mut self.convert_triple;
            if let Err(e) = self
                .parser
                .parse_step(&mut |t| buffer.push(convert_triple(t)))
            {
                return Some(Err(e.into()));
            }
        }
    }
}

/// A parser returning [`Quad`](../model/struct.Quad.html).
pub trait QuadParser: Sized {
    type Error: Error;

    /// Parses the complete file and calls `on_quad` each time a new quad is read.
    fn parse_all(&mut self, on_quad: &mut impl FnMut(Quad) -> ()) -> Result<(), Self::Error> {
        while !self.is_end() {
            if let Err(error) = self.parse_step(on_quad) {
                return Err(error);
            }
        }
        Ok(())
    }

    /// Parses a small chunk of the file and calls `on_triple` each time a new triple is read.
    ///
    /// This method should be called as long as `is_end` returns false.
    ///
    /// I could be a line for line based formats like N-Triples...
    fn parse_step(&mut self, on_quad: &mut impl FnMut(Quad) -> ()) -> Result<(), Self::Error>;

    /// Return `true` if the complete file has been consumed by the parser.
    fn is_end(&self) -> bool;

    /// Converts the parser into a `Result<T, E>` iterator.
    ///
    /// `convert_triple` is a function converting Rio [`Triple`](../model/struct.Triple.html) to `T`.
    fn into_iter<T, E: From<Self::Error>, F: FnMut(Quad) -> Result<T, E>>(
        self,
        convert_quad: F,
    ) -> QuadsParserIterator<T, E, F, Self> {
        QuadsParserIterator {
            parser: self,
            buffer: Vec::default(),
            convert_quad,
        }
    }
}

/// Created with the method [`into_iter`](trait.TripleParser.html#method.into_iter).
pub struct QuadsParserIterator<T, E: From<P::Error>, F: FnMut(Quad) -> Result<T, E>, P: QuadParser>
{
    parser: P,
    buffer: Vec<Result<T, E>>,
    convert_quad: F,
}

impl<T, E: From<P::Error>, F: FnMut(Quad) -> Result<T, E>, P: QuadParser> Iterator
    for QuadsParserIterator<T, E, F, P>
{
    type Item = Result<T, E>;

    fn next(&mut self) -> Option<Result<T, E>> {
        loop {
            if let Some(r) = self.buffer.pop() {
                return Some(r);
            }
            if self.parser.is_end() {
                return None;
            }

            let buffer = &mut self.buffer;
            let convert_quad = &mut self.convert_quad;
            if let Err(e) = self
                .parser
                .parse_step(&mut |t| buffer.push(convert_quad(t)))
            {
                return Some(Err(e.into()));
            }
        }
    }
}
