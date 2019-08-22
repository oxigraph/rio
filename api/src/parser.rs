//! Interfaces for RDF parsers.

use crate::model::{Quad, Triple};
use std::error::Error;

/// A parser returning [`Triple`](../model/struct.Triple.html).
pub trait TripleParser: Sized {
    type Error: Error;

    /// Parses the complete file and calls `on_triple` each time a new triple is read.
    ///
    /// May fail on errors caused by the parser itself.
    ///
    /// See also [`try_parse_all`](#method.try_parse_all).
    fn parse_all(&mut self, on_triple: &mut impl FnMut(Triple) -> ()) -> Result<(), Self::Error> {
        while !self.is_end() {
            self.parse_step(on_triple)?;
        }
        Ok(())
    }

    /// Parses the complete file and calls `on_triple` each time a new triple is read.
    ///
    /// May fail on errors caused by the parser itself or by the callback function ``on_triple``.
    ///
    /// See also [`parse_all`](#method.parse_all).
    fn try_parse_all<F, E>(&mut self, on_triple: &mut F) -> Result<(), E>
    where
        F: FnMut(Triple) -> Result<(), E>,
        E: Error + From<Self::Error>,
    {
        while !self.is_end() {
            self.try_parse_step(on_triple)?;
        }
        Ok(())
    }

    /// Parses a small chunk of the file and calls `on_triple` each time a new triple is read.
    /// (A "small chunk" could be a line for an N-Triples parser.)
    ///
    /// This method should be called as long as [`is_end`](#tymethod.is_end) returns false.
    /// It may fail on errors caused by the parser itself.
    ///
    /// See also [`try_parse_step`](#tymethod.try_parse_step).
    fn parse_step(&mut self, on_triple: &mut impl FnMut(Triple) -> ()) -> Result<(), Self::Error> {
        self.try_parse_step(&mut |t| {
            on_triple(t);
            Ok(())
        })
    }

    /// Parses a small chunk of the file and calls `on_triple` each time a new triple is read.
    /// (A "small chunk" could be a line for an N-Triples parser.)
    ///
    /// This method should be called as long as [`is_end`](#tymethod.is_end) returns false.
    /// It may fail on errors caused by the parser itself or by the callback function ``on_triple``.
    ///
    /// See also [`parse_step`](#method.parse_step).
    fn try_parse_step<F, E>(&mut self, on_triple: &mut F) -> Result<(), E>
    where
        F: FnMut(Triple) -> Result<(), E>,
        E: Error + From<Self::Error>;

    /// Returns `true` if the file has been completely consumed by the parser.
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
    ///
    /// May fail on errors caused by the parser itself.
    ///
    /// See also [`try_parse_all`](#method.try_parse_all).
    fn parse_all(&mut self, on_quad: &mut impl FnMut(Quad) -> ()) -> Result<(), Self::Error> {
        while !self.is_end() {
            self.parse_step(on_quad)?
        }
        Ok(())
    }

    /// Parses the complete file and calls `on_quad` each time a new quad is read.
    ///
    /// May fail on errors caused by the parser itself or by the callback function ``on_quad``.
    ///
    /// See also [`parse_all`](#method.parse_all).
    fn try_parse_all<F, E>(&mut self, on_quad: &mut F) -> Result<(), E>
    where
        F: FnMut(Quad) -> Result<(), E>,
        E: Error + From<Self::Error>,
    {
        while !self.is_end() {
            self.try_parse_step(on_quad)?
        }
        Ok(())
    }

    /// Parses a small chunk of the file and calls `on_quad` each time a new quad is read.
    /// (A "small chunk" could be a line for an N-Quads parser.)
    ///
    /// This method should be called as long as [`is_end`](#tymethod.is_end) returns false.
    /// It may fail on errors caused by the parser itself.
    ///
    /// See also [`try_parse_step`](#tymethod.try_parse_step).
    fn parse_step(&mut self, on_quad: &mut impl FnMut(Quad) -> ()) -> Result<(), Self::Error> {
        self.try_parse_step(&mut |q| {
            on_quad(q);
            Ok(())
        })
    }

    /// Parses a small chunk of the file and calls `on_quad` each time a new quad is read.
    /// (A "small chunk" could be a line for an N-Quads parser.)
    ///
    /// This method should be called as long as [`is_end`](#tymethod.is_end) returns false.
    /// It may fail on errors caused by the parser itself or by the callback function ``on_quad``.
    ///
    /// See also [`parse_step`](#method.parse_step).
    fn try_parse_step<F, E>(&mut self, on_quad: &mut F) -> Result<(), E>
    where
        F: FnMut(Quad) -> Result<(), E>,
        E: Error + From<Self::Error>;

    /// Returns `true` if the file has been completely consumed by the parser.
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

/// Created with the method [`into_iter`](trait.QuadParser.html#method.into_iter).
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

/// Error trait that allows to get the textual position of the error
pub trait ParseError: Error {
    /// Returns the position of the error in the text, if known.
    fn textual_position(&self) -> Option<LineBytePosition>;
}

#[derive(Eq, PartialEq, Debug, Clone, Copy, Hash)]
pub struct LineBytePosition {
    line_number: usize,
    byte_number: usize,
}

impl LineBytePosition {
    /// Creates a new position where `line_number` and `byte_number` are both starting from zero
    pub fn new(line_number: usize, byte_number: usize) -> Self {
        Self {
            line_number,
            byte_number,
        }
    }

    /// The line number where the error occurred starting from 0
    pub fn line_number(&self) -> usize {
        self.line_number
    }

    /// The byte number where the error occurred starting from 0
    pub fn byte_number(&self) -> usize {
        self.byte_number
    }
}
