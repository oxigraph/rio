//! Interfaces for RDF parsers.
//!
//! The main types are [`TriplesParser`] for triples parsing and [`QuadsParser`] for quads parsing.

#[cfg(feature = "generalized")]
pub use crate::generalized::parser::*;
use crate::model::{Quad, Triple};
use std::error::Error;

/// A parser returning [`Triple`](super::model::Triple).
pub trait TriplesParser: Sized {
    type Error: Error;

    /// Parses the complete file and calls `on_triple` each time a new triple is read.
    ///
    /// May fail on errors caused by the parser itself or by the callback function `on_triple`.
    fn parse_all<E: From<Self::Error>>(
        &mut self,
        on_triple: &mut impl FnMut(Triple<'_>) -> Result<(), E>,
    ) -> Result<(), E> {
        while !self.is_end() {
            self.parse_step(on_triple)?;
        }
        Ok(())
    }

    /// Parses a small chunk of the file and calls `on_triple` each time a new triple is read.
    /// (A "small chunk" could be a line for an N-Triples parser.)
    ///
    /// This method should be called as long as [`is_end`](TriplesParser::is_end) returns false.
    ///
    /// It may fail on errors caused by the parser itself or by the callback function `on_triple`.
    fn parse_step<E: From<Self::Error>>(
        &mut self,
        on_triple: &mut impl FnMut(Triple<'_>) -> Result<(), E>,
    ) -> Result<(), E>;

    /// Returns `true` if the file has been completely consumed by the parser.
    fn is_end(&self) -> bool;

    /// Converts the parser into a `Result<T, E>` iterator.
    ///
    /// `convert_triple` is a function converting Rio [`Triple`](super::model::Triple) to `T`.
    fn into_iter<T, E: From<Self::Error>, F: FnMut(Triple<'_>) -> Result<T, E>>(
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

/// Created with the method [`into_iter`](TriplesParser::into_iter()).
pub struct TriplesParserIterator<
    T,
    E: From<P::Error>,
    F: FnMut(Triple<'_>) -> Result<T, E>,
    P: TriplesParser,
> {
    parser: P,
    buffer: Vec<T>,
    convert_triple: F,
}

impl<T, E: From<P::Error>, F: FnMut(Triple<'_>) -> Result<T, E>, P: TriplesParser> Iterator
    for TriplesParserIterator<T, E, F, P>
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
            let convert_triple = &mut self.convert_triple;
            if let Err(e) = self
                .parser
                .parse_step(&mut |t| convert_triple(t).map(|t| buffer.push(t)))
            {
                return Some(Err(e));
            }
        }
    }
}

/// A parser returning [`Quad`](super::model::Quad).
pub trait QuadsParser: Sized {
    type Error: Error;

    /// Parses the complete file and calls `on_quad` each time a new quad is read.
    ///
    /// May fails on errors caused by the parser itself or by the callback function `on_quad`.
    fn parse_all<E: From<Self::Error>>(
        &mut self,
        on_quad: &mut impl FnMut(Quad<'_>) -> Result<(), E>,
    ) -> Result<(), E> {
        while !self.is_end() {
            self.parse_step(on_quad)?
        }
        Ok(())
    }

    /// Parses a small chunk of the file and calls `on_quad` each time a new quad is read.
    /// (A "small chunk" could be a line for an N-Quads parser.)
    ///
    /// This method should be called as long as [`is_end`](QuadsParser::is_end) returns false.
    ///
    /// May fails on errors caused by the parser itself or by the callback function `on_quad`.
    fn parse_step<E: From<Self::Error>>(
        &mut self,
        on_quad: &mut impl FnMut(Quad<'_>) -> Result<(), E>,
    ) -> Result<(), E>;

    /// Returns `true` if the file has been completely consumed by the parser.
    fn is_end(&self) -> bool;

    /// Converts the parser into a `Result<T, E>` iterator.
    ///
    /// `convert_triple` is a function converting Rio [`Triple`](super::model::Triple) to `T`.
    fn into_iter<T, E: From<Self::Error>, F: FnMut(Quad<'_>) -> Result<T, E>>(
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

/// Created with the method [`into_iter`](QuadsParser::into_iter()).
pub struct QuadsParserIterator<
    T,
    E: From<P::Error>,
    F: FnMut(Quad<'_>) -> Result<T, E>,
    P: QuadsParser,
> {
    parser: P,
    buffer: Vec<T>,
    convert_quad: F,
}

impl<T, E: From<P::Error>, F: FnMut(Quad<'_>) -> Result<T, E>, P: QuadsParser> Iterator
    for QuadsParserIterator<T, E, F, P>
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

/// Error trait that allows to get the textual position of the error
pub trait ParseError: Error {
    /// Returns the position of the error in the text, if known.
    fn textual_position(&self) -> Option<LineBytePosition>;
}

#[derive(Eq, PartialEq, Debug, Clone, Copy, Hash)]
pub struct LineBytePosition {
    line_number: u64,
    byte_number: u64,
}

impl LineBytePosition {
    /// Creates a new position where `line_number` and `byte_number` are both starting from 1
    pub fn new(line_number: u64, byte_number: u64) -> Self {
        Self {
            line_number,
            byte_number,
        }
    }

    /// The line number where the error occurred starting from 0
    pub fn line_number(&self) -> u64 {
        self.line_number
    }

    /// The byte number where the error occurred starting from 0
    pub fn byte_number(&self) -> u64 {
        self.byte_number
    }
}
