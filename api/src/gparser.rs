//! Interface for generalized RDF parsers.

use std::error::Error;
use crate::gmodel::GeneralizedQuad;

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
