//! Interfaces for RDF formatters.

use crate::model::{Quad, Triple};
use std::error::Error;

/// A formatter for [`Triple`](../model/struct.Triple.html).
pub trait TriplesFormatter {
    type Error: Error;

    /// Writes a triple
    fn format(&mut self, triple: &Triple) -> Result<(), Self::Error>;
}

/// A formatter for [`Quad`](../model/struct.Quad.html).
pub trait QuadsFormatter {
    type Error: Error;

    /// Writes a quad
    fn format(&mut self, quad: &Quad) -> Result<(), Self::Error>;
}
