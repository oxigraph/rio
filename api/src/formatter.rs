//! Interfaces for RDF formatters.
//!
//! The main types are [`TriplesFormatter`] for triples parsing and [`QuadsFormatter`] for quads parsing.

use crate::model::{Quad, Triple};
use std::error::Error;

/// A formatter for [`Triple`](super::model::Triple).
pub trait TriplesFormatter {
    type Error: Error;

    /// Writes a triple
    fn format(&mut self, triple: &Triple<'_>) -> Result<(), Self::Error>;
}

/// A formatter for [`Quad`](super::model::Quad).
pub trait QuadsFormatter {
    type Error: Error;

    /// Writes a quad
    fn format(&mut self, quad: &Quad<'_>) -> Result<(), Self::Error>;
}
