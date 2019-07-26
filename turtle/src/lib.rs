//! Implementation of [N-Triples](https://www.w3.org/TR/n-triples/) and [Turtle](https://www.w3.org/TR/turtle/) parsers.
//!
//! All the provided parsers work in streaming from a `BufRead` implementation.
//! They do not rely on any dependencies outside of Rust standard library.

mod error;
mod iri;
mod ntriples;
mod shared;
mod turtle;
mod utils;

pub use error::TurtleError;
pub use ntriples::NTriplesParser;
pub use turtle::TurtleParser;
