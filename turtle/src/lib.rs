//! Implementation of [N-Triples](https://www.w3.org/TR/n-triples/), [N-Quads](https://www.w3.org/TR/n-quads/), [Turtle](https://www.w3.org/TR/turtle/) and [TriG](https://www.w3.org/TR/trig/) parsers.
//!
//! All the provided parsers work in streaming from a `BufRead` implementation.
//! They do not rely on any dependencies outside of Rust standard library.
//! The parsers are not protected against memory overflows.
//! For example if the parsed content contains a literal string of 16GB, 16GB of memory will be allocated.
//!
//! How to read a file `foo.ttl` and count the number of `rdf:type` triples:
//! ```no_run
//! use rio_turtle::{TurtleParser, TurtleError};
//! use rio_api::parser::TriplesParser;
//! use rio_api::model::NamedNode;
//! use std::io::BufReader;
//! use std::fs::File;
//!
//! let rdf_type = NamedNode { iri: "http://www.w3.org/1999/02/22-rdf-syntax-ns#type" };
//! let mut count = 0;
//! TurtleParser::new(BufReader::new(File::open("foo.ttl").unwrap()), "file:foo.ttl").unwrap().parse_all(&mut |t| {
//!     if t.predicate == rdf_type {
//!         count += 1;
//!     }
//!     Ok(()) as Result<(), TurtleError>
//! }).unwrap();
//! ```
//!
//! Replace `TurtleParser` by `NTriplesParser`, `NQuadsParser` or `TriGParser` to read a N-Triples, N-Quads or TriG file instead.
//!
//! `NTriplesParser` and `NQuadsParser` do not use the second argument of the `new` function that is the IRI of the file.
#![deny(
    future_incompatible,
    nonstandard_style,
    rust_2018_idioms,
    missing_copy_implementations,
    trivial_casts,
    trivial_numeric_casts,
    unsafe_code,
    unused_qualifications
)]

mod error;
mod formatters;
mod ntriples;
mod shared;
mod turtle;
mod utils;

#[cfg(feature = "generalized")]
mod gtrig;

pub use error::TurtleError;
pub use formatters::NQuadsFormatter;
pub use formatters::NTriplesFormatter;
pub use formatters::TriGFormatter;
pub use formatters::TurtleFormatter;
pub use ntriples::NQuadsParser;
pub use ntriples::NTriplesParser;
pub use turtle::TriGParser;
pub use turtle::TurtleParser;

#[cfg(feature = "generalized")]
pub use gtrig::GTriGParser;

/// [Sophia] adapters for Rio parsers.
///
/// This module is available if feature `sophia` is enabled.
///
/// [Sophia]: https://crates.io/crates/sophia
#[cfg(feature = "sophia")]
pub mod sophia {
    #[cfg(feature = "generalized")]
    mod gtrig;
    mod nq;
    mod nt;
    mod trig;
    mod turtle;

    #[cfg(feature = "generalized")]
    pub use gtrig::*;
    pub use nq::*;
    pub use nt::*;
    pub use trig::*;
    pub use turtle::*;
}
