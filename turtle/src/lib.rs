//! Implementation of [N-Triples](https://www.w3.org/TR/n-triples/), [N-Quads](https://www.w3.org/TR/n-quads/), [Turtle](https://www.w3.org/TR/turtle/) and [TriG](https://www.w3.org/TR/trig/) parsers.
//!
//! All the provided parsers work in streaming from a `BufRead` implementation.
//! They do not rely on any dependencies outside of Rust standard library.
//!
//! How to read a file `foo.ttl` and count the number of `rdf:type` triples:
//! ```no_run
//! use rio_turtle::{TurtleParser, TurtleError};
//! use rio_api::parser::TripleParser;
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

mod error;
mod iri;
mod ntriples;
mod shared;
mod turtle;
mod utils;

pub use error::TurtleError;
pub use ntriples::NQuadsParser;
pub use ntriples::NTriplesParser;
pub use turtle::TriGParser;
pub use turtle::TurtleParser;
