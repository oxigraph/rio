//! Implementation of [N-Triples](https://www.w3.org/TR/n-triples/), [N-Quads](https://www.w3.org/TR/n-quads/), [Turtle](https://www.w3.org/TR/turtle/) and [TriG](https://www.w3.org/TR/trig/) parsers.
//!
//! [RDF-star](https://w3c.github.io/rdf-star/cg-spec/) syntaxes are also supported, i.e. [Turtle-star](https://w3c.github.io/rdf-star/cg-spec/#turtle-star), [TriG-star](https://w3c.github.io/rdf-star/cg-spec/#trig-star), [N-Triples-star](https://w3c.github.io/rdf-star/cg-spec/#n-triples-star) and [N-Quads-star](https://w3c.github.io/rdf-star/cg-spec/#n-quads-star).
//!
//! All the provided parsers work in streaming from a `BufRead` implementation.
//! They do not rely on any dependencies outside of Rust standard library.
//! The parsers are not protected against memory overflows.
//! For example if the parsed content contains a literal string of 16 GB, 16 GB of memory will be allocated.
//!
//! How to read a file `foo.ttl` and count the number of `rdf:type` triples:
//! ```no_run
//! use rio_turtle::{TurtleParser, TurtleError};
//! use rio_api::parser::TriplesParser;
//! use rio_api::model::NamedNode;
//! use std::io::BufReader;
//! use std::fs::File;
//! use oxiri::Iri;
//!
//! let rdf_type = NamedNode { iri: "http://www.w3.org/1999/02/22-rdf-syntax-ns#type" };
//! let mut count = 0;
//! TurtleParser::new(BufReader::new(File::open("foo.ttl")?), Some(Iri::parse("file:foo.ttl".to_owned()).unwrap())).parse_all(&mut |t| {
//!     if t.predicate == rdf_type {
//!         count += 1;
//!     }
//!     Ok(()) as Result<(), TurtleError>
//! })?;
//! # Result::<_,TurtleError>::Ok(())
//! ```
//!
//! Replace `TurtleParser` by `NTriplesParser`, `NQuadsParser` or `TriGParser` to read an N-Triples, N-Quads or TriG file instead.
//!
//! `NTriplesParser` and `NQuadsParser` do not use the second argument of the `new` function that is the IRI of the file.
#![deny(unsafe_code)]
#![cfg_attr(docsrs, feature(doc_auto_cfg))]
#![doc(test(attr(deny(warnings))))]

mod error;
mod formatters;
mod ntriples;
mod shared;
mod triple_allocator;
mod turtle;
mod utils;

#[cfg(feature = "generalized")]
mod gnquads;
#[cfg(feature = "generalized")]
mod gtrig;
#[cfg(feature = "generalized")]
mod gtriple_allocator;

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
pub use gnquads::GeneralizedNQuadsParser;

#[cfg(feature = "generalized")]
pub use gtrig::GTriGParser;

/// Maximal number of nested structures (collections, blank node, quoted triples...).
///
/// This limit is set in order to avoid stack overflow error when parsing such structures due to too many recursive calls.
/// The actual limit value is a wet finger compromise between not failing to parse valid files and avoiding to trigger stack overflow errors.
const MAX_STACK_SIZE: usize = 128;

/// Maximal size of a buffer (useful to limit memory consumption).
const MAX_BUFFER_SIZE: usize = 10_000_000;
