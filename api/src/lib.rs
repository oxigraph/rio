//! This crate provides basic interfaces and data structures for building RDF parsers.
//!
//! It is currently used by the [`rio_turtle`](https://docs.rs/rio_turtle/) and [`rio_xml`](https://docs.rs/rio_xml/) crates.

pub mod formatter;
pub mod iri;
pub mod model;
pub mod parser;
