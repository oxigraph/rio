//! This crate provides basic interfaces and data structures for building RDF parsers.
//!
//! It is currently used by the `rio_turtle` crate that implements [N-Triples](https://www.w3.org/TR/n-triples/) and [Turtle](https://www.w3.org/TR/turtle/) parsers.

pub mod model;
pub mod parser;
