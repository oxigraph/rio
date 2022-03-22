//! This crate provides basic interfaces and data structures for building [RDF 1.1](https://www.w3.org/TR/rdf11-concepts/) and [RDF-star](https://w3c.github.io/rdf-star/cg-spec/) parsers.
//!
//! It is currently used by the `rio_turtle` and `rio_xml` crates.
#![deny(unsafe_code)]
#![doc(test(attr(deny(warnings))))]

pub mod formatter;
pub mod model;
pub mod parser;

#[cfg(feature = "generalized")]
mod generalized;
