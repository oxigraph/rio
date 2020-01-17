//! Implementation of [W3C RDF tests](http://w3c.github.io/rdf-tests/) to tests Rio parsers conformance.
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

mod isomorphism;
pub mod manifest;
pub mod model;
pub mod parser_evaluator;
pub mod report;
mod vocab;
