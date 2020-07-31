//! This crate provides basic interfaces and data structures for building RDF parsers.
//!
//! It is currently used by the [`rio_turtle`](https://docs.rs/rio_turtle/) and [`rio_xml`](https://docs.rs/rio_xml/) crates.
//!
//! If the `sophia` feature is enabled, the types defined in [`model`](model/index.html) implement the appropriate trait from [Sophia]( https://crates.io/crates/sophia_api).
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
#![doc(test(attr(deny(warnings))))]

pub mod formatter;
pub mod model;
pub mod parser;

#[cfg(feature = "generalized")]
mod generalized;

#[cfg(feature = "sophia_api")]
mod sophia;
