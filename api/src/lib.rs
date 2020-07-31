//! This crate provides basic interfaces and data structures for building RDF parsers.
//!
//! It is currently used by the [`rio_turtle`](https://docs.rs/rio_turtle/) and [`rio_xml`](https://docs.rs/rio_xml/) crates.
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

#[cfg(feature = "sophia")]
/// [Sophia] adapters for Rio parsers.
///
/// This module is available if feature `sophia` is enabled.
///
/// It ensures that the types defined in [`model`]
/// implement the appropriate trait from [Sophia].
///
/// [Sophia]: https://crates.io/crates/sophia
/// [`model`]: ../model/index.html
mod sophia {
    mod model;
}
