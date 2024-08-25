//! Implementation of an [RDF/XML](https://www.w3.org/TR/rdf-syntax-grammar/) streaming parser.
//!
//! <strong style="font-size: 150%">
//!
//! This library is going to be deprecated.
//! [oxrdfxml](https://crates.io/crates/oxrdfxml) is currently in development to replace it.
//! </strong>
//!
//! How to read a file `foo.rdf` and count the number of `rdf:type` triples:
//! ```no_run
//! use rio_xml::{RdfXmlParser, RdfXmlError};
//! use rio_api::parser::TriplesParser;
//! use rio_api::model::NamedNode;
//! use std::io::BufReader;
//! use std::fs::File;
//! use oxiri::Iri;
//!
//! let rdf_type = NamedNode { iri: "http://www.w3.org/1999/02/22-rdf-syntax-ns#type" };
//! let mut count = 0;
//! RdfXmlParser::new(BufReader::new(File::open("foo.rdf")?), Some(Iri::parse("file:foo.rdf".to_owned()).unwrap())).parse_all(&mut |t| {
//!     if t.predicate == rdf_type {
//!         count += 1;
//!     }
//!     Ok(()) as Result<(), RdfXmlError>
//! })?;
//! # Result::<_,RdfXmlError>::Ok(())
//! ```
//!
//! Write some triples in RDF/XML into a `Vec` buffer:
//! ```
//! use rio_xml::RdfXmlFormatter;
//! use rio_api::formatter::TriplesFormatter;
//! use rio_api::model::{NamedNode, Triple};
//!
//! let mut formatter = RdfXmlFormatter::new(Vec::default())?;
//! formatter.format(&Triple {
//!     subject: NamedNode { iri: "http://example.com/foo" }.into(),
//!     predicate: NamedNode { iri: "http://www.w3.org/1999/02/22-rdf-syntax-ns#type" }.into(),
//!     object: NamedNode { iri: "http://schema.org/Person" }.into()
//! })?;
//! let _xml = formatter.finish()?;
//! # std::io::Result::Ok(())
//! ```
#![deny(unsafe_code)]
#![cfg_attr(docsrs, feature(doc_auto_cfg))]
#![doc(test(attr(deny(warnings))))]

mod error;
mod formatter;
mod model;
mod parser;
mod utils;

pub use error::RdfXmlError;
pub use formatter::RdfXmlFormatter;
pub use parser::RdfXmlParser;
