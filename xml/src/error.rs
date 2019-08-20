use rio_api::parser::{LineBytePosition, ParseError};
use std::error::Error;
use std::fmt;

/// Error that might be returned during parsing.
///
/// It might wrap an IO error or be a parsing error.
#[derive(Debug)]
pub struct RdfXmlError {
    pub(crate) kind: RdfXmlErrorKind,
}

#[derive(Debug)]
pub enum RdfXmlErrorKind {
    Xml(quick_xml::Error),
    InvalidIri(String),
    Other(String),
}

impl fmt::Display for RdfXmlError {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match &self.kind {
            RdfXmlErrorKind::Xml(error) => error.fmt(f),
            RdfXmlErrorKind::InvalidIri(iri) => write!(f, "The IRI {} is invalid", iri),
            RdfXmlErrorKind::Other(message) => write!(f, "{}", message),
        }
    }
}

impl Error for RdfXmlError {
    fn source(&self) -> Option<&(dyn Error + 'static)> {
        match &self.kind {
            RdfXmlErrorKind::Xml(quick_xml::Error::Io(error)) => Some(error),
            RdfXmlErrorKind::Xml(quick_xml::Error::Utf8(error)) => Some(error),
            _ => None,
        }
    }
}

impl ParseError for RdfXmlError {
    fn textual_position(&self) -> Option<LineBytePosition> {
        None
    }
}

impl From<quick_xml::Error> for RdfXmlError {
    fn from(error: quick_xml::Error) -> Self {
        Self {
            kind: RdfXmlErrorKind::Xml(error),
        }
    }
}

impl From<String> for RdfXmlError {
    // TODO: remove
    fn from(error: String) -> Self {
        Self {
            kind: RdfXmlErrorKind::Other(error),
        }
    }
}

impl From<&str> for RdfXmlError {
    // TODO: remove
    fn from(error: &str) -> Self {
        Self {
            kind: RdfXmlErrorKind::Other(error.to_owned()),
        }
    }
}
