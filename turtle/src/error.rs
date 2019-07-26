use std::char;
use std::error::Error;
use std::fmt;
use std::io;

/// Error that might be returned during parsing.
///
/// It might wrap an IO error or be a parsing error.
#[derive(Debug)]
pub struct TurtleError {
    pub(crate) kind: TurtleErrorKind,
    pub(crate) line_number: usize,
    pub(crate) byte_number: usize,
}

#[derive(Debug)]
pub enum TurtleErrorKind {
    IO(io::Error),
    PrematureEOF,
    UnexpectedByte(u8),
    InvalidUnicodeCodePoint(u32),
    InvalidUTF8,
    InvalidBaseIRI, //TODO: remove
    InvalidIRI,
}

impl fmt::Display for TurtleError {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match &self.kind {
            TurtleErrorKind::IO(error) => return error.fmt(f),
            TurtleErrorKind::PrematureEOF => write!(f, "premature end of file"),
            TurtleErrorKind::UnexpectedByte(c) => match char::from_u32(u32::from(*c)) {
                Some(c) => write!(f, "unexpected character '{}'", c.escape_debug()),
                None => write!(f, "unexpected byter {}", c),
            },
            TurtleErrorKind::InvalidUnicodeCodePoint(point) => {
                write!(f, "invalid unicode code point '{}'", point)
            }
            TurtleErrorKind::InvalidUTF8 => write!(f, "invalid UTF-8 encoding"),
            TurtleErrorKind::InvalidBaseIRI => write!(f, "invalid base URI"),
            TurtleErrorKind::InvalidIRI => write!(f, "invalid URI"),
        }?;
        write!(
            f,
            " on line {} at position {}",
            self.line_number, self.byte_number
        )
    }
}

impl Error for TurtleError {
    fn source(&self) -> Option<&(dyn Error + 'static)> {
        match &self.kind {
            TurtleErrorKind::IO(error) => Some(error),
            _ => None,
        }
    }
}

impl From<io::Error> for TurtleError {
    fn from(error: io::Error) -> Self {
        Self {
            kind: TurtleErrorKind::IO(error),
            line_number: 0,
            byte_number: 0,
        }
    }
}
