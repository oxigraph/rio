use crate::error::*;
use rio_api::parser::LineBytePosition;
use std::io::BufRead;
use std::str;
use std::u8;

pub const EOF: u8 = u8::MAX;

/// An interface for the parsers input providing a lot of utilities
pub trait LookAheadByteRead {
    /// Returns the current byte or EOF if the file is finished
    fn current(&self) -> u8;

    /// Returns the next byte if available
    fn next(&self) -> Option<u8> {
        self.ahead(1)
    }

    /// Returns a future byte if available
    fn ahead(&self, count: usize) -> Option<u8>;

    /// Consumes the current char and moves to the next one
    fn consume(&mut self) -> Result<(), TurtleError>;

    /// Consumes the many chars and moves to the next one
    fn consume_many(&mut self, count: usize) -> Result<(), TurtleError>;

    /// Returns the line number of the current byte starting at 0
    fn line_number(&self) -> usize;

    /// Returns the byte number of the current byte in the line starting at 0
    fn byte_number(&self) -> usize;

    /// Returns if the current buffer starts with a given byte string. Does not work cross line boundaries
    fn starts_with(&self, prefix: &[u8]) -> bool;

    /// Returns if the current buffer starts with a given byte string in a ASCII case insensitive manner.
    /// Does not work cross line boundaries
    fn starts_with_ignore_ascii_case(&self, prefix: &[u8]) -> bool;

    fn unexpected_char_error<T>(&self) -> Result<T, TurtleError> {
        Err(self.parse_error(if self.current() == EOF {
            TurtleErrorKind::PrematureEOF
        } else {
            TurtleErrorKind::UnexpectedByte(self.current())
        }))
    }

    fn check_is_current(&self, expected: u8) -> Result<(), TurtleError> {
        if self.current() == expected {
            Ok(())
        } else {
            self.unexpected_char_error()
        }
    }

    fn parse_error(&self, kind: TurtleErrorKind) -> TurtleError {
        TurtleError {
            kind,
            position: Some(LineBytePosition::new(
                self.line_number(),
                self.byte_number(),
            )),
        }
    }
}

/// Reads the file line by line in a streaming way
pub struct LookAheadLineBasedByteReader<R: BufRead> {
    inner: R,
    line: Vec<u8>,
    current: u8,
    line_number: usize,
    byte_number: usize,
}

impl<R: BufRead> LookAheadLineBasedByteReader<R> {
    pub fn new(inner: R) -> Result<Self, TurtleError> {
        let mut this = Self {
            inner,
            line: Vec::default(),
            current: EOF,
            line_number: 0,
            byte_number: 0,
        };
        // We fill current and next with the appropriate values and we reset properly the line and byte numbers
        this.consume()?;
        this.line_number = 0;
        Ok(this)
    }
}

impl<R: BufRead> LookAheadByteRead for LookAheadLineBasedByteReader<R> {
    fn current(&self) -> u8 {
        self.current
    }

    fn ahead(&self, count: usize) -> Option<u8> {
        self.line.get(self.byte_number + count).cloned()
    }

    fn consume(&mut self) -> Result<(), TurtleError> {
        //TODO: define from consume many?
        self.byte_number += 1;
        if self.byte_number >= self.line.len() {
            self.line.clear();
            self.inner.read_until(b'\n', &mut self.line)?;
            self.line_number += 1;
            self.byte_number = 0;
        }
        self.current = self.line.get(self.byte_number).cloned().unwrap_or(EOF);
        Ok(())
    }

    fn consume_many(&mut self, count: usize) -> Result<(), TurtleError> {
        self.byte_number += count;
        while self.byte_number >= self.line.len() && !self.line.is_empty() {
            self.byte_number -= self.line.len();
            self.line.clear();
            self.inner.read_until(b'\n', &mut self.line)?;
            self.line_number += 1;
        }
        self.current = self.line.get(self.byte_number).cloned().unwrap_or(EOF);
        Ok(())
    }

    fn line_number(&self) -> usize {
        self.line_number
    }

    fn byte_number(&self) -> usize {
        self.byte_number + 1
    }

    fn starts_with(&self, prefix: &[u8]) -> bool {
        let end = self.byte_number + prefix.len();
        if end < self.line.len() {
            self.line[self.byte_number..end].eq(prefix)
        } else {
            false
        }
    }

    fn starts_with_ignore_ascii_case(&self, prefix: &[u8]) -> bool {
        let end = self.byte_number + prefix.len();
        if end < self.line.len() {
            self.line[self.byte_number..end].eq_ignore_ascii_case(prefix)
        } else {
            false
        }
    }
}

#[derive(Default)]
pub struct StringBufferStack {
    inner: Vec<String>,
    len: usize,
}

impl StringBufferStack {
    pub fn push(&mut self) -> &mut String {
        self.len += 1;
        if self.len > self.inner.len() {
            self.inner.push(String::default())
        }
        &mut self.inner[self.len - 1]
    }

    pub fn pop(&mut self) {
        self.inner[self.len - 1].clear();
        self.len -= 1;
    }

    pub fn last(&self) -> &str {
        &self.inner[self.len - 1]
    }

    pub fn before_last(&self) -> &str {
        &self.inner[self.len - 2]
    }
}

#[derive(Default)]
pub struct BlankNodeIdGenerator {
    //TODO: avoid collisions
    counter: usize,
}

impl BlankNodeIdGenerator {
    pub fn generate(&mut self) -> BlankNodeId {
        let mut id: [u8; 12] = [
            b'r', b'i', b'o', b'g', b'0', b'0', b'0', b'0', b'0', b'0', b'0', b'0',
        ];
        self.counter += 1;
        write_usize_to_slice(self.counter, &mut id[4..]);
        BlankNodeId { id }
    }
}

fn write_usize_to_slice(mut v: usize, s: &mut [u8]) {
    for i in (0..s.len()).rev() {
        s[i] = b'0' + (v % 10) as u8;
        v /= 10;
    }
}

#[derive(Eq, PartialEq, Copy, Clone, Hash)]
pub struct BlankNodeId {
    id: [u8; 12],
}

impl AsRef<str> for BlankNodeId {
    fn as_ref(&self) -> &str {
        // We know what id is and it's always valid UTF8
        unsafe { str::from_utf8_unchecked(&self.id) }
    }
}
