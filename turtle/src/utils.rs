use crate::error::*;
use rio_api::parser::LineBytePosition;
use std::collections::VecDeque;
use std::io::{BufRead, ErrorKind, Read};
use std::str;
use std::u8;

/// An interface for the parsers input providing a lot of utilities
pub trait LookAheadByteRead {
    /// Returns the current byte if it exists
    fn current(&self) -> Option<u8>;

    /// Returns the current byte if it exists or fail if it does not
    fn required_current(&self) -> Result<u8, TurtleError> {
        self.current()
            .ok_or_else(|| self.parse_error(TurtleErrorKind::PrematureEOF))
    }

    /// Returns the next byte if it exists
    fn next(&mut self) -> Result<Option<u8>, TurtleError> {
        self.ahead(1)
    }

    /// Returns a future byte if it exists
    fn ahead(&mut self, count: usize) -> Result<Option<u8>, TurtleError>;

    /// Consumes the current char and moves to the next one
    fn consume(&mut self) -> Result<(), TurtleError>;

    /// Consumes the many chars and moves to the next one
    fn consume_many(&mut self, count: usize) -> Result<(), TurtleError>;

    /// Returns the line number of the current byte starting at 1
    fn line_number(&self) -> usize;

    /// Returns the byte number of the current byte in the line starting at 1
    fn byte_number(&self) -> usize;

    /// Returns if the current buffer starts with a given byte string. Does not work cross line boundaries
    fn starts_with(&mut self, prefix: &[u8]) -> bool;

    /// Returns if the current buffer starts with a given byte string in a ASCII case insensitive manner.
    /// Does not work cross line boundaries
    fn starts_with_ignore_ascii_case(&mut self, prefix: &[u8]) -> bool;

    fn unexpected_char_error<T>(&self) -> Result<T, TurtleError> {
        Err(self.parse_error(match self.current() {
            Some(c) => TurtleErrorKind::UnexpectedByte(c),
            None => TurtleErrorKind::PrematureEOF,
        }))
    }

    fn check_is_current(&self, expected: u8) -> Result<(), TurtleError> {
        if self.current() == Some(expected) {
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

    fn consume_line_end(&mut self) -> Result<(), TurtleError> {
        loop {
            if let Some(b'\n') | None = self.current() {
                return self.consume();
            }
            self.consume()?;
        }
    }
}

/// Reads the file in a streaming way
pub struct LookAheadByteReader<R: Read> {
    inner: R,
    buffer: VecDeque<u8>,
    current: Option<u8>,
    line_number: usize,
    byte_number: usize,
}

const DEFAULT_CAPACITY: usize = 8 * 1024;

impl<R: BufRead> LookAheadByteReader<R> {
    pub fn new(inner: R) -> Self {
        let mut buffer = VecDeque::with_capacity(DEFAULT_CAPACITY);
        buffer.push_back(b'\n');
        Self {
            inner,
            buffer,
            current: Some(b'\n'),
            line_number: 0,
            byte_number: 1,
        }
    }

    fn fill_and_is_end(&mut self) -> Result<bool, TurtleError> {
        loop {
            let mut buf = [0; DEFAULT_CAPACITY]; //TODO: increase?
            match self.inner.read(&mut buf) {
                Ok(0) => return Ok(true),
                Ok(read) => {
                    self.buffer.extend(buf[..read].iter());
                    return Ok(false);
                }
                Err(e) if e.kind() == ErrorKind::Interrupted => {}
                Err(e) => return Err(e.into()),
            }
        }
    }

    fn starts_with_with_eq(&mut self, prefix: &[u8], eq: impl Fn(&[u8], &[u8]) -> bool) -> bool {
        loop {
            let (first, second) = self.buffer.as_slices();
            if prefix.len() <= first.len() {
                return eq(&first[..prefix.len()], prefix);
            } else if prefix.len() <= first.len() + second.len() {
                return eq(first, &prefix[..first.len()])
                    && eq(
                        &second[..prefix.len() - first.len()],
                        &prefix[first.len()..],
                    );
            }
            if let Ok(true) | Err(_) = self.fill_and_is_end() {
                return false;
            }
        }
    }
}

impl<R: BufRead> LookAheadByteRead for LookAheadByteReader<R> {
    fn current(&self) -> Option<u8> {
        self.current
    }

    fn ahead(&mut self, count: usize) -> Result<Option<u8>, TurtleError> {
        loop {
            let mut position = count;
            let (first, second) = self.buffer.as_slices();
            if position < first.len() {
                return Ok(Some(first[position]));
            }
            position -= first.len();
            if position < second.len() {
                return Ok(Some(second[position]));
            }
            if self.fill_and_is_end()? {
                return Ok(None);
            }
        }
    }

    fn consume(&mut self) -> Result<(), TurtleError> {
        self.consume_many(1)
    }

    fn consume_many(&mut self, count: usize) -> Result<(), TurtleError> {
        for _ in 0..count {
            if self.buffer.is_empty() {
                self.fill_and_is_end()?;
            }
            if let Some(c) = self.buffer.pop_front() {
                if c == b'\n' {
                    self.line_number += 1;
                    self.byte_number = 1;
                } else {
                    self.byte_number += 1;
                }
            }
        }
        if self.buffer.is_empty() {
            self.fill_and_is_end()?;
        }
        self.current = self.buffer.front().cloned();
        Ok(())
    }

    fn line_number(&self) -> usize {
        self.line_number
    }

    fn byte_number(&self) -> usize {
        self.byte_number
    }

    fn starts_with(&mut self, prefix: &[u8]) -> bool {
        self.starts_with_with_eq(prefix, |a, b| a == b)
    }

    fn starts_with_ignore_ascii_case(&mut self, prefix: &[u8]) -> bool {
        self.starts_with_with_eq(prefix, |a, b| a.eq_ignore_ascii_case(b))
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
        str::from_utf8(&self.id).unwrap()
    }
}
