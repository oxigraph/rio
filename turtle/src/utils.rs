use crate::error::*;
use std::io::BufRead;
use std::u8;

pub const EOF: u8 = u8::MAX;

/// Reads the file line by line in a streaming way
pub trait OneLookAheadLineByteRead {
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

    /// Returns the line number of the current byte starting at 1
    fn line_number(&self) -> usize;

    /// Returns the byte number of the current byte in the line starting at 1
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
            line_number: self.line_number(),
            byte_number: self.byte_number(),
        }
    }
}

pub struct OneLookAheadLineByteReader<R: BufRead> {
    inner: R,
    line: Vec<u8>,
    current: u8,
    line_number: usize,
    byte_number: usize,
}

impl<R: BufRead> OneLookAheadLineByteReader<R> {
    pub fn new(inner: R) -> Result<Self, TurtleError> {
        let mut this = Self {
            inner,
            line: Vec::default(),
            current: EOF,
            line_number: 1,
            byte_number: 0,
        };
        // We fill current and next with the appropriate values and we reset properly the line and byte numbers
        this.consume()?;
        this.line_number = 1;
        Ok(this)
    }
}

impl<R: BufRead> OneLookAheadLineByteRead for OneLookAheadLineByteReader<R> {
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

pub trait PushChar {
    fn push_char(&mut self, c: char);
}

impl PushChar for Vec<u8> {
    fn push_char(&mut self, c: char) {
        match c.len_utf8() {
            1 => self.push(c as u8),
            _ => self.extend_from_slice(c.encode_utf8(&mut [0; 4]).as_bytes()),
        }
    }
}

#[derive(Default)]
pub struct BufferStack<T> {
    inner: Vec<Vec<T>>,
    len: usize,
}

impl<T> BufferStack<T> {
    pub fn push(&mut self) -> &mut Vec<T> {
        self.len += 1;
        if self.len > self.inner.len() {
            self.inner.push(Vec::default())
        }
        &mut self.inner[self.len - 1]
    }

    pub fn pop(&mut self) {
        self.inner[self.len - 1].clear();
        self.len -= 1;
    }

    pub fn last(&self) -> &[T] {
        self.inner[self.len - 1].as_slice()
    }

    pub fn before_last(&self) -> &[T] {
        self.inner[self.len - 2].as_slice()
    }
}

#[derive(Default)]
pub struct BlankNodeIdGenerator {
    //TODO: avoid collisions
    counter: usize,
}

impl BlankNodeIdGenerator {
    pub fn generate(&mut self) -> [u8; 12] {
        let mut id: [u8; 12] = [
            b'r', b'i', b'o', b'g', b'0', b'0', b'0', b'0', b'0', b'0', b'0', b'0',
        ];
        self.counter += 1;
        write_usize_to_slice(self.counter, &mut id[4..]);
        id
    }
}

fn write_usize_to_slice(mut v: usize, s: &mut [u8]) {
    for i in (0..s.len()).rev() {
        s[i] = b'0' + (v % 10) as u8;
        v /= 10;
    }
}
