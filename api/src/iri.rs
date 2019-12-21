//! Utilities to validate and resolve IRIs following [RFC 3987](https://www.ietf.org/rfc/rfc3987).
//!
//! Example:
//! ```
//! use rio_api::iri::Iri;
//!
//! // Parse and validate base IRI
//! let base_iri = Iri::parse("http://foo.com/bar/baz").unwrap();
//!
//! // Validate and resolve relative IRI
//! let iri = base_iri.resolve("bat#foo").unwrap();
//! assert_eq!("http://foo.com/bar/bat#foo", iri.into_inner())
//! ```

use std::error::Error;
use std::fmt;
use std::net::{AddrParseError, Ipv6Addr};
use std::ops::Deref;
use std::str::{Chars, FromStr};

/// A [RFC 3987](https://www.ietf.org/rfc/rfc3987) IRI.
///
/// Example:
/// ```
/// use rio_api::iri::Iri;
///
/// // Parse and validate base IRI
/// let base_iri = Iri::parse("http://foo.com/bar/baz").unwrap();
///
/// // Validate and resolve relative IRI
/// let iri = base_iri.resolve("bat#foo").unwrap();
/// assert_eq!("http://foo.com/bar/bat#foo", iri.into_inner());
/// ```
#[derive(Eq, PartialEq, Debug, Clone, Hash)]
pub struct Iri<T: Deref<Target = str>> {
    iri: T,
    positions: IriElementsPositions,
}

impl<T: Deref<Target = str>> Iri<T> {
    /// Parses and validates the IRI following [RFC 3987](https://www.ietf.org/rfc/rfc3987) IRI syntax.
    ///
    /// This operation keeps internally the `iri` parameter and does not allocate.
    ///
    /// Warning: validation is not fully implemented yet!
    ///
    /// Example:
    /// ```
    /// use rio_api::iri::Iri;
    /// Iri::parse("http://foo.com/bar/baz").unwrap();
    /// ```
    pub fn parse(iri: T) -> Result<Self, IriParseError> {
        let positions = IriParser::parse(
            &iri,
            None as Option<&Iri<&str>>,
            &mut VoidOutputBuffer::default(),
        )?;
        Ok(Self { iri, positions })
    }

    /// Validates and resolved a relative IRI against the current IRI
    /// following [RFC 3987](https://www.ietf.org/rfc/rfc3986) relative URI resolution algorithm.
    ///
    /// Example:
    /// ```
    /// use rio_api::iri::Iri;
    /// let base_iri = Iri::parse("http://foo.com/bar/baz").unwrap();
    /// let iri = base_iri.resolve("bat#foo").unwrap();
    /// assert_eq!("http://foo.com/bar/bat#foo", iri.into_inner());
    /// ```
    pub fn resolve(&self, iri: &str) -> Result<Iri<String>, IriParseError> {
        let mut target_buffer = String::with_capacity(self.iri.len() + iri.len());
        let positions = IriParser::parse(iri, Some(&self), &mut target_buffer)?;
        Ok(Iri {
            iri: target_buffer,
            positions,
        })
    }

    /// Validates and resolved a relative IRI against the current IRI
    /// following [RFC 3986](https://www.ietf.org/rfc/rfc3986) relative URI resolution algorithm.
    ///
    /// It outputs the resolved IRI into `target_buffer` to avoid any memory allocation.
    ///
    /// Example:
    /// ```
    /// use rio_api::iri::Iri;
    /// let base_iri = Iri::parse("http://foo.com/bar/baz").unwrap();
    /// let mut result = String::default();
    /// let iri = base_iri.resolve_into("bat#foo", &mut result).unwrap();
    /// assert_eq!("http://foo.com/bar/bat#foo", result);
    /// ```
    pub fn resolve_into(&self, iri: &str, target_buffer: &mut String) -> Result<(), IriParseError> {
        IriParser::parse(iri, Some(&self), target_buffer)?;
        Ok(())
    }

    /// Returns the underlying IRI representation.
    #[inline]
    pub fn as_str(&self) -> &str {
        &self.iri
    }

    /// Returns the underlying IRI representation.
    #[inline]
    pub fn into_inner(self) -> T {
        self.iri
    }
}

impl<T: Deref<Target = str> + fmt::Display> fmt::Display for Iri<T> {
    #[inline]
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        self.iri.fmt(f)
    }
}

/// An error raised during `Iri` validation.
#[derive(Debug)]
pub struct IriParseError {
    iri: String,
    kind: IriParseErrorKind,
}

impl fmt::Display for IriParseError {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match &self.kind {
            IriParseErrorKind::NoScheme => write!(f, "No scheme found in IRI: {}", self.iri),
            IriParseErrorKind::InvalidHostCharacter(c) => write!(
                f,
                "Invalid character '{}' found in IRI host: {}",
                c, self.iri
            ),
            IriParseErrorKind::InvalidHostIp(e) => {
                write!(f, "Invalid host IP ({}) found in IRI: {}", e, self.iri)
            }
            IriParseErrorKind::InvalidPortCharacter(c) => write!(
                f,
                "Invalid character '{}' found in IRI port: {}",
                c, self.iri
            ),
            IriParseErrorKind::InvalidIriCodePoint(c) => {
                write!(f, "Invalid IRI code point '{}' in {}", c, self.iri)
            }
            IriParseErrorKind::InvalidPercentEncoding(cs) => write!(
                f,
                "Invalid IRI percent encoding '{}' in {}",
                cs.iter().flatten().cloned().collect::<String>(),
                self.iri
            ),
        }
    }
}

impl Error for IriParseError {
    fn source(&self) -> Option<&(dyn Error + 'static)> {
        if let IriParseErrorKind::InvalidHostIp(e) = &self.kind {
            Some(e)
        } else {
            None
        }
    }
}

#[derive(Debug)]
enum IriParseErrorKind {
    NoScheme,
    InvalidHostCharacter(char),
    InvalidHostIp(AddrParseError),
    InvalidPortCharacter(char),
    InvalidIriCodePoint(char),
    InvalidPercentEncoding([Option<char>; 3]),
}

#[derive(Eq, PartialEq, Debug, Clone, Hash)]
struct IriElementsPositions {
    scheme_end: usize,
    authority_end: usize,
    path_end: usize,
    query_end: usize,
    cannot_be_a_base: bool,
}

trait OutputBuffer {
    fn push(&mut self, c: char);

    fn push_str(&mut self, s: &str);

    fn clear(&mut self);

    fn truncate(&mut self, new_len: usize);

    fn len(&self) -> usize;

    fn as_str(&self) -> &str;
}

#[derive(Default)]
struct VoidOutputBuffer {
    len: usize,
}

impl OutputBuffer for VoidOutputBuffer {
    #[inline]
    fn push(&mut self, c: char) {
        self.len += c.len_utf8();
    }

    #[inline]
    fn push_str(&mut self, s: &str) {
        self.len += s.len();
    }

    #[inline]
    fn clear(&mut self) {
        self.len = 0;
    }

    #[inline]
    fn truncate(&mut self, new_len: usize) {
        self.len = new_len;
    }

    #[inline]
    fn len(&self) -> usize {
        self.len
    }

    #[inline]
    fn as_str(&self) -> &str {
        ""
    }
}

impl OutputBuffer for String {
    #[inline]
    fn push(&mut self, c: char) {
        self.push(c);
    }

    #[inline]
    fn push_str(&mut self, s: &str) {
        self.push_str(s);
    }

    #[inline]
    fn clear(&mut self) {
        self.clear();
    }

    #[inline]
    fn truncate(&mut self, new_len: usize) {
        self.truncate(new_len);
    }

    #[inline]
    fn len(&self) -> usize {
        self.len()
    }

    #[inline]
    fn as_str(&self) -> &str {
        self.as_str()
    }
}

struct ParserInput<'a> {
    value: Chars<'a>,
    position: usize,
}
impl<'a> ParserInput<'a> {
    #[inline]
    fn next(&mut self) -> Option<char> {
        if let Some(head) = self.value.next() {
            self.position += head.len_utf8();
            Some(head)
        } else {
            None
        }
    }

    #[inline]
    fn front(&self) -> Option<char> {
        self.value.clone().next()
    }

    #[inline]
    fn starts_with(&self, c: char) -> bool {
        self.value.as_str().starts_with(c)
    }
}

/// parser implementing https://url.spec.whatwg.org/#concept-basic-url-parser without the normalization or backward compatibility bits to comply with RFC 3987
///
/// A sub function takes care of each state
struct IriParser<'a, BC: Deref<Target = str>, O: OutputBuffer> {
    iri: &'a str,
    base: Option<&'a Iri<BC>>,
    input: ParserInput<'a>,
    output: &'a mut O,
    output_positions: IriElementsPositions,
    input_scheme_end: usize,
}

impl<'a, BC: Deref<Target = str>, O: OutputBuffer> IriParser<'a, BC, O> {
    fn parse(
        iri: &'a str,
        base: Option<&'a Iri<BC>>,
        output: &'a mut O,
    ) -> Result<IriElementsPositions, IriParseError> {
        let mut parser = Self {
            iri,
            base,
            input: ParserInput {
                value: iri.chars(),
                position: 0,
            },
            output,
            output_positions: IriElementsPositions {
                scheme_end: 0,
                authority_end: 0,
                path_end: 0,
                query_end: 0,
                cannot_be_a_base: false,
            },
            input_scheme_end: 0,
        };
        parser.parse_scheme_start()?;
        Ok(parser.output_positions)
    }

    fn parse_scheme_start(&mut self) -> Result<(), IriParseError> {
        match self.input.front() {
            Some(c) if c.is_ascii_alphabetic() => self.parse_scheme(),
            _ => self.parse_no_scheme(),
        }
    }

    fn parse_scheme(&mut self) -> Result<(), IriParseError> {
        loop {
            let c = self.input.next();
            match c {
                Some(c) if c.is_ascii_alphanumeric() || c == '+' || c == '-' || c == '.' => {
                    self.output.push(c)
                }
                Some(':') => {
                    self.output.push(':');
                    self.output_positions.scheme_end = self.output.len();
                    self.input_scheme_end = self.input.position;
                    return if self.input.starts_with('/') {
                        self.input.next();
                        self.output.push('/');
                        self.parse_path_or_authority()
                    } else {
                        self.output_positions.authority_end = self.output.len();
                        self.output_positions.cannot_be_a_base = true;
                        self.parse_cannot_be_a_base()
                    };
                }
                _ => {
                    self.input = ParserInput {
                        value: self.iri.chars(),
                        position: 0,
                    }; // reset
                    self.output.clear();
                    return self.parse_no_scheme();
                }
            }
        }
    }

    fn parse_no_scheme(&mut self) -> Result<(), IriParseError> {
        if let Some(base) = self.base {
            if base.positions.cannot_be_a_base {
                if self.input.starts_with('#') {
                    self.output.push_str(&base.iri[..base.positions.query_end]);
                    self.output.push('#');
                    self.output_positions.scheme_end = base.positions.scheme_end;
                    self.output_positions.authority_end = base.positions.authority_end;
                    self.output_positions.path_end = base.positions.path_end;
                    self.output_positions.query_end = base.positions.query_end;
                    self.output_positions.cannot_be_a_base = true;
                    self.parse_fragment()
                } else {
                    self.parse_error(IriParseErrorKind::NoScheme)
                }
            } else {
                self.parse_relative()
            }
        } else {
            self.parse_error(IriParseErrorKind::NoScheme)
        }
    }

    fn parse_path_or_authority(&mut self) -> Result<(), IriParseError> {
        if self.input.starts_with('/') {
            self.input.next();
            self.output.push('/');
            self.parse_authority()
        } else {
            self.output_positions.authority_end = self.output.len() - 1;
            self.parse_path()
        }
    }

    fn parse_relative(&mut self) -> Result<(), IriParseError> {
        let base = self.base.unwrap();
        match self.input.front() {
            None => {
                self.output.push_str(&base.iri[..base.positions.query_end]);
                self.output_positions.scheme_end = base.positions.scheme_end;
                self.output_positions.authority_end = base.positions.authority_end;
                self.output_positions.path_end = base.positions.path_end;
                self.output_positions.query_end = base.positions.query_end;
                Ok(())
            }
            Some('/') => {
                self.input.next();
                self.parse_relative_slash()
            }
            Some('?') => {
                self.input.next();
                self.output.push_str(&base.iri[..base.positions.path_end]);
                self.output.push('?');
                self.output_positions.scheme_end = base.positions.scheme_end;
                self.output_positions.authority_end = base.positions.authority_end;
                self.output_positions.path_end = base.positions.path_end;
                self.parse_query()
            }
            Some('#') => {
                self.input.next();
                self.output.push_str(&base.iri[..base.positions.query_end]);
                self.output_positions.scheme_end = base.positions.scheme_end;
                self.output_positions.authority_end = base.positions.authority_end;
                self.output_positions.path_end = base.positions.path_end;
                self.output_positions.query_end = base.positions.query_end;
                self.output.push('#');
                self.parse_fragment()
            }
            _ => {
                self.output.push_str(&base.iri[..base.positions.path_end]);
                self.output_positions.scheme_end = base.positions.scheme_end;
                self.output_positions.authority_end = base.positions.authority_end;
                self.output_positions.path_end = base.positions.path_end;
                self.remove_last_segment();
                self.output.push('/');
                self.parse_path()
            }
        }
    }

    fn parse_relative_slash(&mut self) -> Result<(), IriParseError> {
        let base = self.base.unwrap();
        if self.input.starts_with('/') {
            self.input.next();
            self.output.push_str(&base.iri[..base.positions.scheme_end]);
            self.output_positions.scheme_end = base.positions.scheme_end;
            self.output.push('/');
            self.output.push('/');
            self.parse_authority()
        } else {
            self.output
                .push_str(&base.iri[..base.positions.authority_end]);
            self.output.push('/');
            self.output_positions.scheme_end = base.positions.scheme_end;
            self.output_positions.authority_end = base.positions.authority_end;
            self.parse_path()
        }
    }

    fn parse_authority(&mut self) -> Result<(), IriParseError> {
        // @ are not allowed in IRI authorities so not need to take care of ambiguities
        loop {
            let c = self.input.next();
            match c {
                Some('@') => {
                    self.output.push('@');
                    return self.parse_host();
                }
                None | Some('[') | Some('/') | Some('?') | Some('#') => {
                    self.input = ParserInput {
                        value: self.iri[self.input_scheme_end + 2..].chars(),
                        position: self.input_scheme_end + 2,
                    };
                    self.output.truncate(self.output_positions.scheme_end + 2);
                    return self.parse_host();
                }
                Some(c) => {
                    self.read_url_codepoint_or_echar(c)?;
                    self.output.push(c);
                }
            }
        }
    }

    fn parse_host(&mut self) -> Result<(), IriParseError> {
        let mut in_bracket = false;
        loop {
            let c = self.input.next();
            match c {
                Some(':') if !in_bracket => {
                    self.validate_host(
                        &self.iri[self.input_scheme_end + 2..self.input.position - 1],
                    )?;
                    self.output.push(':');
                    return self.parse_port();
                }
                None | Some('/') | Some('?') | Some('#') => {
                    self.output_positions.authority_end = self.output.len();
                    self.validate_host(
                        &self.iri[self.input_scheme_end + 2
                            ..self.input.position - c.map_or(0, |c| c.len_utf8())],
                    )?;
                    return self.parse_path_start(c);
                }
                Some('[') => {
                    self.output.push('[');
                    in_bracket = true;
                }
                Some(']') => {
                    self.output.push(']');
                    in_bracket = false;
                }
                Some(c) => self.output.push(c),
            }
        }
    }

    fn parse_port(&mut self) -> Result<(), IriParseError> {
        loop {
            let c = self.input.next();
            match c {
                Some(c) if c.is_ascii_digit() => self.output.push(c),
                Some('/') | Some('?') | Some('#') | None => {
                    self.output_positions.authority_end = self.output.len();
                    return self.parse_path_start(c);
                }
                Some(c) => return self.parse_error(IriParseErrorKind::InvalidPortCharacter(c)),
            }
        }
    }

    fn parse_path_start(&mut self, c: Option<char>) -> Result<(), IriParseError> {
        match c {
            None => {
                self.output_positions.path_end = self.output.len();
                self.output_positions.query_end = self.output.len();
                Ok(())
            }
            Some('?') => {
                self.output_positions.path_end = self.output.len();
                self.output.push('?');
                self.parse_query()
            }
            Some('#') => {
                self.output_positions.path_end = self.output.len();
                self.output_positions.query_end = self.output.len();
                self.output.push('#');
                self.parse_fragment()
            }
            Some('/') => {
                self.output.push('/');
                self.parse_path()
            }
            Some(c) => {
                self.read_url_codepoint_or_echar(c)?;
                self.output.push(c);
                self.parse_path()
            }
        }
    }

    fn parse_path(&mut self) -> Result<(), IriParseError> {
        loop {
            let c = self.input.next();
            match c {
                None | Some('/') | Some('?') | Some('#') => {
                    if self.output.as_str().ends_with("/..") {
                        self.remove_last_segment();
                        self.remove_last_segment();
                        self.output.push('/');
                    } else if self.output.as_str().ends_with("/.") {
                        self.remove_last_segment();
                        self.output.push('/');
                    } else if c == Some('/') {
                        self.output.push('/');
                    }

                    if c == Some('?') {
                        self.output_positions.path_end = self.output.len();
                        self.output.push('?');
                        return self.parse_query();
                    } else if c == Some('#') {
                        self.output_positions.path_end = self.output.len();
                        self.output_positions.query_end = self.output.len();
                        self.output.push('#');
                        return self.parse_fragment();
                    } else if c == None {
                        self.output_positions.path_end = self.output.len();
                        self.output_positions.query_end = self.output.len();
                        return Ok(());
                    }
                }
                Some(c) => self.read_url_codepoint_or_echar(c)?,
            }
        }
    }

    fn parse_cannot_be_a_base(&mut self) -> Result<(), IriParseError> {
        while let Some(c) = self.input.next() {
            if c == '?' {
                self.output_positions.path_end = self.output.len();
                self.output.push('?');
                return self.parse_query();
            } else if c == '#' {
                self.output_positions.path_end = self.output.len();
                self.output_positions.query_end = self.output.len();
                self.output.push('#');
                return self.parse_fragment();
            } else {
                self.read_url_codepoint_or_echar(c)?
            }
        }
        self.output_positions.path_end = self.output.len();
        self.output_positions.query_end = self.output.len();
        Ok(())
    }

    fn parse_query(&mut self) -> Result<(), IriParseError> {
        while let Some(c) = self.input.next() {
            if c == '#' {
                self.output_positions.query_end = self.output.len();
                self.output.push('#');
                return self.parse_fragment();
            } else {
                self.read_url_codepoint_or_echar(c)?
            }
        }
        self.output_positions.query_end = self.output.len();
        Ok(())
    }

    fn parse_fragment(&mut self) -> Result<(), IriParseError> {
        while let Some(c) = self.input.next() {
            self.read_url_codepoint_or_echar(c)?
        }
        Ok(())
    }

    fn remove_last_segment(&mut self) {
        if let Some(last_slash_position) =
            self.output.as_str()[self.output_positions.authority_end..].rfind('/')
        {
            self.output
                .truncate(last_slash_position + self.output_positions.authority_end)
        }
    }

    #[inline]
    fn read_url_codepoint_or_echar(&mut self, c: char) -> Result<(), IriParseError> {
        if c == '%' {
            let c1 = self.input.next();
            let c2 = self.input.next();
            if c1.map_or(false, |c| c.is_ascii_hexdigit())
                && c2.map_or(false, |c| c.is_ascii_hexdigit())
            {
                self.output.push('%');
                self.output.push(c1.unwrap());
                self.output.push(c2.unwrap());
                Ok(())
            } else {
                self.parse_error(IriParseErrorKind::InvalidPercentEncoding([
                    Some('%'),
                    c1,
                    c2,
                ]))
            }
        } else if is_url_code_point(c) {
            self.output.push(c);
            Ok(())
        } else {
            self.parse_error(IriParseErrorKind::InvalidIriCodePoint(c))
        }
    }

    fn validate_host(&self, host: &str) -> Result<(), IriParseError> {
        if host.starts_with('[') {
            if host.ends_with(']') {
                match Ipv6Addr::from_str(&host[1..host.len() - 1]) {
                    Ok(_) => Ok(()),
                    Err(e) => self.parse_error(IriParseErrorKind::InvalidHostIp(e)),
                }
            } else {
                self.parse_error(IriParseErrorKind::InvalidHostCharacter(
                    host.chars().last().unwrap(),
                ))
            }
        } else {
            for c in host.chars() {
                match c {
                    '\0' | '\t' | '\n' | '\r' | ' ' | '#' | '/' | ':' | '?' | '@' | '[' | '\\'
                    | ']' => return self.parse_error(IriParseErrorKind::InvalidHostCharacter(c)),
                    _ => (),
                }
            }
            Ok(())
        }
    }

    fn parse_error<T>(&self, kind: IriParseErrorKind) -> Result<T, IriParseError> {
        Err(IriParseError {
            iri: self.iri.to_owned(),
            kind,
        })
    }
}

fn is_url_code_point(c: char) -> bool {
    match c {
        'a'..='z'
        | 'A'..='Z'
        | '0'..='9'
        | '!'
        | '$'
        | '&'
        | '\''
        | '('
        | ')'
        | '*'
        | '+'
        | ','
        | '-'
        | '.'
        | '/'
        | ':'
        | ';'
        | '='
        | '?'
        | '@'
        | '_'
        | '~'
        | '\u{A0}'..='\u{D7FF}'
        | '\u{E000}'..='\u{FDCF}'
        | '\u{FDF0}'..='\u{FFFD}'
        | '\u{10000}'..='\u{1FFFD}'
        | '\u{20000}'..='\u{2FFFD}'
        | '\u{30000}'..='\u{3FFFD}'
        | '\u{40000}'..='\u{4FFFD}'
        | '\u{50000}'..='\u{5FFFD}'
        | '\u{60000}'..='\u{6FFFD}'
        | '\u{70000}'..='\u{7FFFD}'
        | '\u{80000}'..='\u{8FFFD}'
        | '\u{90000}'..='\u{9FFFD}'
        | '\u{A0000}'..='\u{AFFFD}'
        | '\u{B0000}'..='\u{BFFFD}'
        | '\u{C0000}'..='\u{CFFFD}'
        | '\u{D0000}'..='\u{DFFFD}'
        | '\u{E1000}'..='\u{EFFFD}'
        | '\u{F0000}'..='\u{FFFFD}'
        | '\u{100000}'..='\u{10FFFD}' => true,
        _ => false,
    }
}

#[test]
fn test_parsing() {
    let examples = [
        "file://foo",
        "ftp://ftp.is.co.za/rfc/rfc1808.txt",
        "http://www.ietf.org/rfc/rfc2396.txt",
        "ldap://[2001:db8::7]/c=GB?objectClass?one",
        "mailto:John.Doe@example.com",
        "news:comp.infosystems.www.servers.unix",
        "tel:+1-816-555-1212",
        "telnet://192.0.2.16:80/",
        "urn:oasis:names:specification:docbook:dtd:xml:4.1.2",
        "http://example.com",
        "http://example.com/",
        "http://example.com/foo",
        "http://example.com/foo/bar",
        "http://example.com/foo/bar/",
        "http://example.com/foo/bar?q=1&r=2",
        "http://example.com/foo/bar/?q=1&r=2",
        "http://example.com#toto",
        "http://example.com/#toto",
        "http://example.com/foo#toto",
        "http://example.com/foo/bar#toto",
        "http://example.com/foo/bar/#toto",
        "http://example.com/foo/bar?q=1&r=2#toto",
        "http://example.com/foo/bar/?q=1&r=2#toto",
        "http://example.com/foo/bar/.././baz",
    ];

    for e in examples.iter() {
        let result = Iri::parse(*e);
        assert!(result.is_ok(), "{}", result.unwrap_err());
    }
}

#[test]
fn test_relative_parsing() {
    // From https://sourceforge.net/projects/foursuite/ under Apache License

    let examples = [
        "file:///foo/bar",
        "mailto:user@host?subject=blah",
        "dav:",   // empty opaque part / rel-path allowed by RFC 2396bis
        "about:", // empty opaque part / rel-path allowed by RFC 2396bis
        //
        // the following test cases are from a Perl script by David A. Wheeler
        // at http://www.dwheeler.com/secure-programs/url.pl
        "http://www.yahoo.com",
        "http://www.yahoo.com/",
        "http://1.2.3.4/",
        "http://www.yahoo.com/stuff",
        "http://www.yahoo.com/stuff/",
        "http://www.yahoo.com/hello%20world/",
        "http://www.yahoo.com?name=obi",
        "http://www.yahoo.com?name=obi+wan&status=jedi",
        "http://www.yahoo.com?onery",
        "http://www.yahoo.com#bottom",
        "http://www.yahoo.com/yelp.html#bottom",
        "https://www.yahoo.com/",
        "ftp://www.yahoo.com/",
        "ftp://www.yahoo.com/hello",
        "demo.txt",
        "demo/hello.txt",
        "demo/hello.txt?query=hello#fragment",
        "/cgi-bin/query?query=hello#fragment",
        "/demo.txt",
        "/hello/demo.txt",
        "hello/demo.txt",
        "/",
        "",
        "#",
        "#here",
        // Wheeler"s script says these are invalid, but they aren"t
        "http://www.yahoo.com?name=%00%01",
        "http://www.yaho%6f.com",
        "http://www.yahoo.com/hello%00world/",
        "http://www.yahoo.com/hello+world/",
        "http://www.yahoo.com?name=obi&",
        "http://www.yahoo.com?name=obi&type=",
        "http://www.yahoo.com/yelp.html#",
        "//",
        // the following test cases are from a Haskell program by Graham Klyne
        // at http://www.ninebynine.org/Software/HaskellUtils/Network/URITest.hs
        "http://example.org/aaa/bbb#ccc",
        "mailto:local@domain.org",
        "mailto:local@domain.org#frag",
        "HTTP://EXAMPLE.ORG/AAA/BBB#CCC",
        "//example.org/aaa/bbb#ccc",
        "/aaa/bbb#ccc",
        "bbb#ccc",
        "#ccc",
        "#",
        "A'C",
        //-- escapes
        "http://example.org/aaa%2fbbb#ccc",
        "http://example.org/aaa%2Fbbb#ccc",
        "%2F",
        "aaa%2Fbbb",
        //-- ports
        "http://example.org:80/aaa/bbb#ccc",
        "http://example.org:/aaa/bbb#ccc",
        "http://example.org./aaa/bbb#ccc",
        "http://example.123./aaa/bbb#ccc",
        //-- bare authority
        "http://example.org",
        //-- IPv6 literals (from RFC2732):
        "http://[FEDC:BA98:7654:3210:FEDC:BA98:7654:3210]:80/index.html",
        "http://[1080:0:0:0:8:800:200C:417A]/index.html",
        "http://[3ffe:2a00:100:7031::1]",
        "http://[1080::8:800:200C:417A]/foo",
        "http://[::192.9.5.5]/ipng",
        "http://[::FFFF:129.144.52.38]:80/index.html",
        "http://[2010:836B:4179::836B:4179]",
        "//[2010:836B:4179::836B:4179]",
        //-- Random other things that crop up
        "http://example/Andr&#567;",
        "file:///C:/DEV/Haskell/lib/HXmlToolbox-3.01/examples/",
    ];

    let base = Iri::parse("http://a/b/c/d;p?q").unwrap();
    for e in examples.iter() {
        let result = base.resolve(*e);
        assert!(result.is_ok(), "{}", result.unwrap_err());
    }
}

#[test]
fn test_wrong_relative_parsing() {
    // From https://sourceforge.net/projects/foursuite/ under Apache License

    let examples = [
        "beepbeep\x07\x07",
        "\n",
        // "::", // not OK, per Roy Fielding on the W3C uri list on 2004-04-01
        //
        // the following test cases are from a Perl script by David A. Wheeler
        // at http://www.dwheeler.com/secure-programs/url.pl
        "http://www yahoo.com",
        "http://www.yahoo.com/hello world/",
        "http://www.yahoo.com/yelp.html#\"",
        //
        // the following test cases are from a Haskell program by Graham Klyne
        // at http://www.ninebynine.org/Software/HaskellUtils/Network/URITest.hs
        "[2010:836B:4179::836B:4179]",
        " ",
        "%",
        "A%Z",
        "%ZZ",
        "%AZ",
        "A C",
        // "A'C",
        "A`C",
        "A<C",
        "A>C",
        "A^C",
        "A\\C",
        "A{C",
        "A|C",
        "A}C",
        "A[C",
        "A]C",
        "A[**]C",
        "http://[xyz]/",
        "http://]/",
        "http://example.org/[2010:836B:4179::836B:4179]",
        "http://example.org/abc#[2010:836B:4179::836B:4179]",
        "http://example.org/xxx/[qwerty]#a[b]",
        //
        // from a post to the W3C uri list on 2004-02-17
        "http://w3c.org:80path1/path2",
    ];

    let base = Iri::parse("http://a/b/c/d;p?q").unwrap();
    for e in examples.iter() {
        let result = base.resolve(*e);
        assert!(result.is_err(), "{} is wrongly considered valid", e);
    }
}

#[test]
fn test_resolve_relative_iri() {
    // From https://sourceforge.net/projects/foursuite/ under Apache License

    let examples = [
        // http://lists.w3.org/Archives/Public/uri/2004Feb/0114.html
        ("/.", "http://a/b/c/d;p?q", "http://a/"),
        ("/.foo", "http://a/b/c/d;p?q", "http://a/.foo"),
        (".foo", "http://a/b/c/d;p?q", "http://a/b/c/.foo"),
        // http://gbiv.com/protocols/uri/test/rel_examples1.html
        // examples from RFC 2396
        ("g:h", "http://a/b/c/d;p?q", "g:h"),
        ("g", "http://a/b/c/d;p?q", "http://a/b/c/g"),
        ("./g", "http://a/b/c/d;p?q", "http://a/b/c/g"),
        ("g/", "http://a/b/c/d;p?q", "http://a/b/c/g/"),
        ("/g", "http://a/b/c/d;p?q", "http://a/g"),
        ("//g", "http://a/b/c/d;p?q", "http://g"),
        // changed with RFC 2396bis
        //("?y"      , "http://a/b/c/d;p?q", "http://a/b/c/d;p?y"),
        ("?y", "http://a/b/c/d;p?q", "http://a/b/c/d;p?y"),
        ("g?y", "http://a/b/c/d;p?q", "http://a/b/c/g?y"),
        // changed with RFC 2396bis
        //("#s"      , "http://a/b/c/d;p?q", CURRENT_DOC_URI + "#s"),
        ("#s", "http://a/b/c/d;p?q", "http://a/b/c/d;p?q#s"),
        ("g#s", "http://a/b/c/d;p?q", "http://a/b/c/g#s"),
        ("g?y#s", "http://a/b/c/d;p?q", "http://a/b/c/g?y#s"),
        (";x", "http://a/b/c/d;p?q", "http://a/b/c/;x"),
        ("g;x", "http://a/b/c/d;p?q", "http://a/b/c/g;x"),
        ("g;x?y#s", "http://a/b/c/d;p?q", "http://a/b/c/g;x?y#s"),
        // changed with RFC 2396bis
        //(""        , "http://a/b/c/d;p?q", CURRENT_DOC_URI),
        ("", "http://a/b/c/d;p?q", "http://a/b/c/d;p?q"),
        (".", "http://a/b/c/d;p?q", "http://a/b/c/"),
        ("./", "http://a/b/c/d;p?q", "http://a/b/c/"),
        ("..", "http://a/b/c/d;p?q", "http://a/b/"),
        ("../", "http://a/b/c/d;p?q", "http://a/b/"),
        ("../g", "http://a/b/c/d;p?q", "http://a/b/g"),
        ("../..", "http://a/b/c/d;p?q", "http://a/"),
        ("../../", "http://a/b/c/d;p?q", "http://a/"),
        ("../../g", "http://a/b/c/d;p?q", "http://a/g"),
        ("../../../g", "http://a/b/c/d;p?q", "http://a/g"),
        ("../../../../g", "http://a/b/c/d;p?q", "http://a/g"),
        // changed with RFC 2396bis
        ("/./g", "http://a/b/c/d;p?q", "http://a/g"),
        // changed with RFC 2396bis
        ("/../g", "http://a/b/c/d;p?q", "http://a/g"),
        ("g.", "http://a/b/c/d;p?q", "http://a/b/c/g."),
        (".g", "http://a/b/c/d;p?q", "http://a/b/c/.g"),
        ("g..", "http://a/b/c/d;p?q", "http://a/b/c/g.."),
        ("..g", "http://a/b/c/d;p?q", "http://a/b/c/..g"),
        ("./../g", "http://a/b/c/d;p?q", "http://a/b/g"),
        ("./g/.", "http://a/b/c/d;p?q", "http://a/b/c/g/"),
        ("g/./h", "http://a/b/c/d;p?q", "http://a/b/c/g/h"),
        ("g/../h", "http://a/b/c/d;p?q", "http://a/b/c/h"),
        ("g;x=1/./y", "http://a/b/c/d;p?q", "http://a/b/c/g;x=1/y"),
        ("g;x=1/../y", "http://a/b/c/d;p?q", "http://a/b/c/y"),
        ("g?y/./x", "http://a/b/c/d;p?q", "http://a/b/c/g?y/./x"),
        ("g?y/../x", "http://a/b/c/d;p?q", "http://a/b/c/g?y/../x"),
        ("g#s/./x", "http://a/b/c/d;p?q", "http://a/b/c/g#s/./x"),
        ("g#s/../x", "http://a/b/c/d;p?q", "http://a/b/c/g#s/../x"),
        ("http:g", "http://a/b/c/d;p?q", "http:g"),
        ("http:", "http://a/b/c/d;p?q", "http:"),
        // not sure where this one originated
        ("/a/b/c/./../../g", "http://a/b/c/d;p?q", "http://a/a/g"),
        // http://gbiv.com/protocols/uri/test/rel_examples2.html
        // slashes in base URI"s query args
        ("g", "http://a/b/c/d;p?q=1/2", "http://a/b/c/g"),
        ("./g", "http://a/b/c/d;p?q=1/2", "http://a/b/c/g"),
        ("g/", "http://a/b/c/d;p?q=1/2", "http://a/b/c/g/"),
        ("/g", "http://a/b/c/d;p?q=1/2", "http://a/g"),
        ("//g", "http://a/b/c/d;p?q=1/2", "http://g"),
        // changed in RFC 2396bis
        ("?y", "http://a/b/c/d;p?q=1/2", "http://a/b/c/d;p?y"),
        ("g?y", "http://a/b/c/d;p?q=1/2", "http://a/b/c/g?y"),
        ("g?y/./x", "http://a/b/c/d;p?q=1/2", "http://a/b/c/g?y/./x"),
        (
            "g?y/../x",
            "http://a/b/c/d;p?q=1/2",
            "http://a/b/c/g?y/../x",
        ),
        ("g#s", "http://a/b/c/d;p?q=1/2", "http://a/b/c/g#s"),
        ("g#s/./x", "http://a/b/c/d;p?q=1/2", "http://a/b/c/g#s/./x"),
        (
            "g#s/../x",
            "http://a/b/c/d;p?q=1/2",
            "http://a/b/c/g#s/../x",
        ),
        ("./", "http://a/b/c/d;p?q=1/2", "http://a/b/c/"),
        ("../", "http://a/b/c/d;p?q=1/2", "http://a/b/"),
        ("../g", "http://a/b/c/d;p?q=1/2", "http://a/b/g"),
        ("../../", "http://a/b/c/d;p?q=1/2", "http://a/"),
        ("../../g", "http://a/b/c/d;p?q=1/2", "http://a/g"),
        // http://gbiv.com/protocols/uri/test/rel_examples3.html
        // slashes in path params
        // all of these changed in RFC 2396bis
        ("g", "http://a/b/c/d;p=1/2?q", "http://a/b/c/d;p=1/g"),
        ("./g", "http://a/b/c/d;p=1/2?q", "http://a/b/c/d;p=1/g"),
        ("g/", "http://a/b/c/d;p=1/2?q", "http://a/b/c/d;p=1/g/"),
        ("g?y", "http://a/b/c/d;p=1/2?q", "http://a/b/c/d;p=1/g?y"),
        (";x", "http://a/b/c/d;p=1/2?q", "http://a/b/c/d;p=1/;x"),
        ("g;x", "http://a/b/c/d;p=1/2?q", "http://a/b/c/d;p=1/g;x"),
        (
            "g;x=1/./y",
            "http://a/b/c/d;p=1/2?q",
            "http://a/b/c/d;p=1/g;x=1/y",
        ),
        (
            "g;x=1/../y",
            "http://a/b/c/d;p=1/2?q",
            "http://a/b/c/d;p=1/y",
        ),
        ("./", "http://a/b/c/d;p=1/2?q", "http://a/b/c/d;p=1/"),
        ("../", "http://a/b/c/d;p=1/2?q", "http://a/b/c/"),
        ("../g", "http://a/b/c/d;p=1/2?q", "http://a/b/c/g"),
        ("../../", "http://a/b/c/d;p=1/2?q", "http://a/b/"),
        ("../../g", "http://a/b/c/d;p=1/2?q", "http://a/b/g"),
        // http://gbiv.com/protocols/uri/test/rel_examples4.html
        // double and triple slash, unknown scheme
        ("g:h", "fred:///s//a/b/c", "g:h"),
        ("g", "fred:///s//a/b/c", "fred:///s//a/b/g"),
        ("./g", "fred:///s//a/b/c", "fred:///s//a/b/g"),
        ("g/", "fred:///s//a/b/c", "fred:///s//a/b/g/"),
        ("/g", "fred:///s//a/b/c", "fred:///g"), // may change to fred:///s//a/g
        ("//g", "fred:///s//a/b/c", "fred://g"), // may change to fred:///s//g
        ("//g/x", "fred:///s//a/b/c", "fred://g/x"), // may change to fred:///s//g/x
        ("///g", "fred:///s//a/b/c", "fred:///g"),
        ("./", "fred:///s//a/b/c", "fred:///s//a/b/"),
        ("../", "fred:///s//a/b/c", "fred:///s//a/"),
        ("../g", "fred:///s//a/b/c", "fred:///s//a/g"),
        ("../../", "fred:///s//a/b/c", "fred:///s//"), // may change to fred:///s//a/../
        ("../../g", "fred:///s//a/b/c", "fred:///s//g"), // may change to fred:///s//a/../g
        ("../../../g", "fred:///s//a/b/c", "fred:///s/g"), // may change to fred:///s//a/../../g
        ("../../../../g", "fred:///s//a/b/c", "fred:///g"), // may change to fred:///s//a/../../../g
        // http://gbiv.com/protocols/uri/test/rel_examples5.html
        // double and triple slash, well-known scheme
        ("g:h", "http:///s//a/b/c", "g:h"),
        ("g", "http:///s//a/b/c", "http:///s//a/b/g"),
        ("./g", "http:///s//a/b/c", "http:///s//a/b/g"),
        ("g/", "http:///s//a/b/c", "http:///s//a/b/g/"),
        ("/g", "http:///s//a/b/c", "http:///g"), // may change to http:///s//a/g
        ("//g", "http:///s//a/b/c", "http://g"), // may change to http:///s//g
        ("//g/x", "http:///s//a/b/c", "http://g/x"), // may change to http:///s//g/x
        ("///g", "http:///s//a/b/c", "http:///g"),
        ("./", "http:///s//a/b/c", "http:///s//a/b/"),
        ("../", "http:///s//a/b/c", "http:///s//a/"),
        ("../g", "http:///s//a/b/c", "http:///s//a/g"),
        ("../../", "http:///s//a/b/c", "http:///s//"), // may change to http:///s//a/../
        ("../../g", "http:///s//a/b/c", "http:///s//g"), // may change to http:///s//a/../g
        ("../../../g", "http:///s//a/b/c", "http:///s/g"), // may change to http:///s//a/../../g
        ("../../../../g", "http:///s//a/b/c", "http:///g"), // may change to http:///s//a/../../../g
        // from Dan Connelly"s tests in http://www.w3.org/2000/10/swap/uripath.py
        ("bar:abc", "foo:xyz", "bar:abc"),
        ("../abc", "http://example/x/y/z", "http://example/x/abc"),
        (
            "http://example/x/abc",
            "http://example2/x/y/z",
            "http://example/x/abc",
        ),
        ("../r", "http://ex/x/y/z", "http://ex/x/r"),
        ("q/r", "http://ex/x/y", "http://ex/x/q/r"),
        ("q/r#s", "http://ex/x/y", "http://ex/x/q/r#s"),
        ("q/r#s/t", "http://ex/x/y", "http://ex/x/q/r#s/t"),
        ("ftp://ex/x/q/r", "http://ex/x/y", "ftp://ex/x/q/r"),
        ("", "http://ex/x/y", "http://ex/x/y"),
        ("", "http://ex/x/y/", "http://ex/x/y/"),
        ("", "http://ex/x/y/pdq", "http://ex/x/y/pdq"),
        ("z/", "http://ex/x/y/", "http://ex/x/y/z/"),
        (
            "#Animal",
            "file:/swap/test/animal.rdf",
            "file:/swap/test/animal.rdf#Animal",
        ),
        ("../abc", "file:/e/x/y/z", "file:/e/x/abc"),
        (
            "/example/x/abc",
            "file:/example2/x/y/z",
            "file:/example/x/abc",
        ),
        ("../r", "file:/ex/x/y/z", "file:/ex/x/r"),
        ("/r", "file:/ex/x/y/z", "file:/r"),
        ("q/r", "file:/ex/x/y", "file:/ex/x/q/r"),
        ("q/r#s", "file:/ex/x/y", "file:/ex/x/q/r#s"),
        ("q/r#", "file:/ex/x/y", "file:/ex/x/q/r#"),
        ("q/r#s/t", "file:/ex/x/y", "file:/ex/x/q/r#s/t"),
        ("ftp://ex/x/q/r", "file:/ex/x/y", "ftp://ex/x/q/r"),
        ("", "file:/ex/x/y", "file:/ex/x/y"),
        ("", "file:/ex/x/y/", "file:/ex/x/y/"),
        ("", "file:/ex/x/y/pdq", "file:/ex/x/y/pdq"),
        ("z/", "file:/ex/x/y/", "file:/ex/x/y/z/"),
        (
            "file://meetings.example.com/cal#m1",
            "file:/devel/WWW/2000/10/swap/test/reluri-1.n3",
            "file://meetings.example.com/cal#m1",
        ),
        (
            "file://meetings.example.com/cal#m1",
            "file:/home/connolly/w3ccvs/WWW/2000/10/swap/test/reluri-1.n3",
            "file://meetings.example.com/cal#m1",
        ),
        ("./#blort", "file:/some/dir/foo", "file:/some/dir/#blort"),
        ("./#", "file:/some/dir/foo", "file:/some/dir/#"),
        // Ryan Lee
        ("./", "http://example/x/abc.efg", "http://example/x/"),
        // Graham Klyne"s tests
        // http://www.ninebynine.org/Software/HaskellUtils/Network/UriTest.xls
        // 01-31 are from Connelly"s cases

        // 32-49
        ("./q:r", "http://ex/x/y", "http://ex/x/q:r"),
        ("./p=q:r", "http://ex/x/y", "http://ex/x/p=q:r"),
        ("?pp/rr", "http://ex/x/y?pp/qq", "http://ex/x/y?pp/rr"),
        ("y/z", "http://ex/x/y?pp/qq", "http://ex/x/y/z"),
        ("y?q", "http://ex/x/y?q", "http://ex/x/y?q"),
        ("/x/y?q", "http://ex?p", "http://ex/x/y?q"),
        /*("c/d", "foo:a/b", "foo:a/c/d"),
        ("/c/d", "foo:a/b", "foo:/c/d"),
        ("", "foo:a/b?c#d", "foo:a/b?c"),
        ("b/c", "foo:a", "foo:b/c"),*/
        ("../b/c", "foo:/a/y/z", "foo:/a/b/c"),
        //("./b/c", "foo:a", "foo:b/c"),
        //("/./b/c", "foo:a", "foo:/b/c"),
        ("../../d", "foo://a//b/c", "foo://a/d"),
        //(".", "foo:a", "foo:"),
        //("..", "foo:a", "foo:"),
        // 50-57 (cf. TimBL comments --
        //  http://lists.w3.org/Archives/Public/uri/2003Feb/0028.html,
        //  http://lists.w3.org/Archives/Public/uri/2003Jan/0008.html)
        ("abc", "http://example/x/y%2Fz", "http://example/x/abc"),
        (
            "../../x%2Fabc",
            "http://example/a/x/y/z",
            "http://example/a/x%2Fabc",
        ),
        (
            "../x%2Fabc",
            "http://example/a/x/y%2Fz",
            "http://example/a/x%2Fabc",
        ),
        ("abc", "http://example/x%2Fy/z", "http://example/x%2Fy/abc"),
        ("q%3Ar", "http://ex/x/y", "http://ex/x/q%3Ar"),
        (
            "/x%2Fabc",
            "http://example/x/y%2Fz",
            "http://example/x%2Fabc",
        ),
        ("/x%2Fabc", "http://example/x/y/z", "http://example/x%2Fabc"),
        (
            "/x%2Fabc",
            "http://example/x/y%2Fz",
            "http://example/x%2Fabc",
        ),
        // 70-77
        (
            "http://example/a/b?c/../d",
            "foo:bar",
            "http://example/a/b?c/../d",
        ),
        (
            "http://example/a/b#c/../d",
            "foo:bar",
            "http://example/a/b#c/../d",
        ),
        // 82-88
        ("http:this", "http://example.org/base/uri", "http:this"),
        ("http:this", "http:base", "http:this"),
        (
            "mini1.xml",
            "file:///C:/DEV/Haskell/lib/HXmlToolbox-3.01/examples/",
            "file:///C:/DEV/Haskell/lib/HXmlToolbox-3.01/examples/mini1.xml",
        ),
    ];

    for (relative, base, output) in examples.iter() {
        let base = Iri::parse(*base).unwrap();
        let result = base.resolve(*relative);
        assert!(
            result.is_ok(),
            "Resolving of {} against {} failed with error: {}",
            relative,
            base,
            result.unwrap_err()
        );
        let result = result.unwrap();
        assert_eq!(
            result.as_str(),
            *output,
            "Resolving of {} against {} is wrong. Found {} and expecting {}",
            relative,
            base,
            result,
            output
        );
    }
}
