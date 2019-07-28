use crate::error::*;
use crate::iri::*;
use crate::utils::*;
use rio_api::model::*;
use std::char;
use std::str;
use std::u8;

pub fn parse_iriref_absolute<'a>(
    read: &mut impl OneLookAheadLineByteRead,
    buffer: &'a mut Vec<u8>,
) -> Result<NamedNode<'a>, TurtleError> {
    parse_iriref(read, buffer)?;
    match validate_iri(buffer) {
        Ok(()) => Ok(NamedNode {
            iri: to_str(read, buffer)?,
        }),
        Err(_) => Err(read.parse_error(TurtleErrorKind::InvalidIRI)), //TODO position
    }
}

pub fn parse_iriref_relative<'a>(
    read: &mut impl OneLookAheadLineByteRead,
    buffer: &'a mut Vec<u8>,
    temp_buffer: &'a mut Vec<u8>,
    base_iri: &'a [u8],
) -> Result<NamedNode<'a>, TurtleError> {
    if base_iri.is_empty() {
        parse_iriref_absolute(read, buffer)
    } else {
        parse_iriref(read, temp_buffer)?;
        let result = resolve_relative_iri(temp_buffer, base_iri, buffer);
        temp_buffer.clear();
        match result {
            Ok(()) => Ok(NamedNode {
                iri: to_str(read, buffer)?,
            }),
            Err(_) => Err(read.parse_error(TurtleErrorKind::InvalidIRI)), //TODO position
        }
    }
}

fn parse_iriref(
    read: &mut impl OneLookAheadLineByteRead,
    buffer: &mut Vec<u8>,
) -> Result<(), TurtleError> {
    // [18] 	IRIREF 	::= 	'<' ([^#x00-#x20<>"{}|^`\] | UCHAR)* '>' /* #x00=NULL #01-#x1F=control codes #x20=space */
    read.check_is_current(b'<')?;
    loop {
        read.consume()?;
        match read.current() {
            b'>' => {
                read.consume()?;
                return Ok(());
            }
            b'\0'..=b' ' | b'<' | b'"' | b'{' | b'}' | b'|' | b'^' | b'`' => {
                read.unexpected_char_error()?
            }
            b'\\' => {
                read.consume()?;
                match match read.current() {
                    b'u' => read_hexa_char(read, 4)?,
                    b'U' => read_hexa_char(read, 8)?,
                    _ => read.unexpected_char_error()?,
                } {
                    '\0'..=' ' | '<' | '>' | '"' | '{' | '}' | '|' | '^' | '`' => {
                        read.unexpected_char_error()?
                    }
                    c => buffer.push_char(c),
                }
            }
            c => buffer.push(c),
        }
    }
}

pub fn parse_blank_node_label<'a>(
    read: &mut impl OneLookAheadLineByteRead,
    buffer: &'a mut Vec<u8>,
) -> Result<BlankNode<'a>, TurtleError> {
    // [141s] 	BLANK_NODE_LABEL 	::= 	'_:' (PN_CHARS_U | [0-9]) ((PN_CHARS | '.')* PN_CHARS)?
    read.check_is_current(b'_')?;
    read.consume()?;
    read.check_is_current(b':')?;
    read.consume()?;

    match read.current() {
        c if is_possible_pn_chars_u(c) || b'0' <= c && c <= b'9' => buffer.push(c),
        _ => read.unexpected_char_error()?,
    }

    loop {
        read.consume()?;
        match read.current() {
            b'.' => match read.next() {
                Some(c) if is_possible_pn_chars(c) => buffer.push(b'.'),
                _ => {
                    return Ok(BlankNode {
                        id: to_str(read, buffer)?,
                    });
                }
            },
            c if is_possible_pn_chars(c) => buffer.push(c),
            _ => {
                return Ok(BlankNode {
                    id: to_str(read, buffer)?,
                })
            }
        }
    }
}

pub fn parse_langtag(
    read: &mut impl OneLookAheadLineByteRead,
    buffer: &mut Vec<u8>,
) -> Result<(), TurtleError> {
    // [144s] 	LANGTAG 	::= 	'@' [a-zA-Z]+ ('-' [a-zA-Z0-9]+)*
    read.check_is_current(b'@')?;
    read.consume()?;

    //[a-zA-Z]
    let c = read.current();
    match c {
        b'a'..=b'z' | b'A'..=b'Z' => buffer.push(c),
        _ => read.unexpected_char_error()?,
    };

    //[a-zA-Z]*
    loop {
        read.consume()?;
        let c = read.current();
        match c {
            b'a'..=b'z' | b'A'..=b'Z' => buffer.push(c),
            b'-' => break, // follow-up
            _ => return Ok(()),
        }
    }

    // ('-' [a-zA-Z0-9]+)*
    loop {
        let c = read.current();
        match c {
            b'a'..=b'z' | b'A'..=b'Z' | b'0'..=b'9' => buffer.push(c),
            b'-' => match read.next() {
                Some(n) => match n {
                    b'a'..=b'z' | b'A'..=b'Z' | b'0'..=b'9' => buffer.push(b'-'),
                    _ => return Ok(()),
                },
                None => return Ok(()),
            },
            _ => return Ok(()),
        }
        read.consume()?
    }
}

pub fn parse_string_literal_quote(
    read: &mut impl OneLookAheadLineByteRead,
    buffer: &mut Vec<u8>,
) -> Result<(), TurtleError> {
    // [22] 	STRING_LITERAL_QUOTE 	::= 	'"' ([^#x22#x5C#xA#xD] | ECHAR | UCHAR)* '"' /* #x22=" #x5C=\ #xA=new line #xD=carriage return */
    parse_string_literal_quote_inner(read, buffer, b'"')
}

pub fn parse_string_literal_quote_inner(
    read: &mut impl OneLookAheadLineByteRead,
    buffer: &mut Vec<u8>,
    quote: u8,
) -> Result<(), TurtleError> {
    read.check_is_current(quote)?;
    loop {
        read.consume()?;
        match read.current() {
            c if c == quote => {
                read.consume()?;
                return Ok(());
            }
            b'\\' => parse_echar_or_uchar(read, buffer)?,
            b'\n' | b'\r' | EOF => read.unexpected_char_error()?,
            c => buffer.push(c),
        }
    }
}

pub fn parse_echar_or_uchar(
    read: &mut impl OneLookAheadLineByteRead,
    buffer: &mut Vec<u8>,
) -> Result<(), TurtleError> {
    read.check_is_current(b'\\')?;
    read.consume()?;
    match read.current() {
        b't' => buffer.push(b'\t'),
        b'b' => buffer.push(0x8),
        b'n' => buffer.push(b'\n'),
        b'r' => buffer.push(b'\r'),
        b'f' => buffer.push(0xC),
        b'"' => buffer.push(b'"'),
        b'\'' => buffer.push(b'\''),
        b'\\' => buffer.push(b'\\'),
        b'u' => buffer.push_char(read_hexa_char(read, 4)?),
        b'U' => buffer.push_char(read_hexa_char(read, 8)?),
        _ => read.unexpected_char_error()?,
    }
    Ok(())
}

pub(crate) fn read_hexa_char(
    read: &mut impl OneLookAheadLineByteRead,
    len: usize,
) -> Result<char, TurtleError> {
    let point = read_hexa_u32(read, len)?;
    char::from_u32(point)
        .ok_or_else(|| read.parse_error(TurtleErrorKind::InvalidUnicodeCodePoint(point)))
}

fn read_hexa_u32(read: &mut impl OneLookAheadLineByteRead, len: usize) -> Result<u32, TurtleError> {
    let mut value = 0;
    for _ in 0..len {
        read.consume()?;
        if let Some(d) = convert_hexa_byte(read.current()) {
            value = value * 16 + u32::from(d);
        } else {
            read.unexpected_char_error()?;
        };
    }
    Ok(value)
}

fn convert_hexa_byte(c: u8) -> Option<u8> {
    match c {
        b'0'..=b'9' => Some(c - b'0'),
        b'a'..=b'f' => Some(c - b'a' + 10),
        b'A'..=b'F' => Some(c - b'A' + 10),
        _ => None,
    }
}

pub(crate) fn to_str<'a>(
    read: &impl OneLookAheadLineByteRead,
    s: &'a [u8],
) -> Result<&'a str, TurtleError> {
    str::from_utf8(s).map_err(|_| read.parse_error(TurtleErrorKind::InvalidUTF8))
}

// [157s] 	PN_CHARS_BASE 	::= 	[A-Z] | [a-z] | [#x00C0-#x00D6] | [#x00D8-#x00F6] | [#x00F8-#x02FF] | [#x0370-#x037D] | [#x037F-#x1FFF] | [#x200C-#x200D] | [#x2070-#x218F] | [#x2C00-#x2FEF] | [#x3001-#xD7FF] | [#xF900-#xFDCF] | [#xFDF0-#xFFFD] | [#x10000-#xEFFFF]
pub fn is_possible_pn_chars_base(c: u8) -> bool {
    match c {
        EOF => false,
        b'A'..=b'Z' | b'a'..=b'z' | 0x80..=0xBF | 0xC2..=0xDF | 0xE0..=0xEF | 0xF0..=0xF4 => true, //TODO be strict
        _ => false,
    }
}

// [158s] 	PN_CHARS_U 	::= 	PN_CHARS_BASE | '_' | ':'
pub fn is_possible_pn_chars_u(c: u8) -> bool {
    match c {
        c if is_possible_pn_chars_base(c) => true,
        b'_' => true, //TODO: add  | ':' ???
        _ => false,
    }
}

// [160s] 	PN_CHARS 	::= 	PN_CHARS_U | '-' | [0-9] | #x00B7 | [#x0300-#x036F] | [#x203F-#x2040]
pub fn is_possible_pn_chars(c: u8) -> bool {
    match c {
        c if is_possible_pn_chars_u(c) => true,
        b'-' | b'0'...b'9' | 0x00B7 => true,
        _ => false,
    }
}
