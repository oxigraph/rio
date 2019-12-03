//! Implementation of N-Triples and N-Quads RDF syntax

use crate::error::*;
use crate::shared::*;
use crate::utils::*;
use rio_api::model::*;
use rio_api::parser::*;
use std::io::BufRead;

/// A [N-Triples](https://www.w3.org/TR/n-triples/) streaming parser.
///
/// It implements the `TriplesParser` trait.
///
/// Its memory consumption is linear in the size of the longest line of the file.
/// It does not do any allocation during parsing except buffer resizing
/// if a line significantly longer than the previous is encountered.
///
///
/// Count the number of of people using the `TriplesParser` API:
/// ```
/// use rio_turtle::{NTriplesParser, TurtleError};
/// use rio_api::parser::TriplesParser;
/// use rio_api::model::NamedNode;
///
/// let file = b"<http://example.com/foo> <http://www.w3.org/1999/02/22-rdf-syntax-ns#type> <http://schema.org/Person> .
/// <http://example.com/foo> <http://schema.org/name> \"Foo\" .
/// <http://example.com/bar> <http://www.w3.org/1999/02/22-rdf-syntax-ns#type> <http://schema.org/Person> .
/// <http://example.com/bar> <http://schema.org/name> \"Bar\" .";
///
/// let rdf_type = NamedNode { iri: "http://www.w3.org/1999/02/22-rdf-syntax-ns#type" };
/// let schema_person = NamedNode { iri: "http://schema.org/Person" };
/// let mut count = 0;
/// NTriplesParser::new(file.as_ref()).unwrap().parse_all(&mut |t| {
///     if t.predicate == rdf_type && t.object == schema_person.into() {
///         count += 1;
///     }
///     Ok(()) as Result<(), TurtleError>
/// }).unwrap();
/// assert_eq!(2, count)
/// ```
pub struct NTriplesParser<R: BufRead> {
    read: LookAheadLineBasedByteReader<R>,
    subject_buf: String,
    predicate_buf: String,
    object_buf: String,
    object_annotation_buf: String, // datatype or language tag
}

impl<R: BufRead> NTriplesParser<R> {
    pub fn new(reader: R) -> Result<Self, TurtleError> {
        Ok(Self {
            read: LookAheadLineBasedByteReader::new(reader),
            subject_buf: String::default(),
            predicate_buf: String::default(),
            object_buf: String::default(),
            object_annotation_buf: String::default(),
        })
    }
}

impl<R: BufRead> TriplesParser for NTriplesParser<R> {
    type Error = TurtleError;

    fn parse_step<E: From<TurtleError>>(
        &mut self,
        on_triple: &mut impl FnMut(Triple) -> Result<(), E>,
    ) -> Result<(), E> {
        let result = match parse_triple_line(
            &mut self.read,
            &mut self.subject_buf,
            &mut self.predicate_buf,
            &mut self.object_buf,
            &mut self.object_annotation_buf,
        ) {
            Ok(Some(triple)) => on_triple(triple),
            Ok(None) => Ok(()),
            Err(error) => {
                self.read.consume_line_end()?;
                Err(E::from(error))
            }
        };

        //We clear the buffers
        self.subject_buf.clear();
        self.predicate_buf.clear();
        self.object_buf.clear();
        self.object_annotation_buf.clear();

        result
    }

    fn is_end(&self) -> bool {
        self.read.current() == EOF
    }
}

/// A [N-Quads](https://www.w3.org/TR/n-quads/) streaming parser.
///
/// It implements the `QuadsParser` trait.
///
/// Its memory consumption is linear in the size of the longest line of the file.
/// It does not do any allocation during parsing except buffer resizing
/// if a line significantly longer than the previous is encountered.
///
///
/// Count the number of of people using the `QuadsParser` API:
/// ```
/// use rio_turtle::{NQuadsParser, TurtleError};
/// use rio_api::parser::QuadsParser;
/// use rio_api::model::NamedNode;
///
/// let file = b"<http://example.com/foo> <http://www.w3.org/1999/02/22-rdf-syntax-ns#type> <http://schema.org/Person> <http://example.com/> .
/// <http://example.com/foo> <http://schema.org/name> \"Foo\" <http://example.com/> .
/// <http://example.com/bar> <http://www.w3.org/1999/02/22-rdf-syntax-ns#type> <http://schema.org/Person> .
/// <http://example.com/bar> <http://schema.org/name> \"Bar\" .";
///
/// let rdf_type = NamedNode { iri: "http://www.w3.org/1999/02/22-rdf-syntax-ns#type" };
/// let schema_person = NamedNode { iri: "http://schema.org/Person" };
/// let mut count = 0;
/// NQuadsParser::new(file.as_ref()).unwrap().parse_all(&mut |t| {
///     if t.predicate == rdf_type && t.object == schema_person.into() {
///         count += 1;
///     }
///     Ok(()) as Result<(), TurtleError>
/// }).unwrap();
/// assert_eq!(2, count)
/// ```
pub struct NQuadsParser<R: BufRead> {
    read: LookAheadLineBasedByteReader<R>,
    subject_buf: String,
    predicate_buf: String,
    object_buf: String,
    object_annotation_buf: String, // datatype or language tag
    graph_name_buf: String,
}

impl<R: BufRead> NQuadsParser<R> {
    pub fn new(reader: R) -> Result<Self, TurtleError> {
        Ok(Self {
            read: LookAheadLineBasedByteReader::new(reader),
            subject_buf: String::default(),
            predicate_buf: String::default(),
            object_buf: String::default(),
            object_annotation_buf: String::default(),
            graph_name_buf: String::default(),
        })
    }
}

impl<R: BufRead> QuadsParser for NQuadsParser<R> {
    type Error = TurtleError;

    fn parse_step<E: From<TurtleError>>(
        &mut self,
        on_quad: &mut impl FnMut(Quad) -> Result<(), E>,
    ) -> Result<(), E> {
        let result = match parse_quad_line(
            &mut self.read,
            &mut self.subject_buf,
            &mut self.predicate_buf,
            &mut self.object_buf,
            &mut self.object_annotation_buf,
            &mut self.graph_name_buf,
        ) {
            Ok(Some(quad)) => on_quad(quad),
            Ok(None) => Ok(()),
            Err(error) => {
                self.read.consume_line_end()?;
                Err(E::from(error))
            }
        };

        //We clear the buffers
        self.subject_buf.clear();
        self.predicate_buf.clear();
        self.object_buf.clear();
        self.object_annotation_buf.clear();
        self.graph_name_buf.clear();

        result
    }

    fn is_end(&self) -> bool {
        self.read.current() == EOF
    }
}

fn parse_triple_line<'a>(
    read: &mut impl LookAheadByteRead,
    subject_buf: &'a mut String,
    predicate_buf: &'a mut String,
    object_buf: &'a mut String,
    object_annotation_buf: &'a mut String,
) -> Result<Option<Triple<'a>>, TurtleError> {
    skip_whitespace(read)?;

    let subject = match read.current() {
        EOF | b'#' | b'\r' | b'\n' => {
            skip_until_eol(read)?;
            return Ok(None);
        }
        _ => parse_named_or_blank_node(read, subject_buf)?,
    };
    skip_whitespace(read)?;

    let predicate = parse_iriref(read, predicate_buf)?;
    skip_whitespace(read)?;

    let object = parse_term(read, object_buf, object_annotation_buf)?;
    skip_whitespace(read)?;

    read.check_is_current(b'.')?;
    read.consume()?;
    skip_whitespace(read)?;

    match read.current() {
        EOF | b'#' | b'\r' | b'\n' => skip_until_eol(read)?,
        _ => read.unexpected_char_error()?,
    }

    Ok(Some(Triple {
        subject,
        predicate,
        object,
    }))
}

fn parse_quad_line<'a>(
    read: &mut impl LookAheadByteRead,
    subject_buf: &'a mut String,
    predicate_buf: &'a mut String,
    object_buf: &'a mut String,
    object_annotation_buf: &'a mut String,
    graph_name_buf: &'a mut String,
) -> Result<Option<Quad<'a>>, TurtleError> {
    skip_whitespace(read)?;

    let subject = match read.current() {
        EOF | b'#' | b'\r' | b'\n' => {
            skip_until_eol(read)?;
            return Ok(None);
        }
        _ => parse_named_or_blank_node(read, subject_buf)?,
    };
    skip_whitespace(read)?;

    let predicate = parse_iriref(read, predicate_buf)?;
    skip_whitespace(read)?;

    let object = parse_term(read, object_buf, object_annotation_buf)?;
    skip_whitespace(read)?;

    let graph_name = match read.current() {
        b'<' | b'_' => Some(parse_named_or_blank_node(read, graph_name_buf)?),
        _ => None,
    };
    skip_whitespace(read)?;

    read.check_is_current(b'.')?;
    read.consume()?;
    skip_whitespace(read)?;

    match read.current() {
        EOF | b'#' | b'\r' | b'\n' => skip_until_eol(read)?,
        _ => read.unexpected_char_error()?,
    }

    Ok(Some(Quad {
        subject,
        predicate,
        object,
        graph_name,
    }))
}

fn parse_term<'a>(
    read: &mut impl LookAheadByteRead,
    buffer: &'a mut String,
    annotation_buffer: &'a mut String,
) -> Result<Term<'a>, TurtleError> {
    match read.current() {
        b'<' => Ok(parse_iriref(read, buffer)?.into()),
        b'_' => Ok(parse_blank_node_label(read, buffer)?.into()),
        b'"' => Ok(parse_literal(read, buffer, annotation_buffer)?.into()),
        _ => read.unexpected_char_error(),
    }
}

fn parse_named_or_blank_node<'a>(
    read: &mut impl LookAheadByteRead,
    buffer: &'a mut String,
) -> Result<NamedOrBlankNode<'a>, TurtleError> {
    match read.current() {
        b'<' => Ok(parse_iriref(read, buffer)?.into()),
        b'_' => Ok(parse_blank_node_label(read, buffer)?.into()),
        _ => read.unexpected_char_error(),
    }
}

fn parse_literal<'a>(
    read: &mut impl LookAheadByteRead,
    buffer: &'a mut String,
    annotation_buffer: &'a mut String,
) -> Result<Literal<'a>, TurtleError> {
    parse_string_literal_quote(read, buffer)?;
    skip_whitespace(read)?;

    match read.current() {
        b'@' => {
            parse_langtag(read, annotation_buffer)?;
            Ok(Literal::LanguageTaggedString {
                value: buffer,
                language: annotation_buffer,
            })
        }
        b'^' => {
            read.consume()?;
            read.check_is_current(b'^')?;
            read.consume()?;
            skip_whitespace(read)?;
            Ok(Literal::Typed {
                value: buffer,
                datatype: parse_iriref(read, annotation_buffer)?,
            })
        }
        _ => Ok(Literal::Simple { value: buffer }),
    }
}

fn skip_whitespace(read: &mut impl LookAheadByteRead) -> Result<(), TurtleError> {
    loop {
        match read.current() {
            b' ' | b'\t' => read.consume()?,
            _ => return Ok(()),
        }
    }
}

fn skip_until_eol(read: &mut impl LookAheadByteRead) -> Result<(), TurtleError> {
    loop {
        match read.current() {
            EOF => return Ok(()),
            b'\n' => {
                read.consume()?;
                return Ok(());
            }
            _ => (),
        }
        read.consume()?;
    }
}

fn parse_iriref<'a>(
    read: &mut impl LookAheadByteRead,
    buffer: &'a mut String,
) -> Result<NamedNode<'a>, TurtleError> {
    parse_iriref_absolute(read, buffer)?;
    Ok(NamedNode { iri: buffer })
}
