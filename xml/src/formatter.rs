use crate::model::OwnedSubject;
use crate::utils::*;
use quick_xml::events::*;
use quick_xml::Writer;
use rio_api::formatter::TriplesFormatter;
use rio_api::model::*;
use std::convert::TryInto;
use std::io;
use std::io::Write;

/// A [RDF/XML](https://www.w3.org/TR/rdf-syntax-grammar/) formatter.
///
/// It implements the `TriplesFormatter` trait.
///
/// Write some triples using the `TriplesFormatter` API into a `Vec` buffer:
/// ```
/// use rio_xml::RdfXmlFormatter;
/// use rio_api::formatter::TriplesFormatter;
/// use rio_api::model::{NamedNode, Triple};
///
/// let mut formatter = RdfXmlFormatter::new(Vec::default())?;
/// formatter.format(&Triple {
///     subject: NamedNode { iri: "http://example.com/foo" }.into(),
///     predicate: NamedNode { iri: "http://www.w3.org/1999/02/22-rdf-syntax-ns#type" }.into(),
///     object: NamedNode { iri: "http://schema.org/Person" }.into()
/// })?;
/// let _xml = formatter.finish()?;
/// # std::io::Result::Ok(())
/// ```
pub struct RdfXmlFormatter<W: Write> {
    writer: Writer<W>,
    current_subject: Option<OwnedSubject>,
}

impl<W: Write> RdfXmlFormatter<W> {
    /// Builds a new formatter from a `Write` implementation and starts writing
    pub fn new(write: W) -> Result<Self, io::Error> {
        Self {
            writer: Writer::new(write),
            current_subject: None,
        }
        .write_start()
    }

    /// Builds a new formatter from a `Write` implementation and starts writing.
    ///
    /// The output is indented with `indentation_size` spaces.
    pub fn with_indentation(write: W, indentation_size: usize) -> Result<Self, io::Error> {
        Self {
            writer: Writer::new_with_indent(write, b' ', indentation_size),
            current_subject: None,
        }
        .write_start()
    }

    fn write_start(mut self) -> Result<Self, io::Error> {
        self.writer
            .write_event(Event::Decl(BytesDecl::new(b"1.0", Some(b"UTF-8"), None)))
            .map_err(map_err)?;
        let mut rdf_open = BytesStart::borrowed_name(b"rdf:RDF");
        rdf_open.push_attribute(("xmlns:rdf", "http://www.w3.org/1999/02/22-rdf-syntax-ns#"));
        self.writer
            .write_event(Event::Start(rdf_open))
            .map_err(map_err)?;
        Ok(self)
    }

    /// Finishes writing and returns the underlying `Write`
    pub fn finish(mut self) -> Result<W, io::Error> {
        if self.current_subject.is_some() {
            self.writer
                .write_event(Event::End(BytesEnd::borrowed(b"rdf:Description")))
                .map_err(map_err)?;
        }
        self.writer
            .write_event(Event::End(BytesEnd::borrowed(b"rdf:RDF")))
            .map_err(map_err)?;
        Ok(self.writer.into_inner())
    }
}

impl<W: Write> TriplesFormatter for RdfXmlFormatter<W> {
    type Error = io::Error;

    fn format(&mut self, triple: &Triple<'_>) -> Result<(), io::Error> {
        // We open a new rdf:Description if useful
        if self.current_subject.as_ref().map(|v| v.into()) != Some(triple.subject) {
            if self.current_subject.is_some() {
                self.writer
                    .write_event(Event::End(BytesEnd::borrowed(b"rdf:Description")))
                    .map_err(map_err)?;
            }

            let mut description_open = BytesStart::borrowed_name(b"rdf:Description");
            match triple.subject {
                Subject::NamedNode(n) => description_open.push_attribute(("rdf:about", n.iri)),
                Subject::BlankNode(n) => description_open.push_attribute(("rdf:nodeID", n.id)),
                _ => {
                    return Err(io::Error::new(
                        io::ErrorKind::InvalidInput,
                        "RDF/XML only supports named or blank subject",
                    ))
                }
            }
            self.writer
                .write_event(Event::Start(description_open))
                .map_err(map_err)?;
        }

        let (prop_prefix, prop_value) = split_iri(triple.predicate.iri);
        let (prop_qname, prop_xmlns) = if prop_value.is_empty() {
            ("prop:", ("xmlns:prop", prop_prefix))
        } else {
            (prop_value, ("xmlns", prop_prefix))
        };
        let mut property_open = BytesStart::borrowed_name(prop_qname.as_bytes());
        property_open.push_attribute(prop_xmlns);
        let content = match triple.object {
            Term::NamedNode(n) => {
                property_open.push_attribute(("rdf:resource", n.iri));
                None
            }
            Term::BlankNode(n) => {
                property_open.push_attribute(("rdf:nodeID", n.id));
                None
            }
            Term::Literal(l) => match l {
                Literal::Simple { value } => Some(value),
                Literal::LanguageTaggedString { value, language } => {
                    property_open.push_attribute(("xml:lang", language));
                    Some(value)
                }
                Literal::Typed { value, datatype } => {
                    property_open.push_attribute(("rdf:datatype", datatype.iri));
                    Some(value)
                }
            },
            _ => {
                return Err(io::Error::new(
                    io::ErrorKind::InvalidInput,
                    "RDF/XML only supports named, blank or literal object",
                ))
            }
        };
        if let Some(content) = content {
            self.writer
                .write_event(Event::Start(property_open))
                .map_err(map_err)?;
            self.writer
                .write_event(Event::Text(BytesText::from_plain_str(content)))
                .map_err(map_err)?;
            self.writer
                .write_event(Event::End(BytesEnd::borrowed(prop_qname.as_bytes())))
                .map_err(map_err)?;
        } else {
            self.writer
                .write_event(Event::Empty(property_open))
                .map_err(map_err)?;
        }
        self.current_subject = Some(triple.subject.try_into()?);
        Ok(())
    }
}

fn map_err(error: quick_xml::Error) -> io::Error {
    if let quick_xml::Error::Io(error) = error {
        error
    } else {
        io::Error::new(io::ErrorKind::Other, error)
    }
}

fn split_iri(iri: &str) -> (&str, &str) {
    if let Some(position_base) = iri.rfind(|c| !is_name_char(c) || c == ':') {
        if let Some(position_add) = iri[position_base..].find(|c| is_name_start_char(c) && c != ':')
        {
            (
                &iri[..position_base + position_add],
                &iri[position_base + position_add..],
            )
        } else {
            (iri, "")
        }
    } else {
        (iri, "")
    }
}

#[cfg(test)]
mod test {
    use super::*;

    #[test]
    fn test_split_iri() {
        assert_eq!(
            split_iri("http://schema.org/Person"),
            ("http://schema.org/", "Person")
        );
        assert_eq!(split_iri("http://schema.org/"), ("http://schema.org/", ""));
    }

    #[cfg(feature = "rio_api/star")]
    #[test]
    fn formmatting_rdf_star_fails_cleanly() {
        use rio_api::formatter::TriplesFormatter;
        let iri = NamedNode { iri: "tag:iri" };
        let triple = Triple {
            subject: Triple {
                subject: iri.into(),
                predicate: iri,
                object: iri.into(),
            }
            .into(),
            predicate: iri,
            object: iri.into(),
        };
        let mut fmt = RdfXmlFormatter::new(std::io::sink()).unwrap();
        let res = fmt.format(&triple).and_then(|_| fmt.finish());
        assert!(res.is_err());
    }
}
