use crate::model::OwnedNamedOrBlankNode;
use crate::RdfXmlError;
use quick_xml::events::*;
use quick_xml::Writer;
use rio_api::formatter::TriplesFormatter;
use rio_api::model::*;
use std::io::Write;

/// A [RDF XML](https://www.w3.org/TR/rdf-syntax-grammar/) formatter.
///
/// It implements the `TriplesFormatter` trait.
///
/// Write some triples using the `TriplesFormatter` API into a `Vec` buffer:
/// ```
/// use rio_xml::RdfXmlFormatter;
/// use rio_api::formatter::TriplesFormatter;
/// use rio_api::model::{NamedNode, Triple};
///
/// let mut formatter = RdfXmlFormatter::new(Vec::default()).unwrap();
/// formatter.format(&Triple {
///     subject: NamedNode { iri: "http://example.com/foo" }.into(),
///     predicate: NamedNode { iri: "http://www.w3.org/1999/02/22-rdf-syntax-ns#type" }.into(),
///     object: NamedNode { iri: "http://schema.org/Person" }.into()
/// }).unwrap();
/// let xml = formatter.finish().unwrap();
/// ```
pub struct RdfXmlFormatter<W: Write> {
    writer: Writer<W>,
    current_subject: Option<OwnedNamedOrBlankNode>,
}

impl<W: Write> RdfXmlFormatter<W> {
    /// Builds a new formatter from a `Write` implementation and starts writing
    pub fn new(write: W) -> Result<Self, RdfXmlError> {
        let mut writer = Writer::new(write);
        writer.write_event(Event::Decl(BytesDecl::new(b"1.0", None, None)))?;
        let mut rdf_open = BytesStart::borrowed_name(b"rdf:RDF");
        rdf_open.push_attribute(("xmlns:rdf", "http://www.w3.org/1999/02/22-rdf-syntax-ns#"));
        writer.write_event(Event::Start(rdf_open))?;
        Ok(Self {
            writer,
            current_subject: None,
        })
    }

    /// Finishes to write and returns the underlying `Write`
    pub fn finish(mut self) -> Result<W, RdfXmlError> {
        if self.current_subject.is_some() {
            self.writer
                .write_event(Event::End(BytesEnd::borrowed(b"rdf:Description")))?;
        }
        self.writer
            .write_event(Event::End(BytesEnd::borrowed(b"rdf:RDF")))?;
        Ok(self.writer.into_inner())
    }
}

impl<W: Write> TriplesFormatter for RdfXmlFormatter<W> {
    type Error = RdfXmlError;

    fn format(&mut self, triple: &Triple) -> Result<(), RdfXmlError> {
        // We open a new rdf:Description if useful
        if self.current_subject.as_ref().map(|v| v.into()) != Some(triple.subject) {
            if self.current_subject.is_some() {
                self.writer
                    .write_event(Event::End(BytesEnd::borrowed(b"rdf:Description")))?;
            }

            let mut description_open = BytesStart::borrowed_name(b"rdf:Description");
            match triple.subject {
                NamedOrBlankNode::NamedNode(n) => {
                    description_open.push_attribute(("rdf:about", n.iri))
                }
                NamedOrBlankNode::BlankNode(n) => {
                    description_open.push_attribute(("rdf:nodeID", n.id))
                }
            }
            self.writer.write_event(Event::Start(description_open))?;
        }

        let mut property_open = BytesStart::borrowed_name(b"prop:");
        let mut content = None;
        property_open.push_attribute(("xmlns:prop", triple.predicate.iri));
        match triple.object {
            Term::NamedNode(n) => property_open.push_attribute(("rdf:resource", n.iri)),
            Term::BlankNode(n) => property_open.push_attribute(("rdf:nodeID", n.id)),
            Term::Literal(l) => match l {
                Literal::Simple { value } => content = Some(value),
                Literal::LanguageTaggedString { value, language } => {
                    content = Some(value);
                    property_open.push_attribute(("xml:lang", language))
                }
                Literal::Typed { value, datatype } => {
                    content = Some(value);
                    property_open.push_attribute(("rdf:datatype", datatype.iri))
                }
            },
        }
        if let Some(content) = content {
            self.writer.write_event(Event::Start(property_open))?;
            self.writer
                .write_event(Event::Text(BytesText::from_plain_str(&content)))?;
            self.writer
                .write_event(Event::End(BytesEnd::borrowed(b"prop:")))?;
        } else {
            self.writer.write_event(Event::Empty(property_open))?;
        }

        self.current_subject = Some(triple.subject.into());
        Ok(())
    }
}
