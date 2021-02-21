use std::{collections::HashMap, io::{self, Write}};

use quick_xml::{Writer, events::{BytesDecl, BytesEnd, BytesStart, BytesText, Event}};
use rio_api::model::{BlankNode, Literal, NamedNode, NamedOrBlankNode, Term, Triple};

use crate::utils::{is_name_char, is_name_start_char};


#[derive(Eq, PartialEq, Ord, PartialOrd, Debug, Clone, Hash)]
pub struct AsRefNamedNode<A:AsRef<str>> {
    pub iri: A,
}


impl From<NamedNode<'_>> for AsRefNamedNode<String> {
    fn from(nn: NamedNode<'_>) -> Self {
        let iri: String = nn.iri.to_string();
        AsRefNamedNode{iri}
    }
}

#[derive(Eq, PartialEq, Ord, PartialOrd, Debug, Clone, Hash)]
pub struct AsRefBlankNode<A:AsRef<str>> {
    pub id: A,
}

impl From<BlankNode<'_>> for AsRefBlankNode<String> {
    fn from(bn: BlankNode<'_>) -> Self {
        AsRefBlankNode{id:bn.id.to_string()}
    }
}

#[derive(Eq, PartialEq, Ord, PartialOrd, Debug, Clone, Hash)]
pub enum AsRefLiteral<A:AsRef<str>> {
    Simple {
        value: A,
    },
    LanguageTaggedString {
        value: A,
        language: A,
    },
    Typed {
        value: A,
        datatype: AsRefNamedNode<A>,
    },
}

impl From<Literal<'_>> for AsRefLiteral<String> {
    fn from(l: Literal<'_>) -> Self {
        match l {
            Literal::Simple { value } =>
                AsRefLiteral::Simple{value:value.to_string()},
            Literal::LanguageTaggedString { value, language } =>
                AsRefLiteral::LanguageTaggedString{
                    value: value.to_string(),
                    language: language.to_string(),
                },
            Literal::Typed { value, datatype } =>
                AsRefLiteral::Typed {
                    value: value.to_string(),
                    datatype: datatype.into(),
                },
        }
    }
}

#[derive(Eq, PartialEq, Ord, PartialOrd, Debug, Clone, Hash)]
pub enum AsRefNamedOrBlankNode<A:AsRef<str>> {
    NamedNode(AsRefNamedNode<A>),
    BlankNode(AsRefBlankNode<A>),
}

impl From<NamedOrBlankNode<'_>> for AsRefNamedOrBlankNode<String> {
    fn from(nbn: NamedOrBlankNode<'_>) -> Self {
        match nbn {
            NamedOrBlankNode::NamedNode(nn) =>
                AsRefNamedOrBlankNode::NamedNode(nn.into()),
            NamedOrBlankNode::BlankNode(bn) =>
                AsRefNamedOrBlankNode::BlankNode(bn.into()),
        }
    }
}

#[derive(Eq, PartialEq, Ord, PartialOrd, Debug, Clone, Hash)]
pub enum AsRefTerm<A:AsRef<str>> {
    NamedNode(AsRefNamedNode<A>),
    BlankNode(AsRefBlankNode<A>),
    Literal(AsRefLiteral<A>),
}

impl From<Term<'_>> for AsRefTerm<String> {
    fn from(t: Term<'_>) -> Self {
        match t {
            Term::NamedNode(nn) =>
                AsRefTerm::NamedNode(nn.into()),
            Term::BlankNode(bn) =>
                AsRefTerm::BlankNode(bn.into()),
            Term::Literal(l) =>
                AsRefTerm::Literal(l.into()),
        }
    }
}

pub struct AsRefTriple<A: AsRef<str>> {
    pub subject: AsRefNamedOrBlankNode<A>,
    pub predicate: AsRefNamedNode<A>,
    pub object: AsRefTerm<A>
}

impl From<Triple<'_>> for AsRefTriple<String> {
    fn from(t: Triple<'_>) -> Self {
        AsRefTriple {
            subject: t.subject.into(),
            predicate: t.predicate.into(),
            object: t.object.into()
        }
    }
}

#[derive(Clone, Debug)]
pub enum AbbrevRdfXmlFormatterConfig<A:AsRef<str>> {
    BNodeContraction,
    Indentation(usize),
    Prefix(HashMap<A,A>),
    TypedNode,
}

pub struct AbbrevRdfXmlFormatter<A:AsRef<str>, W: Write> {
    writer: Writer<W>,
    config: Vec<AbbrevRdfXmlFormatterConfig<A>>,
    current_subject: Option<AsRefNamedOrBlankNode<A>>,
}

impl<A, W> AbbrevRdfXmlFormatter<A, W>
where A: AsRef<str> + Clone + PartialEq,
      W: Write,
{
    /// Builds a new formatter from a `Write` implementation and starts writing
    pub fn new(write: W) -> Result<Self, io::Error> {
        Self {
            writer: Writer::new(write),
            config: vec![],
            current_subject: None
        }
        .write_start()
    }

    /// Builds a new formatter from a `Write` implementation and starts writing.
    ///
    /// The output is indented with `indentation_size` spaces.
    pub fn with_indentation(write: W, indentation_size: usize) -> Result<Self, io::Error> {
        Self {
            writer: Writer::new_with_indent(write, b' ', indentation_size),
            config: vec![],
            current_subject: None
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

    pub fn format(&mut self, triple: &AsRefTriple<A>) -> Result<(), io::Error> {
        // We open a new rdf:Description if useful
        if self.current_subject.as_ref() != Some(&triple.subject) {
            if self.current_subject.is_some() {
                self.writer
                    .write_event(Event::End(BytesEnd::borrowed(b"rdf:Description")))
                    .map_err(map_err)?;
            }

            let mut description_open = BytesStart::borrowed_name(b"rdf:Description");
            match triple.subject {
                AsRefNamedOrBlankNode::NamedNode(ref n) => {
                    description_open.push_attribute(("rdf:about", n.iri.as_ref()))
                }
                AsRefNamedOrBlankNode::BlankNode(ref n) => {
                    description_open.push_attribute(("rdf:nodeID", n.id.as_ref()))
                }
            }
            self.writer
                .write_event(Event::Start(description_open))
                .map_err(map_err)?;
        }

        let (prop_prefix, prop_value) = split_iri(&triple.predicate.iri);
        let (prop_qname, prop_xmlns) = if prop_value.is_empty() {
            ("prop:", ("xmlns:prop", prop_prefix))
        } else {
            (prop_value, ("xmlns", prop_prefix))
        };
        let mut property_open = BytesStart::borrowed_name(prop_qname.as_bytes());
        property_open.push_attribute(prop_xmlns);
        let content = match &triple.object {
            AsRefTerm::NamedNode(n) => {
                property_open.push_attribute(("rdf:resource", n.iri.as_ref()));
                None
            }
            AsRefTerm::BlankNode(n) => {
                property_open.push_attribute(("rdf:nodeID", n.id.as_ref()));
                None
            }
            AsRefTerm::Literal(l) => match l {
                AsRefLiteral::Simple { value } => Some(value),
                AsRefLiteral::LanguageTaggedString { value, language } => {
                    property_open.push_attribute(("xml:lang", language.as_ref()));
                    Some(value)
                }
                AsRefLiteral::Typed { value, datatype } => {
                    property_open.push_attribute(("rdf:datatype", datatype.iri.as_ref()));
                    Some(value)
                }
            },
        };
        if let Some(content) = content {
            self.writer
                .write_event(Event::Start(property_open))
                .map_err(map_err)?;
            self.writer
                .write_event(Event::Text(BytesText::from_plain_str(&content.as_ref())))
                .map_err(map_err)?;
            self.writer
                .write_event(Event::End(BytesEnd::borrowed(prop_qname.as_bytes())))
                .map_err(map_err)?;
        } else {
            self.writer
                .write_event(Event::Empty(property_open))
                .map_err(map_err)?;
        }

        self.current_subject = Some(triple.subject.clone());
        Ok(())
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


fn map_err(error: quick_xml::Error) -> io::Error {
    if let quick_xml::Error::Io(error) = error {
        error
    } else {
        io::Error::new(io::ErrorKind::Other, error)
    }
}

fn split_iri<A:AsRef<str>>(iri: &A) -> (&str, &str) {
    let iri = iri.as_ref();
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
