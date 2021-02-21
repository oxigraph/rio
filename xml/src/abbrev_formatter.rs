use std::{io::{self, Write}};

use quick_xml::{Writer, events::{BytesDecl, BytesStart, Event}};


#[derive(Eq, PartialEq, Ord, PartialOrd, Debug, Clone, Hash)]
pub struct AsRefNamedNode<A:AsRef<str>> {
    pub iri: A,
}

#[derive(Eq, PartialEq, Ord, PartialOrd, Debug, Clone, Hash)]
pub struct AsRefBlankNode<A:AsRef<str>> {
    pub id: A,
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

#[derive(Eq, PartialEq, Ord, PartialOrd, Debug, Clone, Hash)]
pub enum AsRefNamedOrBlankNode<A:AsRef<str>> {
    NamedNode(AsRefNamedNode<A>),
    BlankNode(AsRefBlankNode<A>),
}


#[derive(Eq, PartialEq, Ord, PartialOrd, Debug, Clone, Hash)]
pub enum AsRefTerm<A:AsRef<str>> {
    NamedNode(AsRefNamedNode<A>),
    BlankNode(AsRefBlankNode<A>),
    Literal(AsRefLiteral<A>),
}


pub struct AsRefTriple<A: AsRef<str>> {
    pub subject: AsRefNamedOrBlankNode<A>,
    pub predicate: AsRefNamedNode<A>,
    pub object: AsRefTerm<A>
}

#[derive(Copy, Clone, Debug)]
pub enum AbbrevRdfXmlFormatterConfig {
    BNodeContraction,
    TypedNode,
}

pub struct AbbrevRdfXmlFormatter<A:AsRef<str>, W: Write> {
    writer: Writer<W>,
    config: Vec<AbbrevRdfXmlFormatterConfig>,
    a: std::marker::PhantomData<A>,
}

impl<A: AsRef<str>, W: Write> AbbrevRdfXmlFormatter<A, W> {
    /// Builds a new formatter from a `Write` implementation and starts writing
    pub fn new(write: W) -> Result<Self, io::Error> {
        let mut writer = Writer::new_with_indent(write, b' ', 4);
        writer
            .write_event(Event::Decl(BytesDecl::new(b"1.0", Some(b"UTF-8"), None)))
            .map_err(map_err)?;
        let mut rdf_open = BytesStart::borrowed_name(b"rdf:RDF");
        rdf_open.push_attribute(("xmlns:rdf", "http://www.w3.org/1999/02/22-rdf-syntax-ns#"));
        writer
            .write_event(Event::Start(rdf_open))
            .map_err(map_err)?;
        Ok(Self {
            writer,
            config: vec![],
            a: std::marker::PhantomData
        })
    }

}


fn map_err(error: quick_xml::Error) -> io::Error {
    if let quick_xml::Error::Io(error) = error {
        error
    } else {
        io::Error::new(io::ErrorKind::Other, error)
    }
}
