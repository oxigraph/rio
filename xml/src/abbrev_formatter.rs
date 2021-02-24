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

#[derive(Debug)]
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

#[derive(Clone, Debug, Default)]
pub struct AbbrevRdfXmlFormatterConfig {
    pub bnode_contract: bool,
    pub indentation: usize,
    pub prefix: HashMap<String, String>,
    pub typed_node: bool
}

impl AbbrevRdfXmlFormatterConfig {
    pub fn new() -> Self {
        AbbrevRdfXmlFormatterConfig {
            bnode_contract: false,
            indentation: 0,
            prefix: HashMap::new(),
            typed_node: false
        }
    }
}

enum Tag<'a> {
    Namespaced(String),
    Unnamedspaced(&'a str, &'a str)
}

pub struct AbbrevRdfXmlFormatter<A:AsRef<str>, W: Write> {
    writer: Writer<W>,
    config: AbbrevRdfXmlFormatterConfig,
    current_subject: Vec<AsRefNamedOrBlankNode<A>>,
    current_close: Vec<Vec<u8>>,
}

impl<A, W> AbbrevRdfXmlFormatter<A, W>
where A: AsRef<str> + Clone + std::fmt::Debug + PartialEq,
      W: Write,
{
    /// Builds a new formatter from a `Write` implementation and starts writing
    pub fn new(write: W, mut config: AbbrevRdfXmlFormatterConfig) -> Result<Self, io::Error> {
        config.prefix.insert("http://www.w3.org/1999/02/22-rdf-syntax-ns#".to_string(),
                             "rdf".to_string());

        Self {
            writer: Writer::new_with_indent(write, b' ', config.indentation),
            config,
            current_subject: vec![],
            current_close: vec![],
        }
        .write_start()
    }

    fn write_start(mut self) -> Result<Self, io::Error> {
        self.writer
            .write_event(Event::Decl(BytesDecl::new(b"1.0", Some(b"UTF-8"), None)))
            .map_err(map_err)?;
        let mut rdf_open = BytesStart::borrowed_name(b"rdf:RDF");
        self.write_prefix(&mut rdf_open)?;
        self.writer
            .write_event(Event::Start(rdf_open))
            .map_err(map_err)?;
        Ok(self)
    }

    fn write_prefix(&mut self, rdf_open: &mut BytesStart<'_>) -> Result<(), io::Error> {
        for i in &self.config.prefix {
            let ns = format!("xmlns:{}", &i.1);
            rdf_open.push_attribute((&ns[..],
                                     &i.0[..]));
        }

        Ok(())
    }

    fn tags_for_iri<'a>(&self, iri: &'a A) -> Tag<'a>
    {
        let (iri_prefix, iri_value) = split_iri(iri);
        let (iri_qname, iri_xmlns) = (iri_value, iri_prefix);

        if let Some(iri_ns_prefix) = &self.config.prefix.get(iri_prefix) {
            Tag::Namespaced(format!("{}:{}", &iri_ns_prefix, &iri_qname))
        } else {
            Tag::Unnamedspaced(iri_qname, iri_xmlns)
        }
    }

    fn bytes_for_iri<'a>(&self, iri: &'a A) -> BytesStart<'static> {
        let tag = self.tags_for_iri(&iri);

        match tag {
            Tag::Namespaced(name) =>  BytesStart::owned_name(name),
            Tag::Unnamedspaced(qname, xmlns) => {
                let mut bs = BytesStart::owned_name(qname.as_bytes());
                bs.push_attribute(("xmlns", xmlns));
                bs
            }
        }
    }

    pub fn format(&mut self, triple: &AsRefTriple<A>) -> Result<(), io::Error> {
        let current_subject = self.current_subject.pop();
        let current_close = self.current_close.pop();

        //dbg!(std::str::from_utf8(&current_close.clone().unwrap()));
        if current_subject.as_ref() != Some(&triple.subject) {
            if current_subject.is_some() {
                if let Some(close) = &current_close {
                    self.writer
                        .write_event(Event::End(BytesEnd::borrowed(&close)))
                        .map_err(map_err)?;
                }
            };
        }

        if triple.predicate.iri.as_ref() == "http://www.w3.org/1999/02/22-rdf-syntax-ns#type"
        {
            // Format object tag, about subject
            if let AsRefTerm::NamedNode(nn) = &triple.object {

                let mut open = self.bytes_for_iri(&nn.iri);
                self.current_close.push(open.name().to_vec());
                self.current_subject.push(triple.subject.clone());

                match triple.subject {
                    AsRefNamedOrBlankNode::NamedNode(ref n) => {
                        open.push_attribute(("rdf:about", n.iri.as_ref()))
                    }
                    AsRefNamedOrBlankNode::BlankNode(ref n) => {
                        open.push_attribute(("rdf:nodeID", n.id.as_ref()))
                    }
                }
                self.writer
                    .write_event(Event::Start(open))
                    .map_err(map_err)?;
            }
            return Ok(());
        }

        self.format_normal(triple, current_subject, current_close)
    }


    fn format_normal(&mut self, triple: &AsRefTriple<A>,
                     mut current_subject: Option<AsRefNamedOrBlankNode<A>>,
                     mut current_close: Option<Vec<u8>>
    ) -> Result<(), io::Error> {
        // We open a new rdf:Description if useful
        let mut property_open = self.bytes_for_iri(&triple.predicate.iri);


        //dbg!(std::str::from_utf8(&current_close.clone().unwrap()));
        if current_subject.as_ref() != Some(&triple.subject) {
            if current_subject.is_some() {
                if let Some(close) = &current_close {
                    //dbg!("About to write", std::str::from_utf8(close));
                    self.writer
                        .write_event(Event::End(BytesEnd::borrowed(&close)))
                        .map_err(map_err)?;
                    current_close = None;
                }
            };

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
        } else {
            self.writer
                .write_event(Event::Empty(property_open))
                .map_err(map_err)?;
        }

        self.current_subject.push(triple.subject.clone());
        if let Some(close) = current_close {
            self.current_close.push(close);
        }
        else {
            self.current_close.push(b"rdf:Description".to_vec())
        }
        Ok(())
    }

    /// Finishes writing and returns the underlying `Write`
    pub fn finish(mut self) -> Result<W, io::Error> {
        if let Some(close) = self.current_close.pop() {
            self.writer
                .write_event(Event::End(BytesEnd::borrowed(&close)))
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
