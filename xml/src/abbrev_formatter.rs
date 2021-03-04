use std::{collections::HashMap, io::{self, Write}};
use std::fmt;

use quick_xml::{Writer, events::{BytesDecl, BytesEnd, BytesStart, BytesText, Event}};
use rio_api::model::{BlankNode, Literal, NamedNode, NamedOrBlankNode, Term, Triple};

use crate::utils::{is_name_char, is_name_start_char};

#[derive(Eq, PartialEq, Ord, PartialOrd, Debug, Clone, Hash)]
pub struct AsRefNamedNode<A:AsRef<str>> {
    pub iri: A,
}

impl<A:AsRef<str>> fmt::Display for AsRefNamedNode<A> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let nn:NamedNode<'_> = self.into();
        write!(f, "{}", nn)
    }
}

impl<'a, A:AsRef<str>> From<&'a AsRefNamedNode<A>> for NamedNode<'a> {
    fn from(arnn: &'a AsRefNamedNode<A>) -> Self {
        NamedNode{iri: arnn.iri.as_ref()}
    }
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

impl<A:AsRef<str>> fmt::Display for AsRefBlankNode<A> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let nn:BlankNode<'_> = self.into();
        write!(f, "{}", nn)
    }
}

impl<'a, A:AsRef<str>> From<&'a AsRefBlankNode<A>> for BlankNode<'a> {
    fn from(arbn: &'a AsRefBlankNode<A>) -> Self {
        BlankNode{id: arbn.id.as_ref()}
    }
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

impl<A:AsRef<str>> fmt::Display for AsRefLiteral<A> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let nn:Literal<'_> = self.into();
        write!(f, "{}", nn)
    }
}

impl<'a, A:AsRef<str>> From<&'a AsRefLiteral<A>> for Literal<'a> {
    fn from(l: &'a AsRefLiteral<A>) -> Self {
        match l {
            AsRefLiteral::Simple { value } =>
                Literal::Simple{value:value.as_ref()},
            AsRefLiteral::LanguageTaggedString { value, language } =>
                Literal::LanguageTaggedString{
                    value: value.as_ref(),
                    language: language.as_ref(),
                },
            AsRefLiteral::Typed { value, datatype } =>
                Literal::Typed {
                    value: value.as_ref(),
                    datatype: datatype.into(),
                },
        }
    }
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

impl<A:AsRef<str>> fmt::Display for AsRefNamedOrBlankNode<A> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let nn:NamedOrBlankNode<'_> = self.into();
        write!(f, "{}", nn)
    }
}

impl<'a, A:AsRef<str>> From<&'a AsRefNamedOrBlankNode<A>> for NamedOrBlankNode<'a> {
    fn from(anbn: &'a AsRefNamedOrBlankNode<A>) -> Self {
        match anbn {
            AsRefNamedOrBlankNode::NamedNode(nn) =>
                NamedOrBlankNode::NamedNode(nn.into()),
            AsRefNamedOrBlankNode::BlankNode(bn) =>
                NamedOrBlankNode::BlankNode(bn.into())
        }
    }
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

impl<A:AsRef<str>> fmt::Display for AsRefTerm<A> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let t:Term<'_> = self.into();
        write!(f, "{}", t)
    }
}

impl<'a, A:AsRef<str>> From<&'a AsRefTerm<A>> for Term<'a> {
    fn from(t: &'a AsRefTerm<A>) -> Self {
        match t {
            AsRefTerm::NamedNode(nn) =>
                Term::NamedNode(nn.into()),
            AsRefTerm::BlankNode(bn) =>
                Term::BlankNode(bn.into()),
            AsRefTerm::Literal(l) =>
                Term::Literal(l.into()),
        }
    }
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

#[derive(Clone, Debug)]
pub struct AsRefTriple<A: AsRef<str>> {
    pub subject: AsRefNamedOrBlankNode<A>,
    pub predicate: AsRefNamedNode<A>,
    pub object: AsRefTerm<A>
}

impl<A:AsRef<str>> fmt::Display for AsRefTriple<A> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let t:Triple<'_> = self.into();
        write!(f, "{}", t)
    }
}

impl<'a, A:AsRef<str>> From<&'a AsRefTriple<A>> for Triple<'a>{
    fn from(t: &'a AsRefTriple<A>) -> Self {
        Triple {
            subject: (&t.subject).into(),
            predicate: (&t.predicate).into(),
            object: (&t.object).into()
        }
    }
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
    maybe_empty_open: Option<BytesStart<'static>>,
    bnode_cache: HashMap<AsRefBlankNode<A>, Vec<AsRefTriple<A>>>
}

impl<A, W> AbbrevRdfXmlFormatter<A, W>
where A: AsRef<str> + Clone + std::fmt::Debug + Eq + std::hash::Hash + PartialEq,
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
            maybe_empty_open: None,
            bnode_cache: HashMap::new()
        }
        .write_declaration()
    }

    fn write_declaration(mut self) -> Result<Self, io::Error> {
        self.write_event(Event::Decl(BytesDecl::new(b"1.0", Some(b"UTF-8"), None)))
            .map_err(map_err)?;
        let mut rdf_open = BytesStart::borrowed_name(b"rdf:RDF");
        self.write_prefix(&mut rdf_open)?;
        self.write_event(Event::Start(rdf_open))
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

    fn bytes_for_iri<'a>(&self, iri: &'a A) -> BytesStart<'static> {
        let (iri_prefix, iri_value) = split_iri(iri);
        let (iri_qname, iri_xmlns) = (iri_value, iri_prefix);

        let tag =
            if let Some(iri_ns_prefix) = &self.config.prefix.get(iri_prefix) {
                Tag::Namespaced(format!("{}:{}", &iri_ns_prefix, &iri_qname))
            } else {
                Tag::Unnamedspaced(iri_qname, iri_xmlns)
            };

        match tag {
            Tag::Namespaced(name) =>  BytesStart::owned_name(name),
            Tag::Unnamedspaced(qname, xmlns) => {
                let mut bs = BytesStart::owned_name(qname.as_bytes());
                bs.push_attribute(("xmlns", xmlns));
                bs
            }
        }
    }

    fn write_close(&mut self) -> Result<(), io::Error> {
        //writeln!(self.writer.inner(), "\nwrite_close:");
        let close = self.current_close.pop().ok_or(
            io::Error::new(io::ErrorKind::Other, "close when no close is available")
        )?;

        //  println!("\nwrite_close:");
        if let Some(empty) = self.maybe_empty_open.take() {
            self.write_event(Event::Empty(empty)).map_err(map_err)
        } else {
            self.write_event(Event::End(BytesEnd::owned(close))).map_err(map_err)
        }
    }

    fn write_start(&mut self, event: Event<'_>) -> Result<(), quick_xml::Error> {
        //println!("\nwrite_start:");
        self.write_complete_open()?;
        match event {
            Event::Start(bs) => {
                self.current_close.push(bs.name().to_vec());
                self.maybe_empty_open = Some(bs.to_owned());
            }
            _ => panic!("Only pass a start event to write start"),
        }
        Ok(())
    }

    fn write_complete_open(&mut self) -> Result<(), quick_xml::Error> {
        if let Some(bs) = self.maybe_empty_open.take() {
            self.writer.write_event(Event::Start(bs))?;
        }
        self.maybe_empty_open = None;
        Ok(())
    }

    // Write a single event here.
    fn write_event(&mut self, event: Event<'_>) -> Result<(), quick_xml::Error> {
        self.write_complete_open()?;
        //println!("\nwrite_event:{:?}", &event);
        // If this is a start event, capture it, and hold it till the
        // next event. If the next event is a cognate close, send a Empty.
        self.writer.write_event(event)
    }

    pub fn format(&mut self, triple: &AsRefTriple<A>) -> Result<(), io::Error> {
        //write!(self.writer.inner(), "\nformat: {:?}", triple);

        if let AsRefTriple{
                subject: AsRefNamedOrBlankNode::BlankNode(bnode),
                predicate:_,
                object:_
            } = &triple {
            self.cache_bnode_sub(&bnode, &triple);
            Ok(())
        }
        else{
            self.format_close_maybe(triple)?;
            self.format_others(triple)
        }
    }

    fn format_close_maybe(&mut self, triple: &AsRefTriple<A>) -> Result<(), io::Error> {
        let last_subject = self.current_subject[..].last();
        // writeln!(self.writer.inner(), "\nclose_maybe: {:?} : {}",
        //          last_subject, &triple.subject);

        if last_subject != Some(&triple.subject) {
                if last_subject.is_some() {
                    self.write_close()?;
                };
        }
        Ok(())
    }

    fn format_others(&mut self, triple: &AsRefTriple<A>) -> Result<(), io::Error> {
        //writeln!(self.writer.inner(), "\nformat_others: {}", triple);
        match &triple {
            AsRefTriple{
                subject:AsRefNamedOrBlankNode::BlankNode(_),
                predicate:_,
                object:_,
            } if triple.predicate.iri.as_ref()
                == "http://www.w3.org/1999/02/22-rdf-syntax-ns#first" =>
            {
                //write!(self.writer.inner(), "\nformat_first: {:?}", &bnode);
                self.format_first(&triple)
            },
            AsRefTriple{
                subject:AsRefNamedOrBlankNode::BlankNode(_),
                predicate:_,
                object:AsRefTerm::NamedNode(nn),
            } if nn.iri.as_ref()
                == "http://www.w3.org/1999/02/22-rdf-syntax-ns#nil" =>
            {
                self.write_close()?;
                self.write_close()?;
                self.write_close()
            }
            AsRefTriple{
                subject:AsRefNamedOrBlankNode::BlankNode(_),
                predicate:_,
                object:AsRefTerm::BlankNode(bnode),
            } if triple.predicate.iri.as_ref()
                == "http://www.w3.org/1999/02/22-rdf-syntax-ns#rest" =>
            {
                //write!(self.writer.inner(), "\nformat_rest: {:?}", &bnode);
                for t in self.bnode_cache.remove(bnode).unwrap_or(vec![]) {
                    self.format_others(&t)?;
                }
                Ok(())
            }
            AsRefTriple{
                 subject:_,
                 predicate:_,
                 object: AsRefTerm::BlankNode(bnode),
            } => self.format_bnode_obj(&bnode, &triple),

            _ if triple.predicate.iri.as_ref()
                == "http://www.w3.org/1999/02/22-rdf-syntax-ns#type" =>
                self.format_typed_node(&triple),

            _ => self.format_normal(&triple)
        }
    }

    fn format_bnode_obj(&mut self, bnode: &AsRefBlankNode<A>, triple: &AsRefTriple<A>)
                        -> Result<(), io::Error> {
        //println!("\nformat_bnode_obj: {:?}", triple);
        let mut property_open = self.bytes_for_iri(&triple.predicate.iri);
        match &triple.subject {
            AsRefNamedOrBlankNode::BlankNode(_) => {
                property_open.push_attribute(("rdf:parseType", "Collection"));
            }
            AsRefNamedOrBlankNode::NamedNode(_) => {
                property_open.push_attribute(("rdf:parseType", "Resource"));
            }
        }

        self.write_start(Event::Start(property_open))
            .map_err(map_err)?;

        for t in self.bnode_cache.remove(bnode).unwrap_or(vec![]) {
            //write!(self.writer.inner(), "\nformatting_cached: {:?}", &t);
            self.format_others(&t)?;
        }

        self.current_subject.push(triple.subject.clone());
        Ok(())
    }

    fn cache_bnode_sub(&mut self, bnode: &AsRefBlankNode<A>, triple: &AsRefTriple<A>){
        //write!(self.writer.inner(), "\nformat_bnode_sub: {:?}", &triple);
        let v = self.bnode_cache.get_mut(bnode);

        let t = triple.clone();
        if let Some(v) = v {
            v.push(t);
        }
        else {
            self.bnode_cache.insert(bnode.clone(), vec![t]);
        }
    }

//_:riog00000008
//	<http://www.w3.org/1999/02/22-rdf-syntax-ns#first>
//	<http://www.example.com/iri#C>

    fn format_first(&mut self, triple: &AsRefTriple<A>) -> Result<(), io::Error> {
        println!("\nformat_first:{}", triple);
        match &triple.object {
            AsRefTerm::NamedNode(ref n) => {
                let mut description_open = BytesStart::borrowed_name(b"rdf:Description");
                description_open.push_attribute(("rdf:about", n.iri.as_ref()));
                self.write_start(Event::Start(description_open))
                    .map_err(map_err)?;
                self.write_close()?;
            }
            AsRefTerm::BlankNode(ref _n) => {
                
            }
            _=> todo!()
        };

        Ok(())
    }

    fn format_typed_node(&mut self, triple: &AsRefTriple<A>) -> Result<(), io::Error> {
        //println!("\nformat_typed_node: {:?}", &triple);
        // Format object tag, about subject
        if let AsRefTerm::NamedNode(nn) = &triple.object {

            let mut open = self.bytes_for_iri(&nn.iri);
            self.current_subject.push(triple.subject.clone());

            match triple.subject {
                AsRefNamedOrBlankNode::NamedNode(ref n) => {
                    open.push_attribute(("rdf:about", n.iri.as_ref()))
                }
                AsRefNamedOrBlankNode::BlankNode(_) => {
                    // Don't need to do anything here
                }
            }
            self.write_start(Event::Start(open))
                .map_err(map_err)?;
        }
        return Ok(());
    }

    fn format_normal(&mut self, triple: &AsRefTriple<A>) -> Result<(), io::Error> {
        //println!("format_normal: {:?}", &triple);
        let mut property_open = self.bytes_for_iri(&triple.predicate.iri);

        let last_subject = self.current_subject[..].last();

        //dbg!(std::str::from_utf8(&current_close.clone().unwrap()));
        if last_subject != Some(&triple.subject) {
            let mut description_open = BytesStart::borrowed_name(b"rdf:Description");
            match triple.subject {
                AsRefNamedOrBlankNode::NamedNode(ref n) => {
                    description_open.push_attribute(("rdf:about", n.iri.as_ref()))
                }
                AsRefNamedOrBlankNode::BlankNode(ref n) => {
                    description_open.push_attribute(("rdf:nodeID", n.id.as_ref()))
                }
            }
            self.write_start(Event::Start(description_open))
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
            self.write_start(Event::Start(property_open))
                .map_err(map_err)?;
            self.write_event(Event::Text(BytesText::from_plain_str(&content.as_ref())))
                .map_err(map_err)?;
        } else {
            self.write_event(Event::Empty(property_open))
                .map_err(map_err)?;
        }

        self.current_subject.push(triple.subject.clone());
        Ok(())
    }

    /// Finishes writing and returns the underlying `Write`
    pub fn finish(mut self) -> Result<W, io::Error> {
        //write!(self.writer.inner(), "\nformat_bnode_cache_left: {:?}", &self.bnode_cache);
        while !self.current_close.is_empty() {
            self.write_close()?;
        }

        self.write_event(Event::End(BytesEnd::borrowed(b"rdf:RDF")))
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
