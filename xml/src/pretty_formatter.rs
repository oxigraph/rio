use crate::utils::{is_name_char, is_name_start_char};

use indexmap::IndexMap;
use quick_xml::{Writer, events::{BytesDecl, BytesEnd, BytesStart, BytesText, Event}};
use rio_api::model::{BlankNode, Literal, NamedNode, NamedOrBlankNode, Term, Triple};

use std::{self, cell::RefCell, collections::{HashMap, VecDeque}, fmt, hash::{Hash,Hasher}, io::{self, Write}};

// Utilities
fn map_err(error: quick_xml::Error) -> io::Error {
    if let quick_xml::Error::Io(error) = error {
        error
    } else {
        io::Error::new(io::ErrorKind::Other, error)
    }
}


// We need a complete copy of the while data model because we need to
// be able to copy and cache items without worrying too much about
// lifetimes and without allocation. AsRef<str> supports both of
// this, assuming that there is an Rc in the way somewhere

#[derive(Ord, PartialOrd, Debug, Clone)]
pub struct AsRefNamedNode<A:AsRef<str>> {
    pub iri: A,
    position_cache: RefCell<bool>,
    position_base: RefCell<Option<usize>>,
    position_add: RefCell<Option<usize>>,
}

impl<A:AsRef<str>> AsRefNamedNode<A> {
    pub fn new(iri: A) -> Self {
        AsRefNamedNode{iri, position_cache: RefCell::new(false),
                       position_base: RefCell::new(None),
                       position_add: RefCell::new(None)}
    }
}

impl<A:AsRef<str>> Hash for AsRefNamedNode<A> {
    fn hash<H: Hasher>(&self, state: &mut H) {
        self.iri.as_ref().hash(state);
    }
}

impl<A:AsRef<str>> PartialEq for AsRefNamedNode<A> {
    fn eq(&self, other: &Self) -> bool {
        self.iri.as_ref() == other.iri.as_ref()
    }
}

impl<A:AsRef<str>> Eq for AsRefNamedNode<A> {}


impl<A:AsRef<str>> AsRefNamedNode<A> {
    fn split_iri(&self) -> (&str, &str) {
        let iri = self.iri.as_ref();

        let mut position_cache = self.position_cache.borrow_mut();
        let mut position_base = self.position_base.borrow_mut();
        let mut position_add = self.position_add.borrow_mut();

        if !*position_cache {
            *position_cache = true;
            *position_base = iri.rfind(|c| !is_name_char(c) || c == ':');
            if let Some(position_base) = *position_base {
                *position_add = iri[position_base..].find(|c| is_name_start_char(c) && c != ':')
            }
        }

        if let Some(position_base) = *position_base {
            if let Some(position_add) = *position_add {
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
        AsRefNamedNode::new(iri)
    }
}


#[derive(Eq, PartialEq, Ord, PartialOrd, Debug, Clone, Hash)]
pub struct AsRefBlankNode<A:AsRef<str>> {
    pub id: A,
}

impl<A:AsRef<str>> AsRefBlankNode<A> {
    pub fn new(id: A) -> Self {
        AsRefBlankNode{id}
    }
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

impl<A:AsRef<str>> From<AsRefNamedNode<A>> for AsRefNamedOrBlankNode<A> {
    fn from(nn: AsRefNamedNode<A>) -> Self {
        AsRefNamedOrBlankNode::NamedNode(nn)
    }
}

impl<A:AsRef<str>> From<AsRefBlankNode<A>> for AsRefNamedOrBlankNode<A> {
    fn from(nn: AsRefBlankNode<A>) -> Self {
        AsRefNamedOrBlankNode::BlankNode(nn)
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

impl<A:AsRef<str>> From<AsRefBlankNode<A>> for AsRefTerm<A> {
    fn from(nn: AsRefBlankNode<A>) -> Self {
        AsRefTerm::BlankNode(nn)
    }
}

impl<A:AsRef<str>> From<AsRefNamedNode<A>> for AsRefTerm<A> {
    fn from(nn: AsRefNamedNode<A>) -> Self {
        AsRefTerm::NamedNode(nn)
    }
}

#[derive(Clone, Debug, Eq, PartialEq)]
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
pub struct PrettyRdfXmlFormatterConfig {
    pub bnode_contract: bool,
    pub indentation: usize,
    pub prefix: IndexMap<String, String>,
    pub typed_node: bool
}

impl PrettyRdfXmlFormatterConfig {
    pub fn new() -> Self {
        PrettyRdfXmlFormatterConfig {
            bnode_contract: false,
            indentation: 4,
            prefix: IndexMap::new(),
            typed_node: false
        }
    }
}

#[derive(Debug, Eq, PartialEq)]
enum Subject{Open, Closed}

pub struct PrettyRdfXmlFormatter<A:AsRef<str>, W: Write> {
    writer: Writer<W>,
    config: PrettyRdfXmlFormatterConfig,
    pub (crate) triple_queue: VecDeque<AsRefTriple<A>>,
    pub (crate) bnode_cache: HashMap<AsRefBlankNode<A>, VecDeque<AsRefTriple<A>>>,
    pub (crate) open_tag_stack: Vec<Vec<u8>>,
    pub (crate) open_subject_stack: Vec<AsRefNamedOrBlankNode<A>>,
    // We could remove this requirement for allocation with a more
    // complex data structure here
    last_open_tag: Option<BytesStart<'static>>
}
impl<A, W> PrettyRdfXmlFormatter<A, W>
where A: AsRef<str> + Clone + std::fmt::Debug + Eq + Hash + PartialEq,
      W: Write,
{
    /// Builds a new formatter from a `Write` implementation and starts writing

    pub fn new(write: W, mut config: PrettyRdfXmlFormatterConfig) -> Result<Self, io::Error> {
        config.prefix.insert("http://www.w3.org/1999/02/22-rdf-syntax-ns#".to_string(),
                             "rdf".to_string());

        Self {
            writer: Writer::new_with_indent(write, b' ', config.indentation),
            config,
            triple_queue: Default::default(),
            bnode_cache: Default::default(),
            open_tag_stack: Default::default(),
            open_subject_stack: Default::default(),
            last_open_tag: None,
        }
        .write_declaration()
    }

    // All Write methods produce some XML something.
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
        print!("writing prefix:{:?}", &self.config.prefix);
        for i in &self.config.prefix {
            let ns = format!("xmlns:{}", &i.1);
            rdf_open.push_attribute((&ns[..],
                                     &i.0[..]));
        }

        Ok(())
    }

    fn write_complete_open(&mut self) -> Result<(), quick_xml::Error> {
        if let Some(bs) = self.last_open_tag.take() {
            self.writer.write_event(Event::Start(bs))?;
        }
        self.last_open_tag = None;
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

    fn write_start(&mut self, event: Event<'_>) -> Result<(), quick_xml::Error> {
        self.write_complete_open()?;
        match event {
            Event::Start(bs) => {
                self.open_tag_stack.push(bs.name().to_vec());
                self.last_open_tag = Some(bs.to_owned());
            }
            _ => panic!("Only pass a start event to write start"),
        }
        Ok(())
    }
    fn write_close(&mut self) -> Result<(), io::Error> {
        let close = self.open_tag_stack.pop().ok_or(
            io::Error::new(io::ErrorKind::Other, "close when no close is available")
        ).unwrap();

        //  println!("\nwrite_close:");
        if let Some(empty) = self.last_open_tag.take() {
            self.write_event(Event::Empty(empty)).map_err(map_err)
        } else {
            self.write_event(Event::End(BytesEnd::owned(close))).map_err(map_err)
        }
    }

    fn bytes_start_iri<'a>(&mut self, nn:&'a AsRefNamedNode<A>) -> BytesStart<'a> {
        let (iri_protocol_and_host, iri_qname) = nn.split_iri();
        if let Some(iri_ns_prefix) = &self.config.prefix.get(iri_protocol_and_host) {
            BytesStart::owned_name(
                format!("{}:{}", &iri_ns_prefix, &iri_qname)
            )
        } else {
            let mut bs = BytesStart::owned_name(iri_qname.as_bytes());
            bs.push_attribute(("xmlns", iri_protocol_and_host));
            bs
        }
    }

    fn render_simple(&mut self, triple: &AsRefTriple<A>, ss: Subject) -> Result<(), io::Error> {

        let mut property_open = self.bytes_start_iri(&triple.predicate);
        if ss == Subject::Closed {
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
            self.write_close()?;
        } else {
            self.write_event(Event::Empty(property_open))
                .map_err(map_err)?;
        }

        self.open_subject_stack.push(triple.subject.clone());
        Ok(())
    }

    fn render_typed_node(&mut self, triple: &AsRefTriple<A>) -> Result<(), io::Error> {
            // Format object tag, about subject
        if let AsRefTerm::NamedNode(nn) = &triple.object {

            let mut open = self.bytes_start_iri(&nn);
            self.open_subject_stack.push(triple.subject.clone());

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

    fn render_close_maybe(&mut self, s: &AsRefNamedOrBlankNode<A>) -> Result<Subject, io::Error>
    {
        let last_subject = self.open_subject_stack[..].last();
        if last_subject.is_some() {
            if last_subject != Some(s) {
                self.write_close()?;
                Ok(Subject::Closed)
            } else {
                Ok(Subject::Open)
            }
        } else {
            Ok(Subject::Closed)
        }
    }

    fn render_bnode_obj(&mut self, triple: &AsRefTriple<A>) -> Result<(), io::Error>
    {
        println!("hello");
        let mut property_open = self.bytes_start_iri(&triple.predicate);
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

        self.open_subject_stack.push(triple.subject.clone());
        Ok(())
    }

    fn dispatch_triple(&mut self, triple: &AsRefTriple<A>) -> Result<(), io::Error>
    {
        let ss = self.render_close_maybe(&triple.subject)?;
        match triple {
            // AsRefTriple{subject:_, predicate:_,
            //             object: AsRefTerm::BlankNode(_)
            // } => self.render_bnode_obj(triple),
            _ if triple.predicate.iri.as_ref()
                == "http://www.w3.org/1999/02/22-rdf-syntax-ns#type" =>
                self.render_typed_node(&triple),
            _ => {
                self.render_simple(triple, ss)
            }
        }
    }

    fn dequeue(&mut self) -> Result<(), io::Error>
    {
        let mut snt = true;
        while snt {
            let t = self.triple_queue.pop_front();
            if let Some(t) = t {
                self.dispatch_triple(&t)?;
            } else {
                snt = false;
            }
        }
        Ok(())
    }

    // Check whether we have a triple ending in a bnode, if so decache
    // the bnode cache and push onto queue
    fn decache_maybe(&mut self, triple: &AsRefTriple<A>) {
        self.triple_queue.push_back(triple.clone());
        match triple {
            AsRefTriple {
                subject: _,
                predicate:_,
                object: AsRefTerm::BlankNode(bnode)
            } => {
                println!("Maybe remove from cache");
                if let Some(v) = self.bnode_cache.remove(bnode) {
                    for i in v {
                        self.triple_queue.push_back(i);
                    }
                }
            }
            _ =>{
                // Empty and meant to be empty
            }
        }
    }

    // If the triple starts with a bnode, then cache it till we want it
    fn cache_bnode_sub(&mut self, bnode: &AsRefBlankNode<A>,
                       triple: &AsRefTriple<A>){
        let v = self.bnode_cache.get_mut(bnode);

        let t = triple.clone();
        if let Some(v) = v {
            v.push_back(t);
        }
        else {
            self.bnode_cache.insert(bnode.clone(), vec![t].into());
        }
    }


    pub (crate) fn format_no_dequeue(&mut self, triple: &AsRefTriple<A>) -> Result<(), io::Error> {
        if let AsRefTriple{
            subject: AsRefNamedOrBlankNode::BlankNode(bnode),
            predicate:_,
            object:_
        } = &triple {
            self.cache_bnode_sub(&bnode, &triple);
            Ok(())
        } else {
            self.decache_maybe(triple);
            Ok(())
        }
    }

    // public API
    pub fn format(&mut self, triple: &AsRefTriple<A>) -> Result<(), io::Error>
    {
        self.format_no_dequeue(triple)?;
        if ! matches!(&triple, AsRefTriple{
            subject: AsRefNamedOrBlankNode::BlankNode(_),
            predicate:_,
            object:_}
        ) {
            self.dequeue()
        } else {
            Ok(())
        }
    }

    /// Finishes writing and returns the underlying `Write`
    pub fn finish(mut self) -> Result<W, io::Error> {
        //write!(self.writer.inner(), "\nformat_bnode_cache_left: {:?}", &self.bnode_cache);
        while !self.open_tag_stack.is_empty() {
            self.write_close()?;
        }

        self.write_event(Event::End(BytesEnd::borrowed(b"rdf:RDF")))
            .map_err(map_err)?;

        Ok(self.writer.into_inner())
    }
}





#[cfg(test)]
mod test {
    use indexmap::{IndexMap, indexmap};
    use pretty_assertions::assert_eq;
    use std::{collections::VecDeque, io::sink};
    use rio_api::parser::TriplesParser;
    use super::{AsRefBlankNode, AsRefNamedNode,
                AsRefTriple, PrettyRdfXmlFormatter,
                PrettyRdfXmlFormatterConfig};

    fn tbn() -> AsRefTriple<String> {
        AsRefTriple {
            subject: AsRefBlankNode{id:"bn".to_string()}.into(),
            predicate: AsRefNamedNode::new("http://example.com/p1".to_string()).into(),
            object: AsRefNamedNode::new("http://example.com/o2".to_string()).into()
        }
    }

    // fn tbn_type() -> AsRefTriple<String> {
    //     AsRefTriple {
    //         subject: AsRefBlankNode{id:"bn".to_string()}.into(),
    //         predicate: AsRefNamedNode::new("http://www.w3.org/1999/02/22-rdf-syntax-ns#type".to_string()).into(),
    //         object: AsRefNamedNode::new("http://example.com/o2".to_string()).into()
    //     }
    // }

    fn tbnobj() -> AsRefTriple<String> {
        AsRefTriple {
            subject: AsRefNamedNode::new("http://example.com/s".to_string()).into(),
            predicate: AsRefNamedNode::new("http://example.com/p1".to_string()).into(),
            object: AsRefBlankNode::new("bn".to_string()).into()
        }
    }

    fn tnn () -> AsRefTriple<String> {
        AsRefTriple {
            subject: AsRefNamedNode::new("http://example.com/s".to_string()).into(),
            predicate: AsRefNamedNode::new("http://example.com/p".to_string()).into(),
            object: AsRefNamedNode::new("http://example.com/o".to_string()).into()
        }
    }

    fn tnn_type() -> AsRefTriple<String> {
        AsRefTriple {
            subject: AsRefNamedNode::new("http://example.com/s".to_string()).into(),
            predicate: AsRefNamedNode::new("http://www.w3.org/1999/02/22-rdf-syntax-ns#type".to_string()).into(),
            object: AsRefNamedNode::new("http://example.com/o2".to_string()).into()
        }
    }

    fn tnn_rdf () -> AsRefTriple<String> {
        AsRefTriple {
            subject: AsRefNamedNode::new("http://example.com/s".to_string()).into(),
            predicate: AsRefNamedNode::new("http://www.w3.org/1999/02/22-rdf-syntax-ns#not_real".to_string()).into(),
            object: AsRefNamedNode::new("http://example.com/o".to_string()).into()
        }
    }

    fn render(source: Vec<AsRefTriple<String>>) -> String {
        let sink = vec![];
        let mut f = PrettyRdfXmlFormatter::new(
            sink, PrettyRdfXmlFormatterConfig::new()
        ).unwrap();

        for t in source {
            f.format(&t).unwrap();
        }

        let w = f.finish().unwrap();
        String::from_utf8(w).unwrap()
    }

    fn from_nt(nt: &str) -> String {
        from_nt_prefix(nt, indexmap!("http://www.w3.org/1999/02/22-rdf-syntax-ns#" => "rdf"))
    }

    fn from_nt_prefix(nt: &str, prefix: IndexMap<&str, &str>) -> String {
        let sink = vec![];

        let mut config = PrettyRdfXmlFormatterConfig::new();
        config.prefix =
            prefix.into_iter().map(
                |(k, v)|
                (k.to_string(), v.to_string())
            ).collect();

        let mut f = PrettyRdfXmlFormatter::new(sink,config).unwrap();

        let _: Vec<Result<_, _>>
            = rio_turtle::NTriplesParser::new(nt.as_bytes()).into_iter(
                |rio_triple| {
                    let t = rio_triple.into();
                    let f = f.format(&t);
                    f
                }
            ).collect();
        let w = f.finish().unwrap();
        String::from_utf8(w).unwrap()
    }

    #[test]
    fn test_split_iri() {
        let nn = AsRefNamedNode::new("http://www.example.com");
        assert_eq!(nn.split_iri(), ("http://","www.example.com"))
    }

    #[test]
    fn bnode_cache() -> Result<(), std::io::Error> {
        let b = sink();
        let mut pp = PrettyRdfXmlFormatter::new
            (
                b,
                PrettyRdfXmlFormatterConfig::new()
            )?;

        pp.format_no_dequeue(
            &tnn()
        )?;

        let tp = tbn();

        pp.format_no_dequeue(&tp.clone())?;

        assert_eq!(
            pp.bnode_cache.get(&AsRefBlankNode{id:"bn".to_string()}),
            Some(&vec![tp].into())
        );
        Ok(())
    }

    #[test]
    fn triple_queue() -> Result<(), std::io::Error> {
        let b = sink();
        let mut pp = PrettyRdfXmlFormatter::new
            (
                b,
                PrettyRdfXmlFormatterConfig::new()
            )?;

        let tp1=tbn();
        let tp2=tbnobj();
        pp.format_no_dequeue(&tp1)?;
        pp.format_no_dequeue(&tp2)?;

        let vd:VecDeque<_> = vec![tp2, tp1].into();
        assert_eq!(
            &pp.triple_queue,
            &vd
        );

        Ok(())
    }


    // Test whether can render a triple with out of namespace node
    #[test]
    fn render_triple() {
        let s = render(
            vec![tnn()]
        );
        assert_eq!(s,
r#"<?xml version="1.0" encoding="UTF-8"?>
<rdf:RDF xmlns:rdf="http://www.w3.org/1999/02/22-rdf-syntax-ns#">
    <rdf:Description rdf:about="http://example.com/s">
        <p xmlns="http://example.com/" rdf:resource="http://example.com/o"/>
    </rdf:Description>
</rdf:RDF>"#);
    }


    // Test whether or not the NS prefix is working. RDF name spaces
    // should be shrunk
    #[test]
    fn render_triple_rdf_ns() {
        let s = render(
            vec![tnn_rdf()]
        );

        assert_eq!(s,
r#"<?xml version="1.0" encoding="UTF-8"?>
<rdf:RDF xmlns:rdf="http://www.w3.org/1999/02/22-rdf-syntax-ns#">
    <rdf:Description rdf:about="http://example.com/s">
        <rdf:not_real rdf:resource="http://example.com/o"/>
    </rdf:Description>
</rdf:RDF>"#);
    }


    #[test]
    fn render_typed_triple() {
        let s = render(
            vec![tnn_type()]
        );

        assert_eq!(s,
r#"<?xml version="1.0" encoding="UTF-8"?>
<rdf:RDF xmlns:rdf="http://www.w3.org/1999/02/22-rdf-syntax-ns#">
    <o2 xmlns="http://example.com/" rdf:about="http://example.com/s"/>
</rdf:RDF>"#);
    }

    #[test]
    fn render_embedded() {
        let s = from_nt_prefix(r#"
<http://www.w3.org/TR/rdf-syntax-grammar> <http://purl.org/dc/elements/1.1/title> "RDF/XML Syntax Specification (Revised)" .
<http://www.w3.org/TR/rdf-syntax-grammar> <http://example.org/stuff/1.0/editor>  <http://purl.org/net/dajobe/>.
"#,
                               indexmap![
                                   "http://www.w3.org/1999/02/22-rdf-syntax-ns#" => "rdf",
                                   "http://purl.org/dc/elements/1.1/" => "dc",
                                   "http://example.org/stuff/1.0/" => "ex"
                               ]

        );

        let e =
r###"<?xml version="1.0" encoding="UTF-8"?>
<rdf:RDF xmlns:rdf="http://www.w3.org/1999/02/22-rdf-syntax-ns#" xmlns:dc="http://purl.org/dc/elements/1.1/" xmlns:ex="http://example.org/stuff/1.0/">
    <rdf:Description rdf:about="http://www.w3.org/TR/rdf-syntax-grammar">
        <dc:title>RDF/XML Syntax Specification (Revised)</dc:title>
        <ex:editor rdf:resource="http://purl.org/net/dajobe/"/>
    </rdf:Description>
</rdf:RDF>"###;
        //println!("\nGot:\n{}\nExpected:\n{}", &s, &e);

        assert_eq!(s, e);
    }

    #[test]
    fn render_bnode() {
        let s = from_nt(r#"
<http://www.w3.org/TR/rdf-syntax-grammar> <http://purl.org/dc/elements/1.1/title> "RDF/XML Syntax Specification (Revised)" .
_:genid1 <http://example.org/stuff/1.0/fullName> "Dave Beckett" .
_:genid1 <http://example.org/stuff/1.0/homePage> <http://purl.org/net/dajobe/> .
<http://www.w3.org/TR/rdf-syntax-grammar> <http://example.org/stuff/1.0/editor> _:genid1 .
"#);

        let e = r###"<?xml version="1.0"?>
<rdf:RDF xmlns:rdf="http://www.w3.org/1999/02/22-rdf-syntax-ns#"
            xmlns:dc="http://purl.org/dc/elements/1.1/"
            xmlns:ex="http://example.org/stuff/1.0/">
  <rdf:Description rdf:about="http://www.w3.org/TR/rdf-syntax-grammar"
                   dc:title="RDF 1.1 XML Syntax">
    <ex:editor rdf:parseType="Resource">
      <ex:fullName>Dave Beckett</ex:fullName>
      <ex:homePage rdf:resource="http://purl.org/net/dajobe/"/>
    </ex:editor>
  </rdf:Description>
</rdf:RDF>"###;

        //println!("\nGot:\n{}\nExpected:\n{}", &s, &e);
        assert_eq!(s,e);
    }


    fn render_seq() {
        let s = from_nt(r#"
_:genid1 <http://www.w3.org/1999/02/22-rdf-syntax-ns#first> <http://example.org/banana> .
_:genid2 <http://www.w3.org/1999/02/22-rdf-syntax-ns#first> <http://example.org/apple> .
_:genid1 <http://www.w3.org/1999/02/22-rdf-syntax-ns#rest> _:genid2 .
_:genid3 <http://www.w3.org/1999/02/22-rdf-syntax-ns#first> <http://example.org/pear> .
_:genid2 <http://www.w3.org/1999/02/22-rdf-syntax-ns#rest> _:genid3 .
_:genid3 <http://www.w3.org/1999/02/22-rdf-syntax-ns#rest> <http://www.w3.org/1999/02/22-rdf-syntax-ns#nil> .
<http://example.org/basket> <http://example.org/stuff/1.0/hasFruit> _:genid1 ."#);

        assert_eq!(s,
r###"<?xml version="1.0" encoding="UTF-8"?>
<rdf:RDF xmlns:rdf="http://www.w3.org/1999/02/22-rdf-syntax-ns#"
            xmlns:ex="http://example.org/stuff/1.0/">
  <rdf:Description rdf:about="http://example.org/basket">
    <ex:hasFruit rdf:parseType="Collection">
      <rdf:Description rdf:about="http://example.org/banana"/>
      <rdf:Description rdf:about="http://example.org/apple"/>
      <rdf:Description rdf:about="http://example.org/pear"/>
    </ex:hasFruit>
  </rdf:Description>
</rdf:RDF>"###);
    }
}
