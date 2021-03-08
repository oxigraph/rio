use crate::utils::{is_name_char, is_name_start_char};

use quick_xml::{Writer, events::{BytesDecl, BytesEnd, BytesStart, BytesText, Event}};
use rio_api::model::{BlankNode, Literal, NamedNode, NamedOrBlankNode, Term, Triple};

use std::fmt::{Debug, Formatter};
use std::{self, cell::RefCell, collections::HashMap, fmt,
          hash::{Hash,Hasher},
          io::{self, Write}};
use std::marker::PhantomData;

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

#[derive(Ord, PartialOrd, Clone)]
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

impl <A:Debug + AsRef<str>> Debug for AsRefNamedNode<A> {
        fn fmt(&self, f: &mut Formatter<'_>) -> ::core::fmt::Result {
            match *self {
                AsRefNamedNode {
                    ref iri,
                    position_cache: _,
                    position_base: _,
                    position_add:_
                } => {
                    let mut debug_trait_builder =
                        f.debug_struct("AsRefNamedNode");
                    let _ = debug_trait_builder.field("iri", &&(*iri));
                    debug_trait_builder.finish()
                }
            }
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
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
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

impl<A:AsRef<str>> AsRef<str> for AsRefNamedNode<A> {
    fn as_ref(&self) -> &str {
        self.iri.as_ref()
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
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
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

impl<A:AsRef<str>> AsRef<str> for AsRefBlankNode<A> {
    fn as_ref(&self) -> &str {
        self.id.as_ref()
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
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
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
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
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

impl<A:AsRef<str>> AsRef<str> for AsRefNamedOrBlankNode<A> {
    fn as_ref(&self) -> &str {
        match self {
            AsRefNamedOrBlankNode::NamedNode(nn) => nn.as_ref(),
            AsRefNamedOrBlankNode::BlankNode(bn) => bn.as_ref(),
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
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
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
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
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

enum AcceptorReturn<A:AsRef<str>> {
    Replace(AsRefExpandedTriple<A>),
    Changed,
    Unchanged,
}

trait TripleLike<A>
    where A:AsRef<str> + Clone
{
    /// Can a new Triple be accepted onto this TripleLike.
    fn accept(&mut self, t:AsRefTriple<A>) -> AcceptorReturn<A>;

    /// What is the subject of the triple like
    fn subject(&self) -> &AsRefNamedOrBlankNode<A>;

    /// Count the number of predicate arcs coming `subject`
    fn predicates_from(&self, subject: &AsRefNamedOrBlankNode<A>) -> usize;
}

impl<A> TripleLike<A> for AsRefTriple<A>
    where A:AsRef<str> + Clone + PartialEq
{
    fn accept(&mut self, t:AsRefTriple<A>) -> AcceptorReturn<A>{
        if self.subject.as_ref() == t.subject.as_ref() {
            return AcceptorReturn::Replace(
                AsRefExpandedTriple::AsRefMultiTriple(
                    AsRefMultiTriple
                    {
                        subject: t.subject.clone(),
                        vec: vec![self.clone(), t]
                    }
                )
            )
        } else {
            AcceptorReturn::Unchanged
        }
    }

    fn subject(&self) -> &AsRefNamedOrBlankNode<A> {
        &self.subject
    }

    fn predicates_from(&self, subject:&AsRefNamedOrBlankNode<A>) -> usize {
        if &self.subject == subject {
            1
        } else {
            0
        }
    }
}


// A set of triples with a shared subject
// All the triples in `vec` should start with `subject`.
#[derive(Debug)]
pub struct AsRefMultiTriple<A:AsRef<str>> {
    subject: AsRefNamedOrBlankNode<A>,
    vec: Vec<AsRefTriple<A>>,
}

impl<A> TripleLike<A> for AsRefMultiTriple<A>
where A: AsRef<str> + Clone + PartialEq
{
    fn accept(&mut self, t:AsRefTriple<A>) -> AcceptorReturn<A> {
        if self.subject.as_ref() == t.subject.as_ref() {
            self.vec.push(t);
            AcceptorReturn::Changed
        } else {
            AcceptorReturn::Unchanged
        }
    }

    fn subject(&self) -> &AsRefNamedOrBlankNode<A> {
        &self.subject
    }

    fn predicates_from(&self, subject: &AsRefNamedOrBlankNode<A>) -> usize {
        self.vec.iter().fold(0, |acc, t| acc + t.predicates_from(subject))
    }
}


// A set of terms that should be rendered as a RDF list, using first
// as a the subject of the first node
#[derive(Debug)]
pub struct AsRefTripleSeq<A:AsRef<str>> {
    subject: AsRefNamedOrBlankNode<A>,
    seq: Vec<AsRefTerm<A>>,
}

impl<A> TripleLike<A> for AsRefTripleSeq<A>
where A: AsRef<str> + Clone
{
    fn accept(&mut self, _t:AsRefTriple<A>) -> AcceptorReturn<A> {
        todo!()
    }

    fn subject(&self) -> &AsRefNamedOrBlankNode<A> {
        &self.subject
    }

    fn predicates_from(&self, _subject: &AsRefNamedOrBlankNode<A>) -> usize {
        todo!()
    }
}

// All the different forms of RDF subgraph
#[derive(Debug)]
pub enum AsRefExpandedTriple<A:AsRef<str>> {
    AsRefMultiTriple(AsRefMultiTriple<A>),
    AsRefTripleSeq(AsRefTripleSeq<A>),
    AsRefTriple(AsRefTriple<A>),
}

impl<A> From<AsRefTriple<A>> for AsRefExpandedTriple<A>
where A: AsRef<str> + Clone
{
    fn from(t: AsRefTriple<A>) -> Self {
        AsRefExpandedTriple::AsRefTriple(t)
    }
}

impl<A> TripleLike<A> for AsRefExpandedTriple<A>
where A: AsRef<str> + Clone + PartialEq {
    fn accept(&mut self, triple: AsRefTriple<A>) -> AcceptorReturn<A> {
        match self {
            Self::AsRefMultiTriple(mt) => mt.accept(triple),
            Self::AsRefTripleSeq(seq) => seq.accept(triple),
            Self::AsRefTriple(t) => t.accept(triple)
        }
    }

    fn subject(&self) -> &AsRefNamedOrBlankNode<A> {
        match self {
            Self::AsRefMultiTriple(mt) => mt.subject(),
            Self::AsRefTripleSeq(seq) => seq.subject(),
            Self::AsRefTriple(t) => t.subject(),
        }
    }

    fn predicates_from(&self, _subject: &AsRefNamedOrBlankNode<A>) -> usize {
        todo!()
    }
}



/// A chunk of RDF that should that should be coherent.
/// Current invariants:
///   - each subject should appear only once (i.e. all subjects are
///   grouped in AsRefMultiTriple)
///   - if a subject appears it represents all appearances of the node
///   as a subject in the document of which this is a chunk
///   - if BNodes appear as subjects, they appear after any
///   apperance as an object (TODO: Not implemented yet!)
#[derive(Debug)]
pub struct AsRefChunk<A:AsRef<str>>(Vec<AsRefExpandedTriple<A>>);


impl<A> AsRefChunk<A>
where A: AsRef<str> + Clone + Debug + PartialEq
{
    pub fn normalize(v:Vec<AsRefTriple<A>>) -> Self {
        let mut etv:Vec<AsRefExpandedTriple<A>> = vec![];
        'top: for t in v {
            for (i, et) in etv.iter_mut().enumerate() {
                match et.accept(t.clone()) {
                    AcceptorReturn::Replace(updated) => {
                        etv[i] = updated;
                        continue 'top;
                    }
                    AcceptorReturn::Changed => {
                        continue 'top;
                    }
                    AcceptorReturn::Unchanged => {
                        //Empty!
                    }
                }
            }
            etv.push(t.clone().into())
        }

        AsRefChunk(etv)
    }

    pub fn from_raw(v:Vec<AsRefExpandedTriple<A>>) -> Self {
        AsRefChunk(v)
    }

    pub fn empty() -> Self {
        AsRefChunk(vec![])
    }

    pub fn find_subject(&self, nnb:&AsRefNamedOrBlankNode<A>) -> Option<&AsRefExpandedTriple<A>> {
        self.0.iter().find(|et| et.subject().as_ref() == nnb.as_ref())
    }

    // pub fn filter_subject(&self, nnb:&AsRefNamedOrBlankNode<A>)
    //                       -> impl Iterator<Item=AsRefExpandedTriple<A>>{
    //     self.0.iter().filter(|et| et.subject().as_ref() == nnb.as_ref())
    // }
}



#[derive(Clone, Debug, Default)]
pub struct ChunkedRdfXmlFormatterConfig {
    pub bnode_contract: bool,
    pub indentation: usize,
    pub prefix: HashMap<String, String>,
    pub typed_node: bool
}

impl ChunkedRdfXmlFormatterConfig {
    pub fn new() -> Self {
        ChunkedRdfXmlFormatterConfig {
            bnode_contract: false,
            indentation: 4,
            prefix: HashMap::new(),
            typed_node: false
        }
    }
}

pub struct ChunkedRdfXmlFormatter<A:AsRef<str>, W: Write> {
    writer: Writer<W>,
    config: ChunkedRdfXmlFormatterConfig,
    pub (crate) open_tag_stack: Vec<Vec<u8>>,
    last_open_tag: Option<BytesStart<'static>>,
    pd: PhantomData<A>
}

impl<A, W> ChunkedRdfXmlFormatter<A, W>
where A: AsRef<str> + Clone + Debug + Eq + Hash + PartialEq,
      W: Write,
{
    pub fn new(write: W, mut config: ChunkedRdfXmlFormatterConfig) -> Result<Self, io::Error> {

        config.prefix.insert("http://www.w3.org/1999/02/22-rdf-syntax-ns#".to_string(),
                             "rdf".to_string());

        Self {
            writer: Writer::new_with_indent(write, b' ', config.indentation),
            config,
            open_tag_stack: Default::default(),
            last_open_tag: None,
            pd: PhantomData
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

    fn format_head<T:TripleLike<A>>(&mut self, triple_like:&T) -> Result<(), io::Error> {
        let mut description_open = BytesStart::borrowed_name(b"rdf:Description");
        match triple_like.subject() {
            AsRefNamedOrBlankNode::NamedNode(ref n) => {
                description_open.push_attribute(("rdf:about", n.iri.as_ref()))
            }
            AsRefNamedOrBlankNode::BlankNode(ref n) => {
                description_open.push_attribute(("rdf:nodeID", n.id.as_ref()))
            }
        }
        self.write_start(Event::Start(description_open))
            .map_err(map_err)?;

        Ok(())
    }

    fn format_property_arc(&mut self, triple: &AsRefTriple<A>, chunk:&AsRefChunk<A>) -> Result<(), io::Error> {
        let mut property_open = self.bytes_start_iri(&triple.predicate);

        let content = match &triple.object {
            AsRefTerm::NamedNode(n) => {
                if let Some(t) = chunk.find_subject(&n.clone().into()) {
                    dbg!(n);
                    todo!("need to render next node");
                } else {
                    // Rewrite: 2.4 Empty Property Elements
                    property_open.push_attribute(("rdf:resource", n.iri.as_ref()));
                    None
                }
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

        Ok(())
    }

    fn format_triple(&mut self, triple: &AsRefTriple<A>, chunk:&AsRefChunk<A>) -> Result<(), io::Error> {
        self.format_head(triple)?;
        self.format_property_arc(triple, chunk)?;
        self.write_close()?;
        Ok(())
    }

    fn format_multi(&mut self, multi_triple: &AsRefMultiTriple<A>, chunk:&AsRefChunk<A>) -> Result<(), io::Error> {
        self.format_head(multi_triple)?;

        // Rewrite: 2.3 Multiple Property Elements
        for triple in multi_triple.vec.iter() {
            self.format_property_arc(triple, chunk)?;
        }

        self.write_close()?;
        Ok(())
    }

    pub fn format(&mut self, triple: &AsRefTriple<A>) -> Result<(), io::Error> {
        self.format_triple(triple, &AsRefChunk::empty())
    }

    pub fn format_chunk(&mut self, chunk: &AsRefChunk<A>) -> Result<(), io::Error> {
        for i in chunk.0.iter() {
            match i {
                AsRefExpandedTriple::AsRefTriple(ref t) => {
                    self.format_triple(t, chunk)?;
                }
                AsRefExpandedTriple::AsRefMultiTriple(ref mt) => {
                    self.format_multi(mt, chunk)?;
                }
                _ =>{
                    todo!()
                }
            }
        }

        Ok(())
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
    use rio_api::parser::TriplesParser;
    use rio_turtle::TurtleError;

    use super::{AsRefChunk, AsRefNamedNode, AsRefTriple,
                ChunkedRdfXmlFormatter, ChunkedRdfXmlFormatterConfig};

    fn tnn () -> AsRefTriple<String> {
        AsRefTriple {
            subject: AsRefNamedNode::new("http://example.com/s".to_string()).into(),
            predicate: AsRefNamedNode::new("http://example.com/p".to_string()).into(),
            object: AsRefNamedNode::new("http://example.com/o".to_string()).into()
        }
    }

    #[test]
    pub fn chunk_hello_world() {
        assert!(true)
    }

    #[test]
    pub fn simple_chunk() {
        let chk = AsRefChunk::normalize(
            vec![
                tnn(),
            ]
        );

        assert_eq!(chk.0.len(), 1);
    }

    #[test]
    pub fn multi_chunk() {
        let chk = AsRefChunk::normalize(
            vec![
                tnn(),
                tnn(),
                tnn(),
            ]
        );

        //dbg!(&chk);
        assert_eq!(chk.0.len(), 1);
    }

    fn spec_prefix() -> IndexMap<&'static str, &'static str> {
        indexmap![
            "http://www.w3.org/1999/02/22-rdf-syntax-ns#" => "rdf",
            "http://purl.org/dc/elements/1.1/" => "dc",
            "http://example.org/stuff/1.0/" => "ex"
        ]
    }

    // fn from_nt(nt: &str) -> String {
    //     from_nt_prefix(nt, indexmap!("http://www.w3.org/1999/02/22-rdf-syntax-ns#" => "rdf"))
    // }

    fn from_nt_prefix(nt: &str, prefix: IndexMap<&str, &str>) -> String {
        let mut source: Vec<AsRefTriple<String>> = vec![];
        let _: Vec<Result<(), TurtleError>>
            = rio_turtle::NTriplesParser::new(nt.as_bytes()).into_iter(
                |rio_triple| {
                    source.push(rio_triple.into());
                    Ok(())
                }
            ).collect();


        let sink = vec![];

        let mut config = ChunkedRdfXmlFormatterConfig::new();
        config.prefix =
            prefix.into_iter().map(
                |(k, v)|
                (k.to_string(), v.to_string())
            ).collect();

        let mut f = ChunkedRdfXmlFormatter::new(sink,config).unwrap();
        let chk = AsRefChunk::normalize(source);
        //dbg!(&chk);
        f.format_chunk(&chk).unwrap();

        let w = f.finish().unwrap();
        let s = String::from_utf8(w).unwrap();
        println!("{}", s);
        s
    }

    // fn nt_xml_roundtrip(nt: &str, xml: &str) {
    //     assert_eq!(
    //         from_nt(nt), xml
    //     );
    // }

    fn nt_xml_roundtrip_prefix(nt: &str, xml: &str, prefix: IndexMap<&str, &str>){
        assert_eq!(
            from_nt_prefix(nt, prefix), xml
        );
    }

    // fn xml_roundtrip(xml: &str) {
    //     xml_roundtrip_prefix(xml, indexmap!("http://www.w3.org/1999/02/22-rdf-syntax-ns#" => "rdf"))
    // }

    // fn xml_roundtrip_prefix(xml: &str, prefix: IndexMap<&str, &str>){
    //     let mut source: Vec<AsRefTriple<String>> = vec![];
    //     let _: Vec<Result<(), RdfXmlError>>
    //         = RdfXmlParser::new(xml.as_bytes(), None).into_iter(
    //             |rio_triple| {
    //                 source.push(rio_triple.into());
    //                 Ok(())
    //             }
    //         ).collect();


    //     let sink = vec![];

    //     let mut config = ChunkedRdfXmlFormatterConfig::new();
    //     config.prefix =
    //         prefix.into_iter().map(
    //             |(k, v)|
    //             (k.to_string(), v.to_string())
    //         ).collect();

    //     let mut f = ChunkedRdfXmlFormatter::new(sink,config).unwrap();
    //     f.format_chunk(
    //         &AsRefChunk::from_raw(
    //             source.into_iter().map(|t| AsRefExpandedTriple::AsRefTriple(t)).collect()
    //         )
    //     ).unwrap();

    //     let w = f.finish().unwrap();
    //     let s = String::from_utf8(w).unwrap();
    //     println!("{}", s);
    //     assert_eq!(
    //         s, xml
    //     )
    // }

    #[test]
    fn example4_single_triple() {
        nt_xml_roundtrip_prefix(
r###"<http://www.w3.org/TR/rdf-syntax-grammar> <http://purl.org/dc/elements/1.1/title> "RDF1.1 XML Syntax" .
"### ,
r###"
<?xml version="1.0"?>
<rdf:RDF xmlns:rdf="http://www.w3.org/1999/02/22-rdf-syntax-ns#"
            xmlns:dc="http://purl.org/dc/elements/1.1/"
            xmlns:ex="http://example.org/stuff/1.0/">

  <rdf:Description rdf:about="http://www.w3.org/TR/rdf-syntax-grammar"
             dc:title="RDF1.1 XML Syntax"/>
</rdf:RDF>"### ,
            spec_prefix()
        )
    }

    #[test]
    fn example4_multiple_property_elements(){
        nt_xml_roundtrip_prefix(
r###"<http://www.w3.org/TR/rdf-syntax-grammar> <http://purl.org/dc/elements/1.1/title> "RDF1.1 XML Syntax" .
<http://www.w3.org/TR/rdf-syntax-grammar> <http://example.org/stuff/1.0/editor> _:genid1 .
_:genid1 <http://example.org/stuff/1.0/fullName> "Dave Beckett" .
_:genid1 <http://example.org/stuff/1.0/homePage> <http://purl.org/net/dajobe/> ."### ,

r###"
<?xml version="1.0"?>
<rdf:RDF xmlns:rdf="http://www.w3.org/1999/02/22-rdf-syntax-ns#"
            xmlns:dc="http://purl.org/dc/elements/1.1/"
            xmlns:ex="http://example.org/stuff/1.0/">

  <rdf:Description rdf:about="http://www.w3.org/TR/rdf-syntax-grammar"
             dc:title="RDF1.1 XML Syntax">
    <ex:editor>
      <rdf:Description ex:fullName="Dave Beckett">
        <ex:homePage rdf:resource="http://purl.org/net/dajobe/" />
      </rdf:Description>
    </ex:editor>
  </rdf:Description>

</rdf:RDF>"### ,
            spec_prefix()
        )
    }
}
