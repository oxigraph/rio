use crate::utils::{is_name_char, is_name_start_char};

use indexmap::IndexMap;
use quick_xml::{Writer, events::{BytesDecl, BytesEnd, BytesStart, BytesText, Event}};
use rio_api::model::{BlankNode, Literal, NamedNode, NamedOrBlankNode, Term, Triple};

use std::{collections::{HashMap, VecDeque}, fmt::{Debug, Formatter}};
use std::{self, cell::RefCell, fmt,
          hash::{Hash,Hasher},
          io::{self, Write}};

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

#[derive(Eq, PartialEq, Debug, Clone)]
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

#[derive(Eq, PartialEq, Debug, Clone)]
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

impl<A:AsRef<str>> AsRefTriple<A> {
    pub fn is_type(&self) -> bool {
        self.predicate.iri.as_ref() == "http://www.w3.org/1999/02/22-rdf-syntax-ns#type"
    }

    pub fn is_collection(&self) -> bool {
        self.is_collection_first() || self.is_collection_rest()
    }

    pub fn is_collection_first(&self) -> bool {
        &self.predicate.iri.as_ref() == &"http://www.w3.org/1999/02/22-rdf-syntax-ns#first"
    }

    pub fn is_collection_rest(&self) -> bool {
        &self.predicate.iri.as_ref() == &"http://www.w3.org/1999/02/22-rdf-syntax-ns#rest"
    }

    pub fn is_collection_end(&self) -> bool {
        if let AsRefTerm::NamedNode(nn) = &self.object {
            nn.iri.as_ref() == "http://www.w3.org/1999/02/22-rdf-syntax-ns#nil"
        } else {
            false
        }
    }
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

trait TripleLike<A>
    where A:AsRef<str> + Clone
{
    /// Can a new Triple be accepted onto this TripleLike.
    fn accept(&mut self, t:AsRefTriple<A>) -> Option<AsRefTriple<A>>;

    /// What is the subject of the triple like
    fn subject(&self) -> &AsRefNamedOrBlankNode<A>;

    fn literal_objects(&self) -> Vec<&AsRefTriple<A>>;

    fn find_typed(&self) -> Option<&AsRefTriple<A>>;
}

// A set of triples with a shared subject
// All the triples in `vec` should start with `subject`.
#[derive(Clone, Debug, Eq, PartialEq)]
pub struct AsRefMultiTriple<A:AsRef<str>> {
    vec: Vec<AsRefTriple<A>>,
}

impl<A> AsRefMultiTriple<A>
where A: AsRef<str> + PartialEq
{
    pub fn len(&self) -> usize {
        self.vec.len()
    }
}

impl<A> TripleLike<A> for AsRefMultiTriple<A>
where A: AsRef<str> + Clone + PartialEq
{
    fn accept(&mut self, t:AsRefTriple<A>) -> Option<AsRefTriple<A>> {
        if self.subject().as_ref() == t.subject.as_ref() {
            self.vec.push(t);
            None
        } else {
            Some(t)
        }
    }

    fn subject(&self) -> &AsRefNamedOrBlankNode<A> {
        // There should be no empty instances, so this should be safe
        &self.vec[0].subject
    }

    fn literal_objects(&self) -> Vec<&AsRefTriple<A>> {
        self.vec.iter().filter(|t| matches!(t.object, AsRefTerm::Literal(_))).collect()
    }

    fn find_typed(&self) -> Option<&AsRefTriple<A>> {
        self.vec.iter().find(
            |et| et.is_type()
        )
    }
}


// A set of terms that should be rendered as a RDF list, using first
// as a the subject of the first node
#[derive(Clone, Debug, Eq, PartialEq)]
pub struct AsRefTripleSeq<A:AsRef<str>> {
    list_seq: VecDeque<(AsRefNamedOrBlankNode<A>, Option<AsRefTriple<A>>)>,
}

impl<A:AsRef<str>> AsRefTripleSeq<A> {
    fn from_end(t:AsRefTriple<A>) -> AsRefTripleSeq<A> {
        let mut seq = AsRefTripleSeq{list_seq: vec![].into()};
        if let AsRefNamedOrBlankNode::BlankNode(_) = &t.subject {
            seq.list_seq.push_front((t.subject, None));
        } else {
            todo!("This shouldn't happen")
        }
        seq
    }

}

impl<A> TripleLike<A> for AsRefTripleSeq<A>
where A: AsRef<str> + Clone + Debug + Eq + PartialEq
{
    fn accept(&mut self, t:AsRefTriple<A>) -> Option<AsRefTriple<A>> {
        if t.is_collection_first() {
            if let Some(pos) = self.list_seq.iter().position(
                |tup| &tup.0 == &t.subject
            ){
                if let Some(tuple) = self.list_seq.get_mut(pos){
                    (*tuple).1 = Some(t)
                }

                return None;
            }
        }

        if let AsRefTerm::BlankNode(bn) = &t.object {
            if let &AsRefNamedOrBlankNode::BlankNode(ref snn) = self.subject() {
                if t.is_collection_rest() && snn == bn {
                    self.list_seq.push_front((t.subject, None));
                    return None;
                }
            }
        }

        Some(t)
    }

    fn subject(&self) -> &AsRefNamedOrBlankNode<A> {
        &self.list_seq[0].0
    }

    fn literal_objects(&self) -> Vec<&AsRefTriple<A>> {
        vec![]
    }

    fn find_typed(&self) -> Option<&AsRefTriple<A>> {
        None
    }
}

// All the different forms of RDF subgraph
#[derive(Clone, Debug, Eq, PartialEq)]
pub enum AsRefExpandedTriple<A:AsRef<str>> {
    AsRefMultiTriple(AsRefMultiTriple<A>),
    AsRefTripleSeq(AsRefTripleSeq<A>),
}

impl<A> From<AsRefTriple<A>> for AsRefMultiTriple<A>
where A: AsRef<str> + Clone + Debug + Eq + PartialEq
{
    fn from(t: AsRefTriple<A>) -> Self {
        AsRefMultiTriple{
            vec: vec![t]
        }
    }
}

impl<A> From<AsRefTriple<A>> for AsRefExpandedTriple<A>
where A: AsRef<str> + Clone + Debug + Eq + PartialEq
{
    fn from(t: AsRefTriple<A>) -> Self {
        let t:AsRefMultiTriple<A> = t.into();
        t.into()
    }
}

impl<A> From<AsRefMultiTriple<A>> for AsRefExpandedTriple<A>
where A: AsRef<str> + Clone + Debug + Eq + PartialEq
{
    fn from(t: AsRefMultiTriple<A>) -> Self {
        AsRefExpandedTriple::AsRefMultiTriple(t)
    }
}

impl<A> TripleLike<A> for AsRefExpandedTriple<A>
where A: AsRef<str> + Clone + Debug + Eq + PartialEq {
    fn accept(&mut self, triple: AsRefTriple<A>) -> Option<AsRefTriple<A>> {
        match self {
            Self::AsRefMultiTriple(mt) => mt.accept(triple),
            Self::AsRefTripleSeq(seq) => seq.accept(triple),
        }
    }

    fn subject(&self) -> &AsRefNamedOrBlankNode<A> {
        match self {
            Self::AsRefMultiTriple(mt) => mt.subject(),
            Self::AsRefTripleSeq(seq) => seq.subject(),
        }
    }

    fn literal_objects(&self) -> Vec<&AsRefTriple<A>> {
        match self {
            Self::AsRefMultiTriple(mt) => mt.literal_objects(),
            Self::AsRefTripleSeq(seq) => seq.literal_objects(),
        }
    }

    fn find_typed(&self) -> Option<&AsRefTriple<A>> {
        match self {
            Self::AsRefMultiTriple(mt) => mt.find_typed(),
            Self::AsRefTripleSeq(seq) => seq.find_typed(),
        }
    }
}



/// A chunk of RDF that should that should be coherent.
/// Current invariants:
///   - each subject should appear only once (i.e. all subjects are
///   grouped in AsRefMultiTriple, or AsRefSeq)
///   - if a subject appears it represents all appearances of the node
///   as a subject in the document of which this is a chunk
///   - if BNodes appear as subjects, they appear after any
///   apperance as an object (TODO: Not implemented yet!)
#[derive(Debug)]
pub struct AsRefChunk<A:AsRef<str>>(
    // triples in reverse order for efficient popping
    Vec<AsRefExpandedTriple<A>>
);


impl<A> AsRefChunk<A>
where A: AsRef<str> + Clone + Debug + Eq + Hash + PartialEq
{
    pub fn normalize(v:Vec<AsRefTriple<A>>) -> Self {
        let mut etv:IndexMap<AsRefNamedOrBlankNode<A>, AsRefMultiTriple<A>> = Default::default();
        let mut seq:Vec<AsRefTripleSeq<A>> = vec![];
        let mut seq_rest:HashMap<AsRefNamedOrBlankNode<A>, AsRefTriple<A>> = Default::default();
        let mut seq_first:HashMap<AsRefNamedOrBlankNode<A>, AsRefTriple<A>> = Default::default();

        'top: for t in v {

            // We have a collection add. Create a new seq and store it
            if t.is_collection_end() {
                seq.push(AsRefTripleSeq::from_end(t));
                continue 'top;
            }

            // We have a collection part. Remember for later
            if t.is_collection_rest() {
                if let AsRefTerm::BlankNode(bn) = t.object.clone() {
                    seq_rest.insert(AsRefNamedOrBlankNode::BlankNode(bn), t);
                }
                continue 'top;
            }
            if t.is_collection_first() {
                seq_first.insert(t.subject.clone(), t);
                continue 'top;
            }

            // We have something else. Combine it with existing multi
            // triples
            if let Some(multi) = etv.get_mut(&t.subject) {
                multi.accept(t);
            } else {
                // We have an orphan triple, store it a new multi
                etv.insert(t.subject.clone(), t.into());
            }
        }

        for s in seq.iter_mut() {
            loop {
                if let Some(t) = seq_first.remove(s.subject()) {
                    s.accept(t);
                }

                if let Some(t) = seq_rest.remove(s.subject()) {
                    s.accept(t);
                } else {
                    break;
                }
            }
        }

        let mut etv:Vec<AsRefExpandedTriple<A>> = etv.into_iter()
            .map(|(_k, v)| v.into())
            .collect();

        etv.reverse();

        let mut v:Vec<_> = seq.into_iter()
            .map(|s| AsRefExpandedTriple::AsRefTripleSeq(s))
            .collect();
        v.append(&mut etv);
        AsRefChunk(v)
    }

    pub fn from_raw(vec:Vec<AsRefExpandedTriple<A>>) -> Self {
        AsRefChunk(vec)
    }

    pub fn empty() -> Self {
        AsRefChunk(vec![])
    }

    pub fn remove_et(&mut self, et: &AsRefExpandedTriple<A>) -> bool {
        if let Some(pos) = self.0.iter().position(|tet| tet == et) {
            self.0.remove(pos);
            true
        } else {
            false
        }
    }

    pub fn insert(&mut self, et:AsRefExpandedTriple<A>){
        self.0.insert(1, et);
    }

    pub fn next(&mut self) -> Option<AsRefExpandedTriple<A>> {
        self.0.pop()
    }

    pub fn remove_subject(&mut self, iri_or_id:&A) -> Option<AsRefExpandedTriple<A>> {
        self.0.iter().position(|et| et.subject().as_ref() == iri_or_id.as_ref())
            .map(|pos|self.0.remove(pos))
    }

    pub fn find_subject(&self, iri_or_id:&A) -> Option<AsRefExpandedTriple<A>> {
        self.0.iter().find(|et| et.subject().as_ref() == iri_or_id.as_ref()).cloned()
    }

    pub fn filter_subject<'a>(&'a self, nnb:&'a AsRefNamedOrBlankNode<A>)
                          -> impl Iterator<Item=&'a AsRefExpandedTriple<A>> {
        self.0.iter().filter(move |et| et.subject().as_ref() == nnb.as_ref())
    }
}


#[derive(Clone, Debug, Default)]
pub struct ChunkedRdfXmlFormatterConfig {
    pub bnode_contract: bool,
    pub indentation: usize,
    pub prefix: IndexMap<String, String>,
    pub typed_node: bool
}

impl ChunkedRdfXmlFormatterConfig {
    pub fn new() -> Self {
        ChunkedRdfXmlFormatterConfig {
            bnode_contract: false,
            indentation: 4,
            prefix: IndexMap::new(),
            typed_node: false
        }
    }
}

pub struct ChunkedRdfXmlFormatter<A:AsRef<str>, W: Write> {
    writer: Writer<W>,
    config: ChunkedRdfXmlFormatterConfig,
    pub (crate) open_tag_stack: Vec<Vec<u8>>,
    last_open_tag: Option<BytesStart<'static>>,
    chunk: AsRefChunk<A>,
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
            chunk: AsRefChunk::empty(),
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

    fn format_head<'a, T:TripleLike<A> + Debug>(&mut self, triple_like:&'a T, _chunk:&AsRefChunk<A>)
                                        -> Result<Vec<&'a AsRefTriple<A>>, io::Error> {
        let mut triples_rendered = vec![];
        let mut description_open =
            match triple_like.find_typed() {
                Some(t) => {
                    if let AsRefTerm::NamedNode(nn) = &t.object {
                        triples_rendered.push(t);
                        self.bytes_start_iri(nn)
                    } else {
                        panic!("BNodes cannot be typed, I think")
                    }
                },
                None => {
                    BytesStart::borrowed_name(b"rdf:Description")
                }
            };

        match triple_like.subject() {
            AsRefNamedOrBlankNode::NamedNode(ref n) => {
                description_open.push_attribute(("rdf:about", n.iri.as_ref()))
            }
            AsRefNamedOrBlankNode::BlankNode(_) => {
                // Empty
            }
        }

        // TODO: Shares lots of code with format_property
        // TODO: check all properties unique!!
        for literal_t in triple_like.literal_objects() {
            if let AsRefTerm::Literal(l) = &literal_t.object {
                match l {
                    AsRefLiteral::Simple {value} => {
                        let (iri_protocol_and_host, iri_qname) = literal_t.predicate.split_iri();
                        if let Some(iri_ns_prefix) = &self.config.prefix.get(iri_protocol_and_host) {
                            description_open.push_attribute(
                                (
                                    &format!("{}:{}", &iri_ns_prefix, &iri_qname)[..],
                                    value.as_ref()
                                )
                            );
                            triples_rendered.push(literal_t);
                        } else {
                           todo!()
                        }
                    }
                    AsRefLiteral::LanguageTaggedString {value:_, language:_} => {
                        // Don't do anything here, because the
                        // language environment is wrong. Render later.
                    }
                    AsRefLiteral::Typed {value:_, datatype:_} => {
                        // Don't do anything here because we need to
                        // render later.
                    }
                }
            } else {
                debug_assert!(false, "Non literal object returned from literal object method");
            }
        }
        self.write_start(Event::Start(description_open))
            .map_err(map_err)?;

        Ok(triples_rendered)
    }

    fn format_object(&mut self,
                     mut property_open:BytesStart<'_>,
                     object:&AsRefTerm<A>, chunk:&mut AsRefChunk<A>,
                     collection: bool
    )
                     -> Result<(), io::Error> {
        match object {
            AsRefTerm::NamedNode(n) => {
                // Rewrite: 2.4 Empty Property Elements
                if collection {
                    property_open.push_attribute(("rdf:about", n.iri.as_ref()));
                } else {
                    property_open.push_attribute(("rdf:resource", n.iri.as_ref()));
                }

                self.write_start(Event::Start(property_open))
                    .map_err(map_err)?;
            }
            AsRefTerm::BlankNode(n) => {
                if let Some(t) = chunk.find_subject(&n.id) {
                    if let AsRefExpandedTriple::AsRefTripleSeq(_) = t {
                        property_open.push_attribute(("rdf:parseType", "Collection"));
                    }
                    self.write_start(Event::Start(property_open))
                        .map_err(map_err)?;
                    self.format_expanded(&t, chunk)?;
                } else {
                    property_open.push_attribute(("rdf:nodeID", n.id.as_ref()));
                    self.write_start(Event::Start(property_open))
                        .map_err(map_err)?;
                }
            }
            AsRefTerm::Literal(l) => {
                let content =
                    match l {
                        AsRefLiteral::Simple { value } => value,
                        AsRefLiteral::LanguageTaggedString { value, language } => {
                            property_open.push_attribute(("xml:lang", language.as_ref()));
                            value
                        }
                        AsRefLiteral::Typed { value, datatype } => {
                            property_open.push_attribute(("rdf:datatype", datatype.iri.as_ref()));
                            value
                        }
                    };
                self.write_start(Event::Start(property_open))
                    .map_err(map_err)?;
                self.write_event(Event::Text(BytesText::from_plain_str(&content.as_ref())))
                    .map_err(map_err)?;
            },
        };

        Ok(())
    }

    fn format_property_arc(&mut self, triple: &AsRefTriple<A>,
                           rendered_in_head:&Vec<&AsRefTriple<A>>,
                           chunk:&mut AsRefChunk<A>,
    ) -> Result<(), io::Error> {
        if rendered_in_head.contains(&triple) {
            return Ok(())
        }

        let property_open = self.bytes_start_iri(&triple.predicate);
        self.format_object(property_open, &triple.object, chunk, false)?;

        self.write_close()?;
        Ok(())
    }

    fn format_seq(&mut self, seq: &AsRefTripleSeq<A>, chunk:&mut AsRefChunk<A>)
                  -> Result<(), io::Error> {
        for tup in seq.list_seq.iter() {
            if let Some(ref triple) = tup.1 {
                let property_open = BytesStart::borrowed_name(b"rdf:Description");
                self.format_object(property_open, &triple.object, chunk, true)?;
                self.write_close()?;
            }
        }

        Ok(())
    }

    fn format_multi(&mut self, multi_triple: &AsRefMultiTriple<A>,
                    chunk:&mut AsRefChunk<A>,
    ) -> Result<(), io::Error> {
        let rendered_in_head = self.format_head(multi_triple, chunk)?;

        // Rewrite: 2.3 Multiple Property Elements
        for triple in multi_triple.vec.iter() {
            self.format_property_arc(triple, &rendered_in_head, chunk)?;
        }

        self.write_close()?;
        Ok(())
    }

    fn format_expanded(&mut self, expanded:&AsRefExpandedTriple<A>,
                       chunk:&mut AsRefChunk<A>,
    ) -> Result<(), io::Error> {
        chunk.remove_et(expanded);

        match expanded {
            AsRefExpandedTriple::AsRefMultiTriple(ref mt) => {
                self.format_multi(mt, chunk)?;
            }
            AsRefExpandedTriple::AsRefTripleSeq(ref seq) =>{
                self.format_seq(seq, chunk)?;
            }
        }

        Ok(())
    }

    pub fn chunk_triple(&mut self, triple:AsRefTriple<A>) {
        self.chunk.insert(triple.into());
    }

    pub fn chunk_multi(&mut self, multi: AsRefMultiTriple<A>) {
        self.chunk.insert(multi.into())
    }

    pub fn finish_chunk(&mut self) -> Result<(), io::Error> {
        let mut chk = AsRefChunk::empty();
        std::mem::swap(&mut self.chunk, &mut chk);
        self.format_chunk(chk)
    }

    pub fn format(&mut self, triple:AsRefTriple<A>) -> Result<(), io::Error> {
        self.format_chunk(AsRefChunk::from_raw(
            vec![triple.into()]
        ))
    }

   pub fn format_chunk(&mut self, mut chunk:AsRefChunk<A>) -> Result<(), io::Error> {
        loop {
            let optet = chunk.next();
            if let Some(et) = optet {
                self.format_expanded(&et, &mut chunk)?;
            } else {
                break;
            }
        }
        Ok(())
    }

    /// Finishes writing and returns the underlying `Write`
    pub fn finish(mut self) -> Result<W, io::Error> {
        while !self.open_tag_stack.is_empty() {
            self.write_close()?;
        }

        self.finish_chunk()?;

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

        assert_eq!(chk.0.len(), 1);
    }

    fn spec_prefix() -> IndexMap<&'static str, &'static str> {
        indexmap![
            "http://www.w3.org/1999/02/22-rdf-syntax-ns#" => "rdf",
            "http://purl.org/dc/elements/1.1/" => "dc",
            "http://example.org/stuff/1.0/" => "ex"
        ]
    }

    #[allow(dead_code)]
    fn from_nt(nt: &str) -> String {
        from_nt_prefix(nt, indexmap!("http://www.w3.org/1999/02/22-rdf-syntax-ns#" => "rdf"))
    }

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
        f.format_chunk(chk).unwrap();

        let w = f.finish().unwrap();
        let s = String::from_utf8(w).unwrap();
        println!("{}", s);
        s
    }

    fn nt_xml_roundtrip_prefix(nt: &str, xml: &str, prefix: IndexMap<&str, &str>){
        assert_eq!(
            from_nt_prefix(nt, prefix), xml
        );
    }

    #[allow(dead_code)]
    fn nt_xml_roundtrip(nt: &str, xml: &str) {
        assert_eq!(
            from_nt(nt), xml
        );
    }

    #[test]
    fn example4_single_triple() {
        nt_xml_roundtrip_prefix(
r###"<http://www.w3.org/TR/rdf-syntax-grammar> <http://purl.org/dc/elements/1.1/title> "RDF1.1 XML Syntax" .
"### ,
r###"<?xml version="1.0" encoding="UTF-8"?>
<rdf:RDF xmlns:rdf="http://www.w3.org/1999/02/22-rdf-syntax-ns#" xmlns:dc="http://purl.org/dc/elements/1.1/" xmlns:ex="http://example.org/stuff/1.0/">
    <rdf:Description rdf:about="http://www.w3.org/TR/rdf-syntax-grammar" dc:title="RDF1.1 XML Syntax"/>
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

r###"<?xml version="1.0" encoding="UTF-8"?>
<rdf:RDF xmlns:rdf="http://www.w3.org/1999/02/22-rdf-syntax-ns#" xmlns:dc="http://purl.org/dc/elements/1.1/" xmlns:ex="http://example.org/stuff/1.0/">
    <rdf:Description rdf:about="http://www.w3.org/TR/rdf-syntax-grammar" dc:title="RDF1.1 XML Syntax">
        <ex:editor>
            <rdf:Description ex:fullName="Dave Beckett">
                <ex:homePage rdf:resource="http://purl.org/net/dajobe/"/>
            </rdf:Description>
        </ex:editor>
    </rdf:Description>
</rdf:RDF>"### ,
            spec_prefix()
        );
    }

    #[test]
    fn example14_typed_nodes() {
        nt_xml_roundtrip_prefix(
r###"<http://example.org/thing> <http://www.w3.org/1999/02/22-rdf-syntax-ns#type> <http://example.org/stuff/1.0/Document> .
<http://example.org/thing> <http://purl.org/dc/elements/1.1/title> "A marvelous thing" ."### ,

r###"<?xml version="1.0" encoding="UTF-8"?>
<rdf:RDF xmlns:rdf="http://www.w3.org/1999/02/22-rdf-syntax-ns#" xmlns:dc="http://purl.org/dc/elements/1.1/" xmlns:ex="http://example.org/stuff/1.0/">
    <ex:Document rdf:about="http://example.org/thing" dc:title="A marvelous thing"/>
</rdf:RDF>"###  ,
            spec_prefix()
        )
    }

    #[test]
    fn example19_collections() {
        nt_xml_roundtrip_prefix(
r###"_:genid1 <http://www.w3.org/1999/02/22-rdf-syntax-ns#first> <http://example.org/banana> .
_:genid2 <http://www.w3.org/1999/02/22-rdf-syntax-ns#first> <http://example.org/apple> .
_:genid1 <http://www.w3.org/1999/02/22-rdf-syntax-ns#rest> _:genid2 .
_:genid3 <http://www.w3.org/1999/02/22-rdf-syntax-ns#first> <http://example.org/pear> .
_:genid2 <http://www.w3.org/1999/02/22-rdf-syntax-ns#rest> _:genid3 .
_:genid3 <http://www.w3.org/1999/02/22-rdf-syntax-ns#rest> <http://www.w3.org/1999/02/22-rdf-syntax-ns#nil> .
<http://example.org/basket> <http://example.org/stuff/1.0/hasFruit> _:genid1 ."### ,

r###"<?xml version="1.0" encoding="UTF-8"?>
<rdf:RDF xmlns:rdf="http://www.w3.org/1999/02/22-rdf-syntax-ns#" xmlns:dc="http://purl.org/dc/elements/1.1/" xmlns:ex="http://example.org/stuff/1.0/">
    <rdf:Description rdf:about="http://example.org/basket">
        <ex:hasFruit rdf:parseType="Collection">
            <rdf:Description rdf:about="http://example.org/banana"/>
            <rdf:Description rdf:about="http://example.org/apple"/>
            <rdf:Description rdf:about="http://example.org/pear"/>
        </ex:hasFruit>
    </rdf:Description>
</rdf:RDF>"### ,
            spec_prefix()
        )
    }
}

