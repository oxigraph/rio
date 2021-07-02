use quick_xml::events::*;
use quick_xml::{Reader, Writer};
use rio_api::model::*;
use rio_api::parser::TriplesParser;
use std::convert::TryInto;
use std::io::BufRead;
use std::str;

use crate::error::{RdfXmlError, RdfXmlErrorKind};
use crate::model::*;
use crate::utils::*;
use oxilangtag::LanguageTag;
use oxiri::Iri;
use quick_xml::escape::unescape_with;
use quick_xml::events::attributes::Attribute;
use std::collections::{HashMap, HashSet};

/// A [RDF/XML](https://www.w3.org/TR/rdf-syntax-grammar/) streaming parser.
///
/// It implements the [`TriplesParser`] trait.
/// It reads the file in streaming.
/// It does not keep data in memory except a stack for handling nested XML tags, and a set of all
/// seen `rdf:ID`s to detect duplicate ids and fail according to the specification.
///
/// Its performances are not optimized yet and hopefully could be significantly enhanced by reducing the
/// number of allocations and copies done by the parser.
///
/// Count the number of people using the [`TriplesParser`] API without proper error management:
/// ```
/// use rio_xml::{RdfXmlParser, RdfXmlError};
/// use rio_api::parser::TriplesParser;
/// use rio_api::model::NamedNode;
///
/// let file = b"<?xml version=\"1.0\"?>
/// <rdf:RDF xmlns:rdf=\"http://www.w3.org/1999/02/22-rdf-syntax-ns#\" xmlns:schema=\"http://schema.org/\">
///  <rdf:Description rdf:about=\"http://example.com/foo\">
///    <rdf:type rdf:resource=\"http://schema.org/Person\" />
///    <schema:name>Foo</schema:name>
///  </rdf:Description>
///  <schema:Person rdf:about=\"http://example.com/bar\" schema:name=\"Bar\" />
/// </rdf:RDF>";
///
/// let rdf_type = NamedNode { iri: "http://www.w3.org/1999/02/22-rdf-syntax-ns#type" };
/// let schema_person = NamedNode { iri: "http://schema.org/Person" };
/// let mut count = 0;
/// RdfXmlParser::new(file.as_ref(), None).parse_all(&mut |t| {
///     if t.predicate == rdf_type && t.object == schema_person.into() {
///         count += 1;
///     }
///     Ok(()) as Result<(), RdfXmlError>
/// })?;
/// assert_eq!(2, count);
/// # Result::<_,RdfXmlError>::Ok(())
/// ```
pub struct RdfXmlParser<R: BufRead> {
    reader: RdfXmlReader<R>,
    reader_buffer: Vec<u8>,
    is_end: bool,
}

impl<R: BufRead> RdfXmlParser<R> {
    /// Builds the parser from a `BufRead` implementation, and a base IRI for relative IRI resolution.
    pub fn new(reader: R, base_iri: Option<Iri<String>>) -> Self {
        let mut reader = Reader::from_reader(reader);
        reader.expand_empty_elements(true);
        Self {
            reader: RdfXmlReader {
                reader,
                state: vec![RdfXmlState::Doc { base_iri }],
                namespace_buffer: Vec::default(),
                bnode_id_generator: BlankNodeIdGenerator::default(),
                custom_entities: HashMap::default(),
                in_literal_depth: 0,
                known_rdf_id: HashSet::default(),
            },
            reader_buffer: Vec::default(),
            is_end: false,
        }
    }
}

impl<R: BufRead> TriplesParser for RdfXmlParser<R> {
    type Error = RdfXmlError;

    fn parse_step<E: From<RdfXmlError>>(
        &mut self,
        on_triple: &mut impl FnMut(Triple<'_>) -> Result<(), E>,
    ) -> Result<(), E> {
        let (_, event) = self
            .reader
            .reader
            .read_namespaced_event(&mut self.reader_buffer, &mut self.reader.namespace_buffer)
            .map_err(RdfXmlError::from)?;
        match event {
            Event::DocType(dt) => self.reader.parse_doctype(dt)?,
            Event::Start(event) => self.reader.parse_start_event(event, on_triple)?,
            Event::Text(event) => self.reader.parse_text_event(event)?,
            Event::End(event) => self.reader.parse_end_event(event, on_triple)?,
            Event::Eof => {
                self.is_end = true;
            }
            _ => {}
        };
        Ok(())
    }

    fn is_end(&self) -> bool {
        self.is_end
    }
}

const RDF_ABOUT: &str = "http://www.w3.org/1999/02/22-rdf-syntax-ns#about";
const RDF_ABOUT_EACH: &str = "http://www.w3.org/1999/02/22-rdf-syntax-ns#aboutEach";
const RDF_ABOUT_EACH_PREFIX: &str = "http://www.w3.org/1999/02/22-rdf-syntax-ns#aboutEachPrefix";
const RDF_BAG_ID: &str = "http://www.w3.org/1999/02/22-rdf-syntax-ns#bagID";
const RDF_DATATYPE: &str = "http://www.w3.org/1999/02/22-rdf-syntax-ns#datatype";
const RDF_DESCRIPTION: &str = "http://www.w3.org/1999/02/22-rdf-syntax-ns#Description";
const RDF_FIRST: &str = "http://www.w3.org/1999/02/22-rdf-syntax-ns#first";
const RDF_ID: &str = "http://www.w3.org/1999/02/22-rdf-syntax-ns#ID";
const RDF_LI: &str = "http://www.w3.org/1999/02/22-rdf-syntax-ns#li";
const RDF_NIL: &str = "http://www.w3.org/1999/02/22-rdf-syntax-ns#nil";
const RDF_NODE_ID: &str = "http://www.w3.org/1999/02/22-rdf-syntax-ns#nodeID";
const RDF_OBJECT: &str = "http://www.w3.org/1999/02/22-rdf-syntax-ns#object";
const RDF_PARSE_TYPE: &str = "http://www.w3.org/1999/02/22-rdf-syntax-ns#parseType";
const RDF_PREDICATE: &str = "http://www.w3.org/1999/02/22-rdf-syntax-ns#predicate";
const RDF_RDF: &str = "http://www.w3.org/1999/02/22-rdf-syntax-ns#RDF";
const RDF_REST: &str = "http://www.w3.org/1999/02/22-rdf-syntax-ns#rest";
const RDF_RESOURCE: &str = "http://www.w3.org/1999/02/22-rdf-syntax-ns#resource";
const RDF_STATEMENT: &str = "http://www.w3.org/1999/02/22-rdf-syntax-ns#Statement";
const RDF_SUBJECT: &str = "http://www.w3.org/1999/02/22-rdf-syntax-ns#subject";
const RDF_TYPE: &str = "http://www.w3.org/1999/02/22-rdf-syntax-ns#type";
const RDF_XML_LITERAL: &str = "http://www.w3.org/1999/02/22-rdf-syntax-ns#XMLLiteral";

const RESERVED_RDF_ELEMENTS: [&str; 11] = [
    RDF_ABOUT,
    RDF_ABOUT_EACH,
    RDF_ABOUT_EACH_PREFIX,
    RDF_BAG_ID,
    RDF_DATATYPE,
    RDF_ID,
    RDF_LI,
    RDF_NODE_ID,
    RDF_PARSE_TYPE,
    RDF_RDF,
    RDF_RESOURCE,
];
const RESERVED_RDF_ATTRIBUTES: [&str; 5] = [
    RDF_ABOUT_EACH,
    RDF_ABOUT_EACH_PREFIX,
    RDF_LI,
    RDF_RDF,
    RDF_RESOURCE,
];

#[derive(Clone, Debug)]
enum NodeOrText {
    Node(OwnedSubject),
    Text(String),
}

enum RdfXmlState {
    Doc {
        base_iri: Option<Iri<String>>,
    },
    Rdf {
        base_iri: Option<Iri<String>>,
        language: Option<LanguageTag<String>>,
    },
    NodeElt {
        base_iri: Option<Iri<String>>,
        language: Option<LanguageTag<String>>,
        subject: OwnedSubject,
        li_counter: u64,
    },
    PropertyElt {
        //Resource, Literal or Empty property element
        iri: String,
        base_iri: Option<Iri<String>>,
        language: Option<LanguageTag<String>>,
        subject: OwnedSubject,
        object: Option<NodeOrText>,
        id_attr: Option<OwnedNamedNode>,
        datatype_attr: Option<OwnedNamedNode>,
    },
    ParseTypeCollectionPropertyElt {
        iri: String,
        base_iri: Option<Iri<String>>,
        language: Option<LanguageTag<String>>,
        subject: OwnedSubject,
        objects: Vec<OwnedSubject>,
        id_attr: Option<OwnedNamedNode>,
    },
    ParseTypeLiteralPropertyElt {
        iri: String,
        base_iri: Option<Iri<String>>,
        language: Option<LanguageTag<String>>,
        subject: OwnedSubject,
        writer: Writer<Vec<u8>>,
        id_attr: Option<OwnedNamedNode>,
        emit: bool, //false for parseTypeOtherPropertyElt support
    },
}

impl RdfXmlState {
    fn base_iri(&self) -> Option<&Iri<String>> {
        match self {
            RdfXmlState::Doc { base_iri, .. } => base_iri,
            RdfXmlState::Rdf { base_iri, .. } => base_iri,
            RdfXmlState::NodeElt { base_iri, .. } => base_iri,
            RdfXmlState::PropertyElt { base_iri, .. } => base_iri,
            RdfXmlState::ParseTypeCollectionPropertyElt { base_iri, .. } => base_iri,
            RdfXmlState::ParseTypeLiteralPropertyElt { base_iri, .. } => base_iri,
        }
        .as_ref()
    }

    fn language(&self) -> Option<&LanguageTag<String>> {
        match self {
            RdfXmlState::Doc { .. } => None,
            RdfXmlState::Rdf { language, .. } => language.as_ref(),
            RdfXmlState::NodeElt { language, .. } => language.as_ref(),
            RdfXmlState::PropertyElt { language, .. } => language.as_ref(),
            RdfXmlState::ParseTypeCollectionPropertyElt { language, .. } => language.as_ref(),
            RdfXmlState::ParseTypeLiteralPropertyElt { language, .. } => language.as_ref(),
        }
    }
}

struct RdfXmlReader<R: BufRead> {
    reader: Reader<R>,
    state: Vec<RdfXmlState>,
    namespace_buffer: Vec<u8>,
    bnode_id_generator: BlankNodeIdGenerator,
    custom_entities: HashMap<Vec<u8>, Vec<u8>>,
    in_literal_depth: usize,
    known_rdf_id: HashSet<String>,
}

impl<R: BufRead> RdfXmlReader<R> {
    fn parse_doctype(&mut self, dt: BytesText<'_>) -> Result<(), RdfXmlError> {
        // we extract entities
        for input in dt.split(|c| *c == b'<').skip(1) {
            if !input.starts_with(b"!ENTITY") {
                continue;
            }
            let mut input = trim_start(&input[7..]);
            if input.starts_with(b"%") {
                input = trim_start(&input[1..]);
            }
            let (entity_name, input) = split_once(input, is_whitespace).ok_or_else(|| {
                RdfXmlError::msg(
                    "<!ENTITY declarations should contain both an entity name and an entity value",
                )
            })?;
            let input = trim_start(input);
            if !input.starts_with(b"\"") {
                return Err(RdfXmlError::msg(
                    "<!ENTITY values should be enclosed in double quotes",
                ));
            }
            let input = &input[1..];
            let (entity_value, input) = split_once(input, |c| *c == b'"').ok_or_else(|| {
                RdfXmlError::msg("<!ENTITY declarations values should be enclosed in double quotes")
            })?;
            let input = trim_start(input);
            if !input.starts_with(b">") {
                return Err(RdfXmlError::msg(
                    "<!ENTITY declarations values should end with >",
                ));
            }
            self.custom_entities
                .insert(entity_name.to_vec(), entity_value.to_vec());
        }
        Ok(())
    }

    fn parse_start_event<E: From<RdfXmlError>>(
        &mut self,
        event: BytesStart<'_>,
        on_triple: &mut impl FnMut(Triple<'_>) -> Result<(), E>,
    ) -> Result<(), E> {
        //Literal case
        if let Some(RdfXmlState::ParseTypeLiteralPropertyElt { writer, .. }) = self.state.last_mut()
        {
            let mut clean_event = BytesStart::borrowed_name(event.name());
            for attr in event.attributes() {
                clean_event.push_attribute(attr.map_err(RdfXmlError::from)?);
            }
            writer
                .write_event(Event::Start(clean_event))
                .map_err(RdfXmlError::from)?;
            self.in_literal_depth += 1;
            return Ok(());
        }

        #[derive(PartialEq, Eq)]
        enum RdfXmlParseType {
            Default,
            Collection,
            Literal,
            Resource,
            Other,
        }

        #[derive(PartialEq, Eq)]
        enum RdfXmlNextProduction {
            Rdf,
            NodeElt,
            PropertyElt { subject: OwnedSubject },
        }

        let iri = self.resolve_tag_name(event.name())?;

        //We read attributes
        let (mut language, mut base_iri) = if let Some(current_state) = self.state.last() {
            (
                current_state.language().cloned(),
                current_state.base_iri().cloned(),
            )
        } else {
            (None, None)
        };

        let mut id_attr = None;
        let mut node_id_attr = None;
        let mut about_attr = None;
        let mut property_attrs = Vec::default();
        let mut resource_attr = None;
        let mut datatype_attr = None;
        let mut parse_type = RdfXmlParseType::Default;
        let mut type_attr = None;

        for attribute in event.attributes() {
            let attribute = attribute.map_err(RdfXmlError::from)?;
            match attribute.key {
                b"xml:lang" => {
                    let tag = self.convert_attribute(attribute)?;
                    language = Some(LanguageTag::parse(tag.to_ascii_lowercase()).map_err(
                        |error| RdfXmlError {
                            kind: RdfXmlErrorKind::InvalidLanguageTag { tag, error },
                        },
                    )?);
                }
                b"xml:base" => {
                    let iri = self.convert_attribute(attribute)?;
                    base_iri = Some(
                        Iri::parse(iri.clone())
                            .map_err(|error| RdfXmlError {
                                kind: RdfXmlErrorKind::InvalidIri { iri, error },
                            })?
                            .to_owned(),
                    )
                }
                key if !key.starts_with(b"xml") => {
                    let attribute_url = self.resolve_attribute_name(key)?;
                    if *attribute_url == *RDF_ID {
                        let mut id = self.convert_attribute(attribute)?;
                        if !is_nc_name(&id) {
                            return Err(RdfXmlError::msg(format!(
                                "{} is not a valid rdf:ID value",
                                &id
                            ))
                            .into());
                        }
                        id.insert(0, '#');
                        id_attr = Some(id);
                    } else if *attribute_url == *RDF_BAG_ID {
                        let bag_id = self.convert_attribute(attribute)?;
                        if !is_nc_name(&bag_id) {
                            return Err(RdfXmlError::msg(format!(
                                "{} is not a valid rdf:bagID value",
                                &bag_id
                            ))
                            .into());
                        }
                    } else if *attribute_url == *RDF_NODE_ID {
                        let id = self.convert_attribute(attribute)?;
                        if !is_nc_name(&id) {
                            return Err(RdfXmlError::msg(format!(
                                "{} is not a valid rdf:nodeID value",
                                &id
                            ))
                            .into());
                        }
                        node_id_attr = Some(OwnedBlankNode { id });
                    } else if *attribute_url == *RDF_ABOUT {
                        about_attr = Some(attribute);
                    } else if *attribute_url == *RDF_RESOURCE {
                        resource_attr = Some(attribute);
                    } else if *attribute_url == *RDF_DATATYPE {
                        datatype_attr = Some(attribute);
                    } else if *attribute_url == *RDF_PARSE_TYPE {
                        parse_type = match attribute.value.as_ref() {
                            b"Collection" => RdfXmlParseType::Collection,
                            b"Literal" => RdfXmlParseType::Literal,
                            b"Resource" => RdfXmlParseType::Resource,
                            _ => RdfXmlParseType::Other,
                        };
                    } else if *attribute_url == *RDF_TYPE {
                        type_attr = Some(attribute);
                    } else if RESERVED_RDF_ATTRIBUTES.contains(&&*attribute_url) {
                        return Err(RdfXmlError::msg(format!(
                            "{} is not a valid attribute",
                            &attribute_url
                        ))
                        .into());
                    } else {
                        property_attrs.push((
                            OwnedNamedNode { iri: attribute_url },
                            self.convert_attribute(attribute)?,
                        ));
                    }
                }
                _ => (), //We do not fail for unknown tags in the XML namespace
            }
        }

        //Parsing with the base URI
        let id_attr = match id_attr {
            Some(iri) => {
                let iri = resolve(&base_iri, iri)?;
                if self.known_rdf_id.contains(&iri) {
                    return Err(RdfXmlError::msg(format!(
                        "{} has already been used as rdf:ID value",
                        &iri
                    ))
                    .into());
                }
                self.known_rdf_id.insert(iri.clone());
                Some(OwnedNamedNode { iri })
            }
            None => None,
        };
        let about_attr = match about_attr {
            Some(attr) => Some(self.convert_iri_attribute(&base_iri, attr)?),
            None => None,
        };
        let resource_attr = match resource_attr {
            Some(attr) => Some(self.convert_iri_attribute(&base_iri, attr)?),
            None => None,
        };
        let datatype_attr = match datatype_attr {
            Some(attr) => Some(self.convert_iri_attribute(&base_iri, attr)?),
            None => None,
        };
        let type_attr = match type_attr {
            Some(attr) => Some(self.convert_iri_attribute(&base_iri, attr)?),
            None => None,
        };

        let expected_production = match self.state.last() {
            Some(RdfXmlState::Doc { .. }) => RdfXmlNextProduction::Rdf,
            Some(RdfXmlState::Rdf { .. }) => RdfXmlNextProduction::NodeElt,
            Some(RdfXmlState::NodeElt { subject, .. }) => RdfXmlNextProduction::PropertyElt {
                subject: subject.clone(),
            },
            Some(RdfXmlState::PropertyElt { .. }) => RdfXmlNextProduction::NodeElt,
            Some(RdfXmlState::ParseTypeCollectionPropertyElt { .. }) => {
                RdfXmlNextProduction::NodeElt
            }
            Some(RdfXmlState::ParseTypeLiteralPropertyElt { .. }) => {
                panic!("ParseTypeLiteralPropertyElt production children should never be considered as a RDF/XML content")
            }
            None => {
                return Err(
                    RdfXmlError::msg("No state in the stack: the XML is not balanced").into(),
                );
            }
        };

        let new_state = match expected_production {
            RdfXmlNextProduction::Rdf => {
                if *iri == *RDF_RDF {
                    RdfXmlState::Rdf { base_iri, language }
                } else if RESERVED_RDF_ELEMENTS.contains(&&*iri) {
                    return Err(RdfXmlError::msg(format!(
                        "Invalid node element tag name: {}",
                        &iri
                    ))
                    .into());
                } else {
                    self.build_node_elt(
                        OwnedNamedNode { iri },
                        base_iri,
                        language,
                        id_attr,
                        node_id_attr,
                        about_attr,
                        type_attr,
                        property_attrs,
                        on_triple,
                    )?
                }
            }
            RdfXmlNextProduction::NodeElt => {
                if RESERVED_RDF_ELEMENTS.contains(&&*iri) {
                    return Err(RdfXmlError::msg(format!(
                        "Invalid property element tag name: {}",
                        &iri
                    ))
                    .into());
                }
                self.build_node_elt(
                    OwnedNamedNode { iri },
                    base_iri,
                    language,
                    id_attr,
                    node_id_attr,
                    about_attr,
                    type_attr,
                    property_attrs,
                    on_triple,
                )?
            }
            RdfXmlNextProduction::PropertyElt { subject } => {
                let iri = if *iri == *RDF_LI {
                    if let Some(RdfXmlState::NodeElt { li_counter, .. }) = self.state.last_mut() {
                        *li_counter += 1;
                        format!("http://www.w3.org/1999/02/22-rdf-syntax-ns#_{}", li_counter)
                    } else {
                        return Err(RdfXmlError::msg(format!(
                            "Invalid property element tag name: {}",
                            &iri
                        ))
                        .into());
                    }
                } else if RESERVED_RDF_ELEMENTS.contains(&&*iri) || *iri == *RDF_DESCRIPTION {
                    return Err(RdfXmlError::msg(format!(
                        "Invalid property element tag name: {}",
                        &iri
                    ))
                    .into());
                } else {
                    iri
                };
                match parse_type {
                    RdfXmlParseType::Default => {
                        if resource_attr.is_some()
                            || node_id_attr.is_some()
                            || !property_attrs.is_empty()
                        {
                            let object: OwnedSubject = match (resource_attr, node_id_attr)
                    {
                        (Some(resource_attr), None) => resource_attr.into(),
                        (None, Some(node_id_attr)) => node_id_attr.into(),
                        (None, None) => OwnedBlankNode {
                            id: self.bnode_id_generator.generate().as_ref().to_owned(),
                        }.into(),
                        (Some(_), Some(_)) => return Err(RdfXmlError::msg("Not both rdf:resource and rdf:nodeID could be set at the same time").into())
                    };
                            self.emit_property_attrs(
                                (&object).into(),
                                property_attrs,
                                &language,
                                on_triple,
                            )?;
                            if let Some(type_attr) = type_attr {
                                on_triple(Triple {
                                    subject: (&object).into(),
                                    predicate: NamedNode { iri: RDF_TYPE },
                                    object: NamedNode::from(&type_attr).into(),
                                })?;
                            }
                            RdfXmlState::PropertyElt {
                                iri,
                                base_iri,
                                language,
                                subject,
                                object: Some(NodeOrText::Node(object)),
                                id_attr,
                                datatype_attr,
                            }
                        } else {
                            RdfXmlState::PropertyElt {
                                iri,
                                base_iri,
                                language,
                                subject,
                                object: None,
                                id_attr,
                                datatype_attr,
                            }
                        }
                    }
                    RdfXmlParseType::Literal => RdfXmlState::ParseTypeLiteralPropertyElt {
                        iri,
                        base_iri,
                        language,
                        subject,
                        writer: Writer::new(Vec::default()),
                        id_attr,
                        emit: true,
                    },
                    RdfXmlParseType::Resource => self.build_parse_type_resource_property_elt(
                        OwnedNamedNode { iri },
                        base_iri,
                        language,
                        subject,
                        id_attr,
                        on_triple,
                    )?,
                    RdfXmlParseType::Collection => RdfXmlState::ParseTypeCollectionPropertyElt {
                        iri,
                        base_iri,
                        language,
                        subject,
                        objects: Vec::default(),
                        id_attr,
                    },
                    RdfXmlParseType::Other => RdfXmlState::ParseTypeLiteralPropertyElt {
                        iri,
                        base_iri,
                        language,
                        subject,
                        writer: Writer::new(Vec::default()),
                        id_attr,
                        emit: false,
                    },
                }
            }
        };
        self.state.push(new_state);
        Ok(())
    }

    fn parse_end_event<E: From<RdfXmlError>>(
        &mut self,
        event: BytesEnd<'_>,
        on_triple: &mut impl FnMut(Triple<'_>) -> Result<(), E>,
    ) -> Result<(), E> {
        //Literal case
        if self.in_literal_depth > 0 {
            if let Some(RdfXmlState::ParseTypeLiteralPropertyElt { writer, .. }) =
                self.state.last_mut()
            {
                writer
                    .write_event(Event::End(BytesEnd::borrowed(event.name())))
                    .map_err(RdfXmlError::from)?;
                self.in_literal_depth -= 1;
                return Ok(());
            }
        }

        if let Some(current_state) = self.state.pop() {
            self.end_state(current_state, on_triple)?;
        }
        Ok(())
    }

    fn parse_text_event(&mut self, event: BytesText<'_>) -> Result<(), RdfXmlError> {
        match self.state.last_mut() {
            Some(RdfXmlState::PropertyElt { object, .. }) => {
                if !event.iter().all(is_whitespace) {
                    *object = Some(NodeOrText::Text(
                        event.unescape_and_decode_with_custom_entities(
                            &self.reader,
                            &self.custom_entities,
                        )?,
                    ));
                }
                Ok(())
            }
            Some(RdfXmlState::ParseTypeLiteralPropertyElt { writer, .. }) => {
                writer.write_event(Event::Text(event))?;
                Ok(())
            }
            _ => {
                if event.iter().all(is_whitespace) {
                    Ok(())
                } else {
                    Err(RdfXmlError::msg(format!(
                        "Unexpected text event: {}",
                        event.unescape_and_decode_with_custom_entities(
                            &self.reader,
                            &self.custom_entities
                        )?
                    )))
                }
            }
        }
    }

    fn resolve_tag_name(&self, qname: &[u8]) -> Result<String, RdfXmlError> {
        let (namespace, local_name) = self.reader.event_namespace(qname, &self.namespace_buffer);
        self.resolve_ns_name(namespace, local_name)
    }

    fn resolve_attribute_name(&self, qname: &[u8]) -> Result<String, RdfXmlError> {
        let (namespace, local_name) = self
            .reader
            .attribute_namespace(qname, &self.namespace_buffer);
        self.resolve_ns_name(namespace, local_name)
    }

    fn resolve_ns_name(
        &self,
        namespace: Option<&[u8]>,
        local_name: &[u8],
    ) -> Result<String, RdfXmlError> {
        Ok(match namespace {
            Some(namespace) => {
                let namespace = unescape_with(namespace, &self.custom_entities)
                    .map_err(quick_xml::Error::EscapeError)?;
                let namespace = self.reader.decode(&namespace)?;
                namespace.to_owned() + self.reader.decode(local_name)?
            }
            None => self.reader.decode(local_name)?.to_owned(),
        })
    }

    #[allow(clippy::too_many_arguments)]
    fn build_node_elt<E: From<RdfXmlError>>(
        &mut self,
        iri: OwnedNamedNode,
        base_iri: Option<Iri<String>>,
        language: Option<LanguageTag<String>>,
        id_attr: Option<OwnedNamedNode>,
        node_id_attr: Option<OwnedBlankNode>,
        about_attr: Option<OwnedNamedNode>,
        type_attr: Option<OwnedNamedNode>,
        property_attrs: Vec<(OwnedNamedNode, String)>,
        on_triple: &mut impl FnMut(Triple<'_>) -> Result<(), E>,
    ) -> Result<RdfXmlState, E> {
        let subject_id = self.bnode_id_generator.generate(); //TODO: avoid to run it everytime
        let subject: Subject<'_> = match (&id_attr, &node_id_attr, &about_attr) {
            (Some(id_attr), None, None) => NamedNode::from(id_attr).into(),
            (None, Some(node_id_attr), None) => BlankNode::from(node_id_attr).into(),
            (None, None, Some(about_attr)) => NamedNode::from(about_attr).into(),
            (None, None, None) => BlankNode {
                id: subject_id.as_ref(),
            }
            .into(),
            (Some(_), Some(_), _) => {
                return Err(RdfXmlError::msg(
                    "Not both rdf:ID and rdf:nodeID could be set at the same time",
                )
                .into())
            }
            (_, Some(_), Some(_)) => {
                return Err(RdfXmlError::msg(
                    "Not both rdf:nodeID and rdf:resource could be set at the same time",
                )
                .into())
            }
            (Some(_), _, Some(_)) => {
                return Err(RdfXmlError::msg(
                    "Not both rdf:ID and rdf:resource could be set at the same time",
                )
                .into())
            }
        };

        self.emit_property_attrs(subject, property_attrs, &language, on_triple)?;

        if let Some(type_attr) = type_attr {
            on_triple(Triple {
                subject,
                predicate: NamedNode { iri: RDF_TYPE },
                object: NamedNode::from(&type_attr).into(),
            })?;
        }

        if *iri.iri != *RDF_DESCRIPTION {
            on_triple(Triple {
                subject,
                predicate: NamedNode { iri: RDF_TYPE },
                object: NamedNode::from(&iri).into(),
            })?;
        }
        Ok(RdfXmlState::NodeElt {
            base_iri,
            language,
            subject: subject.try_into()?,
            li_counter: 0,
        })
    }

    fn build_parse_type_resource_property_elt<E: From<RdfXmlError>>(
        &mut self,
        iri: OwnedNamedNode,
        base_iri: Option<Iri<String>>,
        language: Option<LanguageTag<String>>,
        subject: OwnedSubject,
        id_attr: Option<OwnedNamedNode>,
        on_triple: &mut impl FnMut(Triple<'_>) -> Result<(), E>,
    ) -> Result<RdfXmlState, E> {
        let object_id = self.bnode_id_generator.generate();
        let object = BlankNode {
            id: object_id.as_ref(),
        };
        let triple = Triple {
            subject: (&subject).into(),
            predicate: (&iri).into(),
            object: object.into(),
        };
        if let Some(id_attr) = &id_attr {
            self.reify(&triple, NamedNode::from(id_attr).into(), on_triple)?;
        }
        on_triple(triple)?;
        Ok(RdfXmlState::NodeElt {
            base_iri,
            language,
            subject: OwnedBlankNode::from(object).into(),
            li_counter: 0,
        })
    }

    fn end_state<E: From<RdfXmlError>>(
        &mut self,
        state: RdfXmlState,
        on_triple: &mut impl FnMut(Triple<'_>) -> Result<(), E>,
    ) -> Result<(), E> {
        match state {
            RdfXmlState::PropertyElt {
                iri,
                language,
                subject,
                id_attr,
                datatype_attr,
                object,
                ..
            } => {
                let object: Term<'_> = match &object {
                    Some(NodeOrText::Node(node)) => Subject::from(node).into(),
                    Some(NodeOrText::Text(text)) => {
                        self.new_literal(text, &language, &datatype_attr).into()
                    }
                    None => self.new_literal("", &language, &datatype_attr).into(),
                };
                let triple = Triple {
                    subject: (&subject).into(),
                    predicate: NamedNode { iri: &iri },
                    object,
                };
                if let Some(id_attr) = &id_attr {
                    self.reify(&triple, NamedNode::from(id_attr).into(), on_triple)?;
                }
                on_triple(triple)?;
            }
            RdfXmlState::ParseTypeCollectionPropertyElt {
                iri,
                subject,
                id_attr,
                objects,
                ..
            } => {
                let mut current_node: OwnedSubject = OwnedNamedNode {
                    iri: RDF_NIL.to_owned(),
                }
                .into();
                for object in objects.iter().rev() {
                    let subject: OwnedSubject = OwnedBlankNode {
                        id: self.bnode_id_generator.generate().as_ref().to_owned(),
                    }
                    .into();
                    on_triple(Triple {
                        subject: (&subject).into(),
                        predicate: NamedNode { iri: RDF_FIRST },
                        object: Subject::from(object).into(),
                    })?;
                    on_triple(Triple {
                        subject: (&subject).into(),
                        predicate: NamedNode { iri: RDF_REST },
                        object: Subject::from(&current_node).into(),
                    })?;
                    current_node = subject;
                }
                let triple = Triple {
                    subject: (&subject).into(),
                    predicate: NamedNode { iri: &iri },
                    object: Subject::from(&current_node).into(),
                };
                if let Some(id_attr) = &id_attr {
                    self.reify(&triple, NamedNode::from(id_attr).into(), on_triple)?;
                }
                on_triple(triple)?;
            }
            RdfXmlState::ParseTypeLiteralPropertyElt {
                iri,
                subject,
                id_attr,
                writer,
                emit,
                ..
            } => {
                if emit {
                    let object = writer.into_inner();
                    if object.is_empty() {
                        return Err(RdfXmlError::msg(format!(
                            "No value found for rdf:XMLLiteral value of property {}",
                            iri
                        ))
                        .into());
                    }
                    let triple = Triple {
                        subject: (&subject).into(),
                        predicate: NamedNode { iri: &iri },
                        object: Literal::Typed {
                            value: str::from_utf8(&object).map_err(|_| {
                                RdfXmlError::msg("The XML literal is not in valid UTF-8".to_owned())
                            })?,
                            datatype: NamedNode {
                                iri: RDF_XML_LITERAL,
                            },
                        }
                        .into(),
                    };
                    if let Some(id_attr) = &id_attr {
                        self.reify(&triple, NamedNode::from(id_attr).into(), on_triple)?;
                    }
                    on_triple(triple)?;
                }
            }
            RdfXmlState::NodeElt { subject, .. } => match self.state.last_mut() {
                Some(RdfXmlState::PropertyElt { object, .. }) => {
                    *object = Some(NodeOrText::Node(subject))
                }
                Some(RdfXmlState::ParseTypeCollectionPropertyElt { objects, .. }) => {
                    objects.push(subject)
                }
                _ => (),
            },
            _ => (),
        }
        Ok(())
    }

    fn new_literal<'a>(
        &self,
        value: &'a str,
        language: &'a Option<LanguageTag<String>>,
        datatype: &'a Option<OwnedNamedNode>,
    ) -> Literal<'a> {
        if let Some(datatype) = datatype {
            Literal::Typed {
                value,
                datatype: datatype.into(),
            }
        } else if let Some(language) = language {
            Literal::LanguageTaggedString {
                value,
                language: language.as_str(),
            }
        } else {
            Literal::Simple { value }
        }
    }

    fn reify<E: From<RdfXmlError>>(
        &self,
        triple: &Triple<'_>,
        statement_id: Subject<'_>,
        on_triple: &mut impl FnMut(Triple<'_>) -> Result<(), E>,
    ) -> Result<(), E> {
        on_triple(Triple {
            subject: statement_id,
            predicate: NamedNode { iri: RDF_TYPE },
            object: NamedNode { iri: RDF_STATEMENT }.into(),
        })?;
        on_triple(Triple {
            subject: statement_id,
            predicate: NamedNode { iri: RDF_SUBJECT },
            object: triple.subject.into(),
        })?;
        on_triple(Triple {
            subject: statement_id,
            predicate: NamedNode { iri: RDF_PREDICATE },
            object: triple.predicate.into(),
        })?;
        on_triple(Triple {
            subject: statement_id,
            predicate: NamedNode { iri: RDF_OBJECT },
            object: triple.object,
        })?;
        Ok(())
    }

    fn emit_property_attrs<E: From<RdfXmlError>>(
        &self,
        subject: Subject<'_>,
        literal_attributes: Vec<(OwnedNamedNode, String)>,
        language: &Option<LanguageTag<String>>,
        on_triple: &mut impl FnMut(Triple<'_>) -> Result<(), E>,
    ) -> Result<(), E> {
        for (literal_predicate, literal_value) in literal_attributes {
            on_triple(Triple {
                subject,
                predicate: (&literal_predicate).into(),
                object: if let Some(language) = language {
                    Literal::LanguageTaggedString {
                        value: &literal_value,
                        language: language.as_str(),
                    }
                } else {
                    Literal::Simple {
                        value: &literal_value,
                    }
                }
                .into(),
            })?;
        }
        Ok(())
    }

    fn convert_attribute(&self, attribute: Attribute<'_>) -> Result<String, RdfXmlError> {
        Ok(attribute
            .unescape_and_decode_value_with_custom_entities(&self.reader, &self.custom_entities)?)
    }

    fn convert_iri_attribute(
        &self,
        base_iri: &Option<Iri<String>>,
        attribute: Attribute<'_>,
    ) -> Result<OwnedNamedNode, RdfXmlError> {
        let value = attribute.unescaped_value_with_custom_entities(&self.custom_entities)?;
        let value = self.reader.decode(&value)?;
        Ok(OwnedNamedNode {
            iri: resolve(base_iri, value)?,
        })
    }
}

fn resolve(
    base_iri: &Option<Iri<String>>,
    relative_iri: impl AsRef<str> + Into<String> + Clone,
) -> Result<String, RdfXmlError> {
    Ok(if let Some(base_iri) = base_iri {
        base_iri
            .resolve(relative_iri.as_ref())
            .map_err(|error| RdfXmlError {
                kind: RdfXmlErrorKind::InvalidIri {
                    iri: relative_iri.into(),
                    error,
                },
            })
    } else {
        Iri::parse(relative_iri.clone().into()).map_err(|error| RdfXmlError {
            kind: RdfXmlErrorKind::InvalidIri {
                iri: relative_iri.into(),
                error,
            },
        })
    }?
    .into_inner())
}

fn is_nc_name(name: &str) -> bool {
    // Name - (Char* ':' Char*)
    is_name(name) && name.chars().all(|c| c != ':')
}

fn is_name(name: &str) -> bool {
    // NameStartChar (NameChar)*
    let mut c = name.chars();
    match c.next() {
        Some(c) if is_name_start_char(c) => (),
        _ => return false,
    };
    c.all(is_name_char)
}

#[derive(Default)]
pub struct BlankNodeIdGenerator {
    //TODO: avoid collisions
    counter: u64,
}

impl BlankNodeIdGenerator {
    pub fn generate(&mut self) -> BlankNodeId {
        let mut id: [u8; 12] = [
            b'r', b'i', b'o', b'g', b'0', b'0', b'0', b'0', b'0', b'0', b'0', b'0',
        ];
        self.counter += 1;
        write_u64_to_slice(self.counter, &mut id[4..]);
        BlankNodeId { id }
    }
}

fn write_u64_to_slice(mut v: u64, s: &mut [u8]) {
    for i in (0..s.len()).rev() {
        s[i] = b'0' + (v % 10) as u8;
        v /= 10;
    }
}

pub struct BlankNodeId {
    id: [u8; 12],
}

impl AsRef<str> for BlankNodeId {
    fn as_ref(&self) -> &str {
        // We know what id is and it's always valid UTF8
        str::from_utf8(&self.id).unwrap()
    }
}

fn split_once(input: &[u8], pred: impl FnMut(&u8) -> bool) -> Option<(&[u8], &[u8])> {
    let mut iter = input.splitn(2, pred);
    let front = iter.next()?;
    let tail = iter.next()?;
    Some((front, tail))
}

fn trim_start(input: &[u8]) -> &[u8] {
    for (i, c) in input.iter().enumerate() {
        if !is_whitespace(c) {
            return &input[i..];
        }
    }
    b"".as_ref()
}

fn is_whitespace(c: &u8) -> bool {
    matches!(c, b' ' | b'\t' | b'\n' | b'\r')
}
