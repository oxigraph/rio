use quick_xml::events::*;
use quick_xml::{Reader, Writer};
use rio_api::model::*;
use rio_api::parser::TriplesParser;
use std::io::BufRead;
use std::str;

use crate::error::RdfXmlError;
use crate::model::*;
use quick_xml::events::attributes::Attribute;
use rio_api::iri::Iri;
use std::collections::HashSet;

/// A [RDF XML](https://www.w3.org/TR/rdf-syntax-grammar/) streaming parser.
///
/// It implements the `TriplesParser` trait.
/// It reads the file in streaming. It does not keep data in memory except a stack for handling nested XML tags
/// and a set of all seen `rdf:ID`s to detect duplicate ids and fail according to the specification.
///
/// Its performances are not optimized yet and hopefully could be significantly enhanced by reducing the
/// number of allocations and copies done by the parser.
///
/// Count the number of of people using the `TriplesParser` API without proper error management:
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
/// RdfXmlParser::new(file.as_ref(), "").unwrap().parse_all(&mut |t| {
///     if t.predicate == rdf_type && t.object == schema_person.into() {
///         count += 1;
///     }
///     Ok(()) as Result<(), RdfXmlError>
/// }).unwrap();
/// assert_eq!(2, count)
/// ```
pub struct RdfXmlParser<R: BufRead> {
    reader: RdfXmlReader<R>,
    reader_buffer: Vec<u8>,
    is_end: bool,
}

impl<R: BufRead> RdfXmlParser<R> {
    /// Builds the parser from a `BufRead` implementation and a base IRI for relative IRI resolution.
    ///
    /// The base IRI might be empty to state there is no base IRI.
    pub fn new(reader: R, base_iri: &str) -> Result<Self, RdfXmlError> {
        let mut reader = Reader::from_reader(reader);
        reader.expand_empty_elements(true);
        reader.trim_text(true);
        Ok(Self {
            reader: RdfXmlReader {
                reader,
                state: vec![RdfXmlState::Doc {
                    base_iri: if base_iri.is_empty() {
                        None
                    } else {
                        Some(Iri::parse(base_iri.to_owned())?)
                    },
                }],
                namespace_buffer: Vec::default(),
                bnode_id_generator: BlankNodeIdGenerator::default(),
                in_literal_depth: 0,
                known_rdf_id: HashSet::default(),
            },
            reader_buffer: Vec::default(),
            is_end: false,
        })
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
    Node(OwnedNamedOrBlankNode),
    Text(String),
}

enum RdfXmlState {
    Doc {
        base_iri: Option<Iri<String>>,
    },
    RDF {
        base_iri: Option<Iri<String>>,
        language: Option<String>,
    },
    NodeElt {
        base_iri: Option<Iri<String>>,
        language: Option<String>,
        subject: OwnedNamedOrBlankNode,
        li_counter: usize,
    },
    PropertyElt {
        //Resource, Literal or Empty property element
        iri: String,
        base_iri: Option<Iri<String>>,
        language: Option<String>,
        subject: OwnedNamedOrBlankNode,
        object: Option<NodeOrText>,
        id_attr: Option<OwnedNamedNode>,
        datatype_attr: Option<OwnedNamedNode>,
    },
    ParseTypeCollectionPropertyElt {
        iri: String,
        base_iri: Option<Iri<String>>,
        language: Option<String>,
        subject: OwnedNamedOrBlankNode,
        objects: Vec<OwnedNamedOrBlankNode>,
        id_attr: Option<OwnedNamedNode>,
    },
    ParseTypeLiteralPropertyElt {
        iri: String,
        base_iri: Option<Iri<String>>,
        language: Option<String>,
        subject: OwnedNamedOrBlankNode,
        writer: Writer<Vec<u8>>,
        id_attr: Option<OwnedNamedNode>,
        emit: bool, //false for parseTypeOtherPropertyElt support
    },
}

impl RdfXmlState {
    fn base_iri(&self) -> &Option<Iri<String>> {
        match self {
            RdfXmlState::Doc { base_iri, .. } => base_iri,
            RdfXmlState::RDF { base_iri, .. } => base_iri,
            RdfXmlState::NodeElt { base_iri, .. } => base_iri,
            RdfXmlState::PropertyElt { base_iri, .. } => base_iri,
            RdfXmlState::ParseTypeCollectionPropertyElt { base_iri, .. } => base_iri,
            RdfXmlState::ParseTypeLiteralPropertyElt { base_iri, .. } => base_iri,
        }
    }

    fn language(&self) -> Option<&String> {
        match self {
            RdfXmlState::Doc { .. } => None,
            RdfXmlState::RDF { language, .. } => language.as_ref(),
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
    in_literal_depth: usize,
    known_rdf_id: HashSet<String>,
}

impl<R: BufRead> RdfXmlReader<R> {
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
            RDF,
            NodeElt,
            PropertyElt { subject: OwnedNamedOrBlankNode },
        }

        let iri = self.resolve_tag_name(event.name())?;

        //We read attributes
        let (mut language, mut base_iri) = if let Some(current_state) = self.state.last() {
            (
                current_state.language().cloned(),
                current_state.base_iri().clone(),
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
                    language = Some(
                        attribute
                            .unescape_and_decode_value(&self.reader)
                            .map_err(RdfXmlError::from)?,
                    );
                }
                b"xml:base" => {
                    base_iri = Some(
                        Iri::parse(
                            attribute
                                .unescape_and_decode_value(&self.reader)
                                .map_err(RdfXmlError::from)?,
                        )
                        .map_err(RdfXmlError::from)?,
                    )
                }
                key if !key.starts_with(b"xml") => {
                    let attribute_url = self.resolve_attribute_name(key)?;
                    if *attribute_url == *RDF_ID {
                        let mut id = attribute
                            .unescape_and_decode_value(&self.reader)
                            .map_err(RdfXmlError::from)?;
                        if !is_nc_name(&id) {
                            return Err(RdfXmlError::from(format!(
                                "{} is not a valid rdf:ID value",
                                &id
                            ))
                            .into());
                        }
                        id.insert(0, '#');
                        id_attr = Some(id);
                    } else if *attribute_url == *RDF_BAG_ID {
                        let bag_id = attribute
                            .unescape_and_decode_value(&self.reader)
                            .map_err(RdfXmlError::from)?;
                        if !is_nc_name(&bag_id) {
                            return Err(RdfXmlError::from(format!(
                                "{} is not a valid rdf:bagID value",
                                &bag_id
                            ))
                            .into());
                        }
                    } else if *attribute_url == *RDF_NODE_ID {
                        let id = attribute
                            .unescape_and_decode_value(&self.reader)
                            .map_err(RdfXmlError::from)?;
                        if !is_nc_name(&id) {
                            return Err(RdfXmlError::from(format!(
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
                        return Err(RdfXmlError::from(format!(
                            "{} is not a valid attribute",
                            &attribute_url
                        ))
                        .into());
                    } else {
                        property_attrs.push((
                            OwnedNamedNode { iri: attribute_url },
                            attribute
                                .unescape_and_decode_value(&self.reader)
                                .map_err(RdfXmlError::from)?,
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
                    return Err(RdfXmlError::from(format!(
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
            Some(attr) => Some(convert_iri_attribute(&base_iri, attr, &self.reader)?),
            None => None,
        };
        let resource_attr = match resource_attr {
            Some(attr) => Some(convert_iri_attribute(&base_iri, attr, &self.reader)?),
            None => None,
        };
        let datatype_attr = match datatype_attr {
            Some(attr) => Some(convert_iri_attribute(&base_iri, attr, &self.reader)?),
            None => None,
        };
        let type_attr = match type_attr {
            Some(attr) => Some(convert_iri_attribute(&base_iri, attr, &self.reader)?),
            None => None,
        };

        let expected_production = match self.state.last() {
    Some(RdfXmlState::Doc { .. }) => RdfXmlNextProduction::RDF,
    Some(RdfXmlState::RDF { .. }) => RdfXmlNextProduction::NodeElt,
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
        return Err(RdfXmlError::from("No state in the stack: the XML is not balanced").into());
    }
};

        let new_state = match expected_production {
            RdfXmlNextProduction::RDF => {
                if *iri == *RDF_RDF {
                    RdfXmlState::RDF { base_iri, language }
                } else if RESERVED_RDF_ELEMENTS.contains(&&*iri) {
                    return Err(RdfXmlError::from(format!(
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
                    return Err(RdfXmlError::from(format!(
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
                        return Err(RdfXmlError::from(format!(
                            "Invalid property element tag name: {}",
                            &iri
                        ))
                        .into());
                    }
                } else if RESERVED_RDF_ELEMENTS.contains(&&*iri) || *iri == *RDF_DESCRIPTION {
                    return Err(RdfXmlError::from(format!(
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
                            let object: OwnedNamedOrBlankNode = match (resource_attr, node_id_attr)
                    {
                        (Some(resource_attr), None) => resource_attr.into(),
                        (None, Some(node_id_attr)) => node_id_attr.into(),
                        (None, None) => OwnedBlankNode {
                            id: self.bnode_id_generator.generate().as_ref().to_owned(),
                        }.into(),
                        (Some(_), Some(_)) => return Err(RdfXmlError::from("Not both rdf:resource and rdf:nodeID could be set at the same time").into())
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
                *object = Some(NodeOrText::Text(event.unescape_and_decode(&self.reader)?));
                Ok(())
            }
            Some(RdfXmlState::ParseTypeLiteralPropertyElt { writer, .. }) => {
                writer.write_event(Event::Text(event))?;
                Ok(())
            }
            _ => Err(format!(
                "Unexpected text event: {}",
                event.unescape_and_decode(&self.reader)?
            )
            .into()),
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
                self.reader.decode(namespace)?.to_owned() + self.reader.decode(local_name)?
            }
            None => self.reader.decode(local_name)?.to_owned(),
        })
    }

    fn build_node_elt<E: From<RdfXmlError>>(
        &mut self,
        iri: OwnedNamedNode,
        base_iri: Option<Iri<String>>,
        language: Option<String>,
        id_attr: Option<OwnedNamedNode>,
        node_id_attr: Option<OwnedBlankNode>,
        about_attr: Option<OwnedNamedNode>,
        type_attr: Option<OwnedNamedNode>,
        property_attrs: Vec<(OwnedNamedNode, String)>,
        on_triple: &mut impl FnMut(Triple<'_>) -> Result<(), E>,
    ) -> Result<RdfXmlState, E> {
        let subject_id = self.bnode_id_generator.generate(); //TODO: avoid to run it everytime
        let subject: NamedOrBlankNode<'_> = match (&id_attr, &node_id_attr, &about_attr) {
            (Some(id_attr), None, None) => NamedNode::from(id_attr).into(),
            (None, Some(node_id_attr), None) => BlankNode::from(node_id_attr).into(),
            (None, None, Some(about_attr)) => NamedNode::from(about_attr).into(),
            (None, None, None) => BlankNode {
                id: subject_id.as_ref(),
            }
            .into(),
            (Some(_), Some(_), _) => {
                return Err(RdfXmlError::from(
                    "Not both rdf:ID and rdf:nodeID could be set at the same time",
                )
                .into())
            }
            (_, Some(_), Some(_)) => {
                return Err(RdfXmlError::from(
                    "Not both rdf:nodeID and rdf:resource could be set at the same time",
                )
                .into())
            }
            (Some(_), _, Some(_)) => {
                return Err(RdfXmlError::from(
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
            subject: subject.into(),
            li_counter: 0,
        })
    }

    fn build_parse_type_resource_property_elt<E: From<RdfXmlError>>(
        &mut self,
        iri: OwnedNamedNode,
        base_iri: Option<Iri<String>>,
        language: Option<String>,
        subject: OwnedNamedOrBlankNode,
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
                    Some(NodeOrText::Node(node)) => NamedOrBlankNode::from(node).into(),
                    Some(NodeOrText::Text(text)) => {
                        self.new_literal(text, &language, &datatype_attr).into()
                    }
                    None => self.new_literal(&"", &language, &datatype_attr).into(),
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
                let mut current_node: OwnedNamedOrBlankNode = OwnedNamedNode {
                    iri: RDF_NIL.to_owned(),
                }
                .into();
                for object in objects.iter().rev() {
                    let subject: OwnedNamedOrBlankNode = OwnedBlankNode {
                        id: self.bnode_id_generator.generate().as_ref().to_owned(),
                    }
                    .into();
                    on_triple(Triple {
                        subject: (&subject).into(),
                        predicate: NamedNode { iri: RDF_FIRST },
                        object: NamedOrBlankNode::from(object).into(),
                    })?;
                    on_triple(Triple {
                        subject: (&subject).into(),
                        predicate: NamedNode { iri: RDF_REST },
                        object: NamedOrBlankNode::from(&current_node).into(),
                    })?;
                    current_node = subject;
                }
                let triple = Triple {
                    subject: (&subject).into(),
                    predicate: NamedNode { iri: &iri },
                    object: NamedOrBlankNode::from(&current_node).into(),
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
                        return Err(RdfXmlError::from(format!(
                            "No value found for rdf:XMLLiteral value of property {}",
                            iri
                        ))
                        .into());
                    }
                    let triple = Triple {
                        subject: (&subject).into(),
                        predicate: NamedNode { iri: &iri },
                        object: Literal::Typed {
                            value: &str::from_utf8(&object).map_err(|_| {
                                RdfXmlError::from(
                                    "The XML literal is not in valid UTF-8".to_owned(),
                                )
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
        language: &'a Option<String>,
        datatype: &'a Option<OwnedNamedNode>,
    ) -> Literal<'a> {
        if let Some(datatype) = datatype {
            Literal::Typed {
                value,
                datatype: datatype.into(),
            }
        } else if let Some(language) = language {
            Literal::LanguageTaggedString { value, language }
        } else {
            Literal::Simple { value }
        }
    }

    fn reify<E: From<RdfXmlError>>(
        &self,
        triple: &Triple<'_>,
        statement_id: NamedOrBlankNode<'_>,
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
        subject: NamedOrBlankNode<'_>,
        literal_attributes: Vec<(OwnedNamedNode, String)>,
        language: &Option<String>,
        on_triple: &mut impl FnMut(Triple<'_>) -> Result<(), E>,
    ) -> Result<(), E> {
        for (literal_predicate, literal_value) in literal_attributes {
            on_triple(Triple {
                subject,
                predicate: (&literal_predicate).into(),
                object: if let Some(language) = language {
                    Literal::LanguageTaggedString {
                        value: &literal_value,
                        language: &language,
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
}

fn convert_iri_attribute<B: BufRead>(
    base_iri: &Option<Iri<String>>,
    attribute: Attribute<'_>,
    reader: &Reader<B>,
) -> Result<OwnedNamedNode, RdfXmlError> {
    let value = attribute.unescaped_value()?;
    let value = reader.decode(&value)?;
    Ok(OwnedNamedNode {
        iri: if let Some(base_iri) = base_iri {
            base_iri.resolve(&value)
        } else {
            Iri::parse(value.to_string())
        }?
        .into_inner(),
    })
}

fn resolve(base_iri: &Option<Iri<String>>, relative_iri: String) -> Result<String, RdfXmlError> {
    Ok(if let Some(base_iri) = base_iri {
        base_iri.resolve(&relative_iri)
    } else {
        Iri::parse(relative_iri)
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

fn is_name_start_char(c: char) -> bool {
    // ":" | [A-Z] | "_" | [a-z] | [#xC0-#xD6] | [#xD8-#xF6] | [#xF8-#x2FF] | [#x370-#x37D] | [#x37F-#x1FFF] | [#x200C-#x200D] | [#x2070-#x218F] | [#x2C00-#x2FEF] | [#x3001-#xD7FF] | [#xF900-#xFDCF] | [#xFDF0-#xFFFD] | [#x10000-#xEFFFF]
    match c {
        ':'
        | 'A'..='Z'
        | '_'
        | 'a'..='z'
        | '\u{C0}'..='\u{D6}'
        | '\u{D8}'..='\u{F6}'
        | '\u{F8}'..='\u{2FF}'
        | '\u{370}'..='\u{37D}'
        | '\u{37F}'..='\u{1FFF}'
        | '\u{200C}'..='\u{200D}'
        | '\u{2070}'..='\u{218F}'
        | '\u{2C00}'..='\u{2FEF}'
        | '\u{3001}'..='\u{D7FF}'
        | '\u{F900}'..='\u{FDCF}'
        | '\u{FDF0}'..='\u{FFFD}'
        | '\u{10000}'..='\u{EFFFF}' => true,
        _ => false,
    }
}

fn is_name_char(c: char) -> bool {
    // NameStartChar | "-" | "." | [0-9] | #xB7 | [#x0300-#x036F] | [#x203F-#x2040]
    match c {
        c if is_name_start_char(c) => true,
        '-' | '.' | '0'..='9' | '\u{B7}' | '\u{0300}'..='\u{036F}' | '\u{203F}'..='\u{2040}' => {
            true
        }
        _ => false,
    }
}

#[derive(Default)]
pub struct BlankNodeIdGenerator {
    //TODO: avoid collisions
    counter: usize,
}

impl BlankNodeIdGenerator {
    pub fn generate(&mut self) -> BlankNodeId {
        let mut id: [u8; 12] = [
            b'r', b'i', b'o', b'g', b'0', b'0', b'0', b'0', b'0', b'0', b'0', b'0',
        ];
        self.counter += 1;
        write_usize_to_slice(self.counter, &mut id[4..]);
        BlankNodeId { id }
    }
}

fn write_usize_to_slice(mut v: usize, s: &mut [u8]) {
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
