#![no_main]
use libfuzzer_sys::fuzz_target;
use rio_api::parser::TriplesParser;
use rio_xml::{RdfXmlError, RdfXmlParser};

fuzz_target!(|data: &[u8]| {
    RdfXmlParser::new(data, None).parse_all(&mut |_| Ok(()) as Result<(), RdfXmlError>);
});
