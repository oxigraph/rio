#![no_main]
use libfuzzer_sys::fuzz_target;
use rio_api::parser::TriplesParser;
use rio_turtle::{NTriplesParser, TurtleError};

fuzz_target!(|data: &[u8]| {
    NTriplesParser::new(data).parse_all(&mut |_| Ok(()) as Result<(), TurtleError>);
});
