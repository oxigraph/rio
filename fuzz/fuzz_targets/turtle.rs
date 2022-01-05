#![no_main]
use libfuzzer_sys::fuzz_target;
use rio_api::parser::TriplesParser;
use rio_turtle::{TurtleError, TurtleParser};

fuzz_target!(|data: &[u8]| {
    TurtleParser::new(data, None).parse_all(&mut |_| Ok(()) as Result<(), TurtleError>);
});
