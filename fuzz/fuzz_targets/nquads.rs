#![no_main]
use libfuzzer_sys::fuzz_target;
use rio_api::parser::QuadsParser;
use rio_turtle::{NQuadsParser, TurtleError};

fuzz_target!(|data: &[u8]| {
    NQuadsParser::new(data).parse_all(&mut |_| Ok(()) as Result<(), TurtleError>);
});
