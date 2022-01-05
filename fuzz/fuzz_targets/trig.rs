#![no_main]
use libfuzzer_sys::fuzz_target;
use rio_api::parser::QuadsParser;
use rio_turtle::{TriGParser, TurtleError};

fuzz_target!(|data: &[u8]| {
    TriGParser::new(data, None).parse_all(&mut |_| Ok(()) as Result<(), TurtleError>);
});
