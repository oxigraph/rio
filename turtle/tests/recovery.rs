use rio_api::parser::*;
use rio_turtle::*;
use std::io::Cursor;

#[test]
fn ntriples_error_recovery() {
    let data = "<http://foo.com> <http://bar.com> <http://baz.com> .\n<http://foo.com> <http://bar.com> < .\n<http://foo.com> <http://bar.com> <http://bat.com> .\n<http://foo.com> <http://bar.com> <bat> .\n<http://foo.com> <http://bar.com> <http://bat.com> .";

    let mut count = 0;
    let mut count_err = 0;
    let mut parser = NTriplesParser::new(Cursor::new(&data));
    while !parser.is_end() {
        let step = parser.parse_step(&mut |_| {
            count += 1;
            Ok(()) as Result<(), TurtleError>
        });
        if step.is_err() {
            count_err += 1;
        }
    }

    assert_eq!(count, 3);
    assert_eq!(count_err, 2);
}

#[test]
fn nquads_error_recovery() {
    let data = "<http://foo.com> <http://bar.com> <http://baz.com> <http://graph.com> .\n<http://foo.com> <http://bar.com> < <http://graph.com>.\n<http://foo.com> <http://bar.com> <http://bat.com>  <http://graph.com>.\n<http://foo.com> <http://bar.com> <bat>  <http://graph.com>.\n<http://foo.com> <http://bar.com> <http://bat.com> .";

    let mut count = 0;
    let mut count_err = 0;
    let mut parser = NQuadsParser::new(Cursor::new(&data));
    while !parser.is_end() {
        let step = parser.parse_step(&mut |_| {
            count += 1;
            Ok(()) as Result<(), TurtleError>
        });
        if step.is_err() {
            count_err += 1;
        }
    }

    assert_eq!(count, 3);
    assert_eq!(count_err, 2);
}

#[test]
fn very_big_literal() {
    let mut data = String::with_capacity(12_000_000);
    data.push_str("<http://example.com/s> <http://example.com/p> \"");
    for _ in 0..11_000_000 {
        data.push('0');
    }
    data.push_str("\" .");
    let mut parser = NTriplesParser::new(Cursor::new(&data));
    assert!(parser
        .parse_step(&mut |_| Ok(()) as Result<(), TurtleError>)
        .is_ok());
    assert!(parser
        .parse_step(&mut |_| Ok(()) as Result<(), TurtleError>)
        .unwrap_err()
        .to_string()
        .contains("out of memory"));
}
