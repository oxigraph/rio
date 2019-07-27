type IriParserState = Result<usize, usize>; // usize = the end position

//TODO: update offset when returning Err

pub fn validate_iri(value: &[u8]) -> Result<(), usize> {
    match parse_iri(value, 0) {
        Ok(i) => {
            if i == value.len() {
                Ok(())
            } else {
                Err(i)
            }
        }
        Err(i) => Err(i),
    }
}

pub fn validate_absolute_iri(value: &[u8]) -> Result<(), usize> {
    match parse_absolute_iri(value, 0) {
        Ok(i) => {
            if i == value.len() {
                Ok(())
            } else {
                Err(i)
            }
        }
        Err(i) => Err(i),
    }
}

pub fn validate_iri_reference(value: &[u8]) -> Result<(), usize> {
    match parse_iri_reference(value, 0) {
        Ok(i) => {
            if i == value.len() {
                Ok(())
            } else {
                Err(i)
            }
        }
        Err(i) => Err(i),
    }
}

fn parse_iri(value: &[u8], start: usize) -> IriParserState {
    // IRI = scheme ":" ihier-part [ "?" iquery ] [ "#" ifragment ] = absolute-IRI [ "#" ifragment ]
    let query_end = parse_absolute_iri(value, start)?;

    let fragment_end = if query_end < value.len() && value[query_end] == b'#' {
        parse_ifragment(value, query_end + 1)?
    } else {
        query_end
    };

    Ok(fragment_end)
}

fn parse_ihier_part(value: &[u8], start: usize) -> IriParserState {
    // ihier-part = "//" iauthority ipath-abempty / ipath-absolute / ipath-rootless / ipath-empty
    if value.len() >= start + 2 && value[start] == b'/' && value[start + 1] == b'/' {
        let i = parse_iauthority(value, start + 2)?;
        parse_ipath_abempty(value, i)
    } else if value.len() > start && value[start] == b'/' {
        parse_ipath_absolute(value, start)
    } else {
        match parse_ipath_rootless(value, start) {
            Ok(i) => Ok(i),
            Err(i) => {
                if i == start {
                    Ok(i) // ipath empty
                } else {
                    Err(i)
                }
            }
        }
    }
}

fn parse_iri_reference(value: &[u8], start: usize) -> IriParserState {
    // IRI-reference  = IRI / irelative-ref
    match parse_iri(value, start) {
        Ok(r) => Ok(r),
        Err(_) => parse_irelative_ref(value, start),
    }
}

fn parse_absolute_iri(value: &[u8], start: usize) -> IriParserState {
    // absolute-IRI = scheme ":" ihier-part [ "?" iquery ]
    let scheme_end = parse_scheme(value, start)?;
    if scheme_end >= value.len() || value[scheme_end] != b':' {
        Err(scheme_end)?
    }

    let path_end = parse_ihier_part(value, scheme_end + 1)?;

    let query_end = if path_end < value.len() && value[path_end] == b'?' {
        parse_iquery(value, path_end + 1)?
    } else {
        path_end
    };

    Ok(query_end)
}

fn parse_irelative_ref(value: &[u8], start: usize) -> IriParserState {
    // irelative-ref = irelative-part [ "?" iquery ] [ "#" ifragment ]
    let path_end = parse_irelative_path(value, start)?;

    let query_end = if path_end < value.len() && value[path_end] == b'?' {
        parse_iquery(value, path_end + 1)?
    } else {
        path_end
    };
    let fragment_end = if query_end < value.len() && value[query_end] == b'#' {
        parse_ifragment(&value, query_end + 1)?
    } else {
        query_end
    };

    Ok(fragment_end)
}

fn parse_irelative_path(value: &[u8], start: usize) -> IriParserState {
    // irelative-part = "//" iauthority ipath-abempty / ipath-absolute / ipath-noscheme / ipath-empty
    if value.len() >= start + 2 && value[start] == b'/' && value[start + 1] == b'/' {
        let i = parse_iauthority(&value, start + 2)?;
        parse_ipath_abempty(value, i)
    } else if value.len() > start && value[start] == b'/' {
        parse_ipath_absolute(value, start)
    } else {
        match parse_ipath_noscheme(value, start) {
            Ok(i) => Ok(i),
            Err(i) => {
                if i == start {
                    Ok(i) // ipath empty
                } else {
                    Err(i)
                }
            }
        }
    }
}

fn parse_scheme(value: &[u8], start: usize) -> IriParserState {
    //  scheme = ALPHA *( ALPHA / DIGIT / "+" / "-" / "." )
    if value.len() <= start || !is_alpha(value[start]) {
        return Err(start);
    }
    for (i, c) in value[start..].iter().enumerate() {
        match *c {
            c if is_alpha(c) || is_digit(c) || c == b'+' || c == b'-' || c == b'.' => (),
            _ => return Ok(start + i),
        }
    }
    Err(value.len())
}

fn parse_iauthority(value: &[u8], start: usize) -> IriParserState {
    // iauthority = [ iuserinfo "@" ] ihost [ ":" port ]
    //TODO: implement properly
    for (i, c) in value[start..].iter().enumerate() {
        if *c == b'/' {
            return Ok(start + i);
        }
    }
    Ok(value.len())
}

fn parse_ipath_abempty(value: &[u8], start: usize) -> IriParserState {
    // ipath-abempty  = *( "/" isegment )
    let mut i = start;
    while i < value.len() {
        match value[i] {
            b'/' => {
                i = parse_isegment(value, i + 1)?;
            }
            _ => return Ok(i),
        }
    }
    Ok(value.len())
}

fn parse_ipath_absolute(value: &[u8], start: usize) -> IriParserState {
    // ipath-absolute = "/" [ isegment-nz *( "/" isegment ) ] = "/" [ isegment-nz ipath-abempty ]
    if value.len() <= start || value[start] != b'/' {
        return Err(start);
    }

    match parse_isegment_nz(value, start + 1) {
        Ok(i) => parse_ipath_abempty(value, i),
        Err(i) => {
            if i == start + 1 {
                Ok(i) // optional
            } else {
                Err(i)
            }
        }
    }
}

fn parse_ipath_noscheme(value: &[u8], start: usize) -> IriParserState {
    // ipath-noscheme = isegment-nz-nc *( "/" isegment ) =  isegment-nz-nc ipath-abempty
    let i = parse_isegment_nz_nc(value, start)?;
    parse_ipath_abempty(&value, i)
}

fn parse_ipath_rootless(value: &[u8], start: usize) -> IriParserState {
    // ipath-rootless = isegment-nz *( "/" isegment ) = isegment-nz ipath-abempty
    let i = parse_isegment_nz(value, start)?;
    parse_ipath_abempty(value, i)
}

fn parse_isegment(value: &[u8], start: usize) -> IriParserState {
    // isegment = *ipchar
    //TODO: implement properly
    for (i, c) in value[start..].iter().enumerate() {
        match *c {
            b'/' | b'?' | b'#' => return Ok(start + i),
            _ => (),
        }
    }
    Ok(value.len())
}

fn parse_isegment_nz(value: &[u8], start: usize) -> IriParserState {
    // isegment-nz    = 1*ipchar
    let i = parse_isegment(value, start)?;
    if i == start {
        Err(0)
    } else {
        Ok(i)
    }
}

fn parse_isegment_nz_nc(value: &[u8], start: usize) -> IriParserState {
    // isegment-nz-nc = 1*( iunreserved / pct-encoded / sub-delims / "@" )
    //TODO: implement properly
    for (i, c) in value[start..].iter().enumerate() {
        match *c {
            b'/' | b'?' | b'#' | b':' => return if i == start { Err(i) } else { Ok(i) },
            _ => (),
        }
    }
    Ok(value.len())
}

fn parse_iquery(value: &[u8], start: usize) -> IriParserState {
    // iquery = *( ipchar / iprivate / "/" / "?" )
    //TODO: implement properly
    for (i, c) in value[start..].iter().enumerate() {
        if *c == b'#' {
            return Ok(start + i);
        }
    }
    Ok(value.len())
}

fn parse_ifragment(value: &[u8], _start: usize) -> IriParserState {
    // ifragment = *( ipchar / "/" / "?" )
    //TODO: implement properly
    Ok(value.len())
}

fn is_alpha(b: u8) -> bool {
    match b {
        b'a'..=b'z' | b'A'..=b'Z' => true,
        _ => false,
    }
}

fn is_digit(b: u8) -> bool {
    match b {
        b'0'..=b'9' => true,
        _ => false,
    }
}

#[test]
fn test_parsing() {
    let examples_absolute = [
        "file://foo",
        "ftp://ftp.is.co.za/rfc/rfc1808.txt",
        "http://www.ietf.org/rfc/rfc2396.txt",
        "ldap://[2001:db8::7]/c=GB?objectClass?one",
        "mailto:John.Doe@example.com",
        "news:comp.infosystems.www.servers.unix",
        "tel:+1-816-555-1212",
        "telnet://192.0.2.16:80/",
        "urn:oasis:names:specification:docbook:dtd:xml:4.1.2",
        "http://example.com",
        "http://example.com/",
        "http://example.com/foo",
        "http://example.com/foo/bar",
        "http://example.com/foo/bar/",
        "http://example.com/foo/bar?q=1&r=2",
        "http://example.com/foo/bar/?q=1&r=2",
    ];

    let examples = [
        "http://example.com#toto",
        "http://example.com/#toto",
        "http://example.com/foo#toto",
        "http://example.com/foo/bar#toto",
        "http://example.com/foo/bar/#toto",
        "http://example.com/foo/bar?q=1&r=2#toto",
        "http://example.com/foo/bar/?q=1&r=2#toto",
    ];

    let examples_relative = [
        "//foo.com/bar",
        "/foo",
        "/../foo",
        "/./foo",
        "bar",
        "?p=1",
        "?p=1#boo",
        "#boo",
    ];

    for e in &examples_absolute {
        assert!(
            validate_iri(e.as_bytes()).is_ok(),
            "{} is not recognized as an IRI",
            e
        );
        assert!(
            validate_absolute_iri(e.as_bytes()).is_ok(),
            "{} is not recognized as an absolute IRI",
            e
        );
        assert!(
            validate_iri_reference(e.as_bytes()).is_ok(),
            "{} is not recognized as an IRI reference",
            e
        );
    }

    for e in &examples {
        assert!(
            validate_iri(e.as_bytes()).is_ok(),
            "{} is not recognized as an IRI",
            e
        );
        assert!(
            validate_iri_reference(e.as_bytes()).is_ok(),
            "{} is not recognized as an IRI reference",
            e
        );
    }

    for e in &examples_relative {
        assert!(
            validate_iri_reference(e.as_bytes()).is_ok(),
            "{} is not recognized as an IRI reference",
            e
        );
    }
}
