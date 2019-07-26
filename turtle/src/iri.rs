type IriParserState = Result<usize, usize>; // usize = the end position

//TODO: update offset when returning Err

pub fn validate_iri(value: &[u8]) -> Result<(), usize> {
    match parse_iri(value) {
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
    match parse_absolute_iri(value) {
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
    match parse_iri_reference(value) {
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

fn parse_iri(value: &[u8]) -> IriParserState {
    // IRI = scheme ":" ihier-part [ "?" iquery ] [ "#" ifragment ] = absolute-IRI [ "#" ifragment ]
    let query_end = parse_absolute_iri(value)?;

    let fragment_end = query_end
        + if query_end < value.len() && value[query_end] == b'#' {
            parse_ifragment(&value[query_end + 1..])? + 1
        } else {
            0
        };

    Ok(fragment_end)
}

fn parse_ihier_part(value: &[u8]) -> IriParserState {
    // ihier-part = "//" iauthority ipath-abempty / ipath-absolute / ipath-rootless / ipath-empty
    if value.len() >= 2 && value[0] == b'/' && value[1] == b'/' {
        let i = parse_iauthority(&value[2..])?;
        Ok(parse_ipath_abempty(&value[2 + i..])? + 2 + i)
    } else if !value.is_empty() && value[0] == b'/' {
        parse_ipath_absolute(value)
    } else {
        match parse_ipath_rootless(value) {
            Ok(i) => Ok(i),
            Err(0) => Ok(0), // ipath empty
            Err(i) => Err(i),
        }
    }
}

fn parse_iri_reference(value: &[u8]) -> IriParserState {
    // IRI-reference  = IRI / irelative-ref
    match parse_iri(value) {
        Ok(r) => Ok(r),
        Err(_) => parse_irelative_ref(value),
    }
}

fn parse_absolute_iri(value: &[u8]) -> IriParserState {
    // absolute-IRI = scheme ":" ihier-part [ "?" iquery ]
    let scheme_end = parse_scheme(value)?;
    if scheme_end >= value.len() || value[scheme_end] != b':' {
        Err(scheme_end)?
    }

    let path_end = scheme_end + 1 + parse_ihier_part(&value[scheme_end + 1..])?;

    let query_end = path_end
        + (if path_end < value.len() && value[path_end] == b'?' {
            parse_iquery(&value[path_end + 1..])? + 1
        } else {
            0
        });

    Ok(query_end)
}

fn parse_irelative_ref(value: &[u8]) -> IriParserState {
    // irelative-ref = irelative-part [ "?" iquery ] [ "#" ifragment ]
    let path_end = parse_irelative_path(&value)?;
    let query_end = path_end
        + (if path_end < value.len() && value[path_end] == b'?' {
            parse_iquery(&value[path_end + 1..])? + 1
        } else {
            0
        });
    let fragment_end = query_end
        + if query_end < value.len() && value[query_end] == b'#' {
            parse_ifragment(&value[query_end + 1..])? + 1
        } else {
            0
        };

    Ok(fragment_end)
}

fn parse_irelative_path(value: &[u8]) -> IriParserState {
    // irelative-part = "//" iauthority ipath-abempty / ipath-absolute / ipath-noscheme / ipath-empty
    if value.len() >= 2 && value[0] == b'/' && value[1] == b'/' {
        let i = parse_iauthority(&value[2..])?;
        Ok(parse_ipath_abempty(&value[2 + i..])? + 2 + i)
    } else if !value.is_empty() && value[0] == b'/' {
        parse_ipath_absolute(value)
    } else {
        match parse_ipath_noscheme(value) {
            Ok(i) => Ok(i),
            Err(0) => Ok(0), // ipath empty
            Err(i) => Err(i),
        }
    }
}

fn parse_scheme(value: &[u8]) -> IriParserState {
    //  scheme = ALPHA *( ALPHA / DIGIT / "+" / "-" / "." )
    if value.is_empty() || !is_alpha(value[0]) {
        return Err(0);
    }
    for (i, c) in value.iter().enumerate() {
        match *c {
            c if is_alpha(c) || is_digit(c) || c == b'+' || c == b'-' || c == b'.' => (),
            _ => return Ok(i),
        }
    }
    Err(value.len())
}

fn parse_iauthority(value: &[u8]) -> IriParserState {
    // iauthority = [ iuserinfo "@" ] ihost [ ":" port ]
    //TODO: implement properly
    for (i, c) in value.iter().enumerate() {
        if *c == b'/' {
            return Ok(i);
        }
    }
    Ok(value.len())
}

fn parse_ipath_abempty(value: &[u8]) -> IriParserState {
    // ipath-abempty  = *( "/" isegment )
    let mut i = 0;
    while i < value.len() {
        match value[i] {
            b'/' => {
                i += parse_isegment(&value[i + 1..])? + 1;
            }
            _ => return Ok(i),
        }
    }
    Ok(value.len())
}

fn parse_ipath_absolute(value: &[u8]) -> IriParserState {
    // ipath-absolute = "/" [ isegment-nz *( "/" isegment ) ] = "/" [ isegment-nz ipath-abempty ]
    if value.is_empty() || value[0] != b'/' {
        return Err(0);
    }

    let i = match parse_isegment_nz(&value[1..]) {
        Ok(i) => i,
        Err(0) => return Ok(1), // optional
        Err(i) => return Err(i + 1),
    };
    Ok(parse_ipath_abempty(&value[i + 1..])? + i + 1)
}

fn parse_ipath_noscheme(value: &[u8]) -> IriParserState {
    // ipath-noscheme = isegment-nz-nc *( "/" isegment ) =  isegment-nz-nc ipath-abempty
    let i = parse_isegment_nz_nc(value)?;
    Ok(parse_ipath_abempty(&value[i..])? + i)
}

fn parse_ipath_rootless(value: &[u8]) -> IriParserState {
    // ipath-rootless = isegment-nz *( "/" isegment ) = isegment-nz ipath-abempty
    let i = parse_isegment_nz(value)?;
    Ok(parse_ipath_abempty(&value[i..])? + i)
}

fn parse_isegment(value: &[u8]) -> IriParserState {
    // isegment = *ipchar
    //TODO: implement properly
    for (i, c) in value.iter().enumerate() {
        match *c {
            b'/' | b'?' | b'#' => return Ok(i),
            _ => (),
        }
    }
    Ok(value.len())
}

fn parse_isegment_nz(value: &[u8]) -> IriParserState {
    // isegment-nz    = 1*ipchar
    let i = parse_isegment(value)?;
    if i == 0 {
        Err(0)
    } else {
        Ok(i)
    }
}

fn parse_isegment_nz_nc(value: &[u8]) -> IriParserState {
    // isegment-nz-nc = 1*( iunreserved / pct-encoded / sub-delims / "@" )
    //TODO: implement properly
    for (i, c) in value.iter().enumerate() {
        match *c {
            b'/' | b'?' | b'#' | b':' => return if i == 0 { Err(0) } else { Ok(i) },
            _ => (),
        }
    }
    Ok(value.len())
}

fn parse_iquery(value: &[u8]) -> IriParserState {
    // iquery = *( ipchar / iprivate / "/" / "?" )
    //TODO: implement properly
    for (i, c) in value.iter().enumerate() {
        if *c == b'#' {
            return Ok(i);
        }
    }
    Ok(value.len())
}

fn parse_ifragment(value: &[u8]) -> IriParserState {
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
            "{} is wrongly not recognized as an IRI",
            e
        );
        assert!(
            validate_absolute_iri(e.as_bytes()).is_ok(),
            "{} is wrongly not recognized as an absolute IRI",
            e
        );
        assert!(
            validate_iri_reference(e.as_bytes()).is_ok(),
            "{} is wrongly not recognized as an IRI reference",
            e
        );
    }

    for e in &examples {
        assert!(
            validate_iri(e.as_bytes()).is_ok(),
            "{} is wrongly not recognized as an IRI",
            e
        );
        assert!(
            validate_iri_reference(e.as_bytes()).is_ok(),
            "{} is wrongly not recognized as an IRI reference",
            e
        );
    }

    for e in &examples_relative {
        assert!(
            validate_iri_reference(e.as_bytes()).is_ok(),
            "{} is wrongly not recognized as an IRI reference",
            e
        );
    }
}
