type IriParserState = Result<usize, usize>; // usize = the end position

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

fn parse_iri(value: &[u8]) -> IriParserState {
    // IRI = scheme ":" ihier-part [ "?" iquery ] [ "#" ifragment ]
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
    } else if value.len() >= 1 && value[0] == b'/' {
        parse_ipath_absolute(value)
    } else if value.len() >= 1 {
        parse_ipath_rootless(value)
    } else {
        Ok(0) // ipath_empty
    }
}

fn parse_scheme(value: &[u8]) -> IriParserState {
    //  scheme = ALPHA *( ALPHA / DIGIT / "+" / "-" / "." )
    if value.is_empty() || !is_alpha(value[0]) {
        return Err(0);
    }
    for i in 0..value.len() {
        match value[i] {
            c if is_alpha(c) || is_digit(c) || c == b'+' || c == b'-' || c == b'.' => (),
            _ => return Ok(i),
        }
    }
    Err(value.len())
}

fn parse_iauthority(value: &[u8]) -> IriParserState {
    // iauthority = [ iuserinfo "@" ] ihost [ ":" port ]
    //TODO: implement properly
    for i in 0..value.len() {
        match value[i] {
            b'/' => return Ok(i),
            _ => (),
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
    // ipath-absolute = "/" [ isegment-nz *( "/" isegment ) ]
    if value.is_empty() || value[0] != b'/' {
        return Err(0);
    }

    let i = parse_isegment_nz(&value[1..])?;
    Ok(parse_ipath_abempty(&value[i..])? + i + 1)
}

fn parse_ipath_rootless(value: &[u8]) -> IriParserState {
    // ipath-rootless = isegment-nz *( "/" isegment )
    let i = parse_isegment_nz(value)?;
    Ok(parse_ipath_abempty(&value[i..])? + i)
}

fn parse_isegment(value: &[u8]) -> IriParserState {
    // isegment = *ipchar
    //TODO: implement properly
    for i in 0..value.len() {
        match value[i] {
            b'/' | b'?' | b'#' => return Ok(i),
            _ => (),
        }
    }
    return Ok(value.len());
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

fn parse_iquery(value: &[u8]) -> IriParserState {
    // iquery = *( ipchar / iprivate / "/" / "?" )
    //TODO: implement properly
    for i in 0..value.len() {
        match value[i] {
            b'#' => return Ok(i),
            _ => (),
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
    let examples = [
        "file://foo",
        "ftp://ftp.is.co.za/rfc/rfc1808.txt",
        "http://www.ietf.org/rfc/rfc2396.txt",
        "ldap://[2001:db8::7]/c=GB?objectClass?one",
        "mailto:John.Doe@example.com",
        "news:comp.infosystems.www.servers.unix",
        "tel:+1-816-555-1212",
        "telnet://192.0.2.16:80/",
        "urn:oasis:names:specification:docbook:dtd:xml:4.1.2",
    ];

    for e in &examples {
        assert!(
            validate_iri(e.as_bytes()).is_ok(),
            "{} is wrongly not recognized as an IRI",
            e
        )
    }
}
