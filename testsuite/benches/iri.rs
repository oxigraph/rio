use criterion::{criterion_group, criterion_main, Criterion};
use rio_api::iri::Iri;

fn bench_iri_parse(c: &mut Criterion) {
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
        "http://example.com",
        "http://example.com/",
        "http://example.com/foo",
        "http://example.com/foo/bar",
        "http://example.com/foo/bar/",
        "http://example.com/foo/bar?q=1&r=2",
        "http://example.com/foo/bar/?q=1&r=2",
        "http://example.com#toto",
        "http://example.com/#toto",
        "http://example.com/foo#toto",
        "http://example.com/foo/bar#toto",
        "http://example.com/foo/bar/#toto",
        "http://example.com/foo/bar?q=1&r=2#toto",
        "http://example.com/foo/bar/?q=1&r=2#toto",
        "http://example.com/foo/bar/.././baz",
    ];

    c.bench_function("w3c parse tests", |b| {
        b.iter(|| {
            for iri in examples.iter() {
                Iri::parse(*iri).unwrap();
            }
        })
    });
}

fn bench_iri_resolve(c: &mut Criterion) {
    let examples = [
        "g:h",
        "g",
        "g/",
        "/g",
        "//g",
        "?y",
        "g?y",
        "#s",
        "g#s",
        "g?y#s",
        ";x",
        "g;x",
        "g;x?y#s",
        "",
        ".",
        "./",
        "./g",
        "..",
        "../",
        "../g",
        "../..",
        "../../",
        "../../g",
        "../../../g",
        "../../../../g",
        "/./g",
        "/../g",
        "g.",
        ".g",
        "g..",
        "..g",
        "./../g",
        "./g/.",
        "g/./h",
        "g/../h",
        "g;x=1/./y",
        "g;x=1/../y",
        "g?y/./x",
        "g?y/../x",
        "g#s/./x",
        "g#s/../x",
        "http:g",
        "./g:h",
    ];

    let base = Iri::parse("http://a/b/c/d;p?q").unwrap();

    c.bench_function("w3c resolve tests", |b| {
        b.iter(|| {
            for relative in examples.iter() {
                base.resolve(relative).unwrap();
            }
        })
    });
}

criterion_group!(iri, bench_iri_parse, bench_iri_resolve);

criterion_main!(iri);
