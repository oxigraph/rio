Rio
===

Rio is a library aiming at providing conformant and fast parsers for RDF related file formats.

It currently provides [N-Triples](https://www.w3.org/TR/n-triples/) and [Turtle](https://www.w3.org/TR/turtle/) parsers.

It is not done to be used directly, but to be embedded inside of RDF libraries written in Rust, or exposed to other programming languages.

It provides multiple crates:
* `rio_api` provides common traits and data structures to be used in Rio parsers (`Triple`, `TripleParser`...).
* `rio_turtle` provides a fully conformant streaming [N-Triples](https://www.w3.org/TR/n-triples/) parser and a work in progress [Turtle](https://www.w3.org/TR/turtle/) parser.

There is also the `rio_testsuite` crate that is used for testing Rio parsers against the [W3C RDF tests](http://w3c.github.io/rdf-tests/) to ensure their conformance.
It provides both an executable for building implementation reports and integration test to quickly ensure that the parsers stay conformant.
