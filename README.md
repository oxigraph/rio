Rio
===

[![Build Status](https://travis-ci.org/Tpt/rio.svg?branch=master)](https://travis-ci.org/Tpt/rio)
[![dependency status](https://deps.rs/repo/github/Tpt/rudf/status.svg)](https://deps.rs/repo/github/Tpt/rio)

Rio is a low level library which provides conformant and fast parsers for RDF related file formats.

It currently provides [N-Triples](https://docs.rs/rio_turtle/latest/rio_turtle/struct.NTriplesParser.html), [N-Quads](https://docs.rs/rio_turtle/latest/rio_turtle/struct.NQuadsParser.html), [Turtle](https://docs.rs/rio_turtle/latest/rio_turtle/struct.TurtleParser.html), [TriG](https://docs.rs/rio_turtle/latest/rio_turtle/struct.TrigParser.html) and [RDF XML](https://docs.rs/rio_xml/latest/rio_xml/struct.RdfXmlParser.html) parsers.

It is split into multiple crates:
* `rio_api` provides common traits and data structures to be used in Rio parsers (`Triple`, `TripleParser`...).
  [![Latest Version](https://img.shields.io/crates/v/rio_api.svg)](https://crates.io/crates/rio_api) 
  [![Latest Documentation](https://img.shields.io/static/v1?label=documentation&message=latest&color=informational)](https://docs.rs/rio_api/)
* `rio_turtle` provides conformant streaming parsers for [Turtle](https://www.w3.org/TR/turtle/), [TriG](https://www.w3.org/TR/trig/), [N-Triples](https://www.w3.org/TR/n-triples/) and [N-Quads](https://www.w3.org/TR/n-quads/).
  [![Latest Version](https://img.shields.io/crates/v/rio_turtle.svg)](https://crates.io/crates/rio_turtle)
  [![Latest Documentation](https://img.shields.io/static/v1?label=documentation&message=latest&color=informational)](https://docs.rs/rio_turtle/)
* `rio_xml` provides a conformant streaming parser for [RDF XML](https://www.w3.org/TR/rdf-syntax-grammar/).
  [![Latest Version](https://img.shields.io/crates/v/rio_xml.svg)](https://crates.io/crates/rio_xml)
  [![Latest Documentation](https://img.shields.io/static/v1?label=documentation&message=latest&color=informational)](https://docs.rs/rio_xml/)

There is also the `rio_testsuite` crate that is used for testing Rio parsers against the [W3C RDF tests](http://w3c.github.io/rdf-tests/) to ensure their conformance.
It provides both an executable for building implementation reports and integration test to quickly ensure that the parsers stay conformant.
It is not designed to be used outside of Rio.


## License

Copyright 2019 The Rio developers.

Licensed under the Apache License, Version 2.0 (the "License");
you may not use this file except in compliance with the License.
You may obtain a copy of the License at http://www.apache.org/licenses/LICENSE-2.0

Unless required by applicable law or agreed to in writing, software
distributed under the License is distributed on an "AS IS" BASIS,
WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
See the License for the specific language governing permissions and
limitations under the License.
