Rio
===

Rio is a library aiming at providing conformant and fast parsers for RDF related file formats.

It currently provides [N-Triples](https://www.w3.org/TR/n-triples/) and [Turtle](https://www.w3.org/TR/turtle/) parsers.

It is design primarily to be embedded inside of RDF libraries written in Rust, or exposed to other programming languages.

It provides multiple crates:
* `rio_api` provides common traits and data structures to be used in Rio parsers (`Triple`, `TripleParser`...).
* `rio_turtle` provides conformant streaming parsers for [Turtle](https://www.w3.org/TR/turtle/) and [N-Triples](https://www.w3.org/TR/n-triples/) formats.

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
