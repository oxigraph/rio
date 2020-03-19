# Changelog

## [0.4.1] - 2020-03-19
- Makes `Iri` allow resolving against base IRIs with not hierarchical path (like `file:foo`).
- Upgrades `quick-xml` dependency to 0.18.

## [0.4.0] - 2020-01-07
- Adds "generalized" RDF support and generalized Trig parser behind a "generalized" feature flag.
- Allows to recover NTriples and NQuads parser errors, the parser jumps to the next line if the current line parsing fail.
- Makes `Iri` parser do the full IRI validation.

## [0.3.1] - 2019-09-02

### Added
- `Iri::as_str` and `Display` implementation to `rio_api`.

## [0.3.0] - 2019-08-28

### Added
- `TriplesFormatter` and `QuadsFormatter` with implementations for NTriples, NQuads, Turtle, TriG and RDF XML.
- `Iri` to `rio_api` that allows to do partial IRI validation and resolution.
- `ParseError::textual_position` that allows to get the error position from a `TurtleError` or a `RdfXmlError`.

### Changed
- `TripleParser` have been renamed to `TriplesParser` for consistency.
- `QuadParser` have been renamed to `QuadsParser` for consistency.
- `TriplesParser::parse_step` and `TriplesParser::parse_all` `on_triple` callbacks should now return a `Result`.
  It allows library user to return more easily errors from their callback code.
  The same change have been applied to `QuadsParser`.
- Literals formatting only escape the characters required by the canonical NTriples syntax.

## [0.2.0] - 2019-08-11

### Added
- `Quad` struct and `QuadParser` trait to `rio_api`.
- N-Quads (`NQuadsParser`) and TriG (`TriGParser`) parsers to `rio_turtle`.
- `rdf_xml` crate with an RDF XML parser.

### Changed
- `\r` characters could also end comments in Turtle/TriG.
- Fixes IRI parsing when the IRI has an authority and a query and/or fragment but no path.
- Do not allow "[] ." lines in Turtle.
- Minor optimisations to the Turtle parser.

## [0.1.0] - 2019-07-28

### Added
- `rio_api` crate with `Triple` struct and `TripleParser` trait.
- `rio_turtle` crate with N-Triples (`TurtleParser`) and Turtle (`TurtleParser`).
