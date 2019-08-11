# Changelog

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
