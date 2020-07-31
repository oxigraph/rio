//! Implementation of sophia TripleSource / QuadSource for rio parsers

use sophia_api::quad::stream::*;
use std::error::Error;

#[macro_export]
/// Implement Sophia's `TripleSource` for a Rio `TriplesParser`.
macro_rules! impl_triple_source {
    ($parser:ident) => {
        mod as_sophia_triple_source {
            use super::*;
            use rio_api::model::Term;
            use rio_api::parser::TriplesParser;
            use rio_api::sophia::RioStreamError;
            use sophia_api::triple::stream::*;
            use sophia_api::triple::streaming_mode::*;
            use std::error::Error;
            use std::io::BufRead;

            impl<B: BufRead> TripleSource for $parser<B> {
                type Error = <$parser<B> as TriplesParser>::Error;
                type Triple = ScopedRioSourceTriple;
                fn try_for_some_triple<F, EF>(
                    &mut self,
                    f: &mut F,
                ) -> StreamResult<bool, Self::Error, EF>
                where
                    F: FnMut(StreamedTriple<'_, Self::Triple>) -> Result<(), EF>,
                    EF: Error,
                {
                    if self.is_end() {
                        return Ok(false);
                    }
                    self.parse_step(&mut |t| -> Result<(), RioStreamError<Self::Error, EF>> {
                        f(StreamedTriple::scoped([
                            t.subject.into(),
                            t.predicate.into(),
                            t.object,
                        ]))
                        .map_err(|e| SinkError(e).into())
                    })
                    .map_err(|e| e.into())
                    .and(Ok(true))
                }
            }

            /// Convenient type alias.
            type RioSourceTriple<'a> = [Term<'a>; 3];
            sophia_api::make_scoped_triple_streaming_mode!(ScopedRioSourceTriple, RioSourceTriple);
        }
    };
}

#[macro_export]
/// Implement Sophia's `QuadSource` for a Rio `QuadsParser`.
macro_rules! impl_quad_source {
    ($parser:ident) => {
        mod as_sophia_quad_source {
            use super::*;
            use rio_api::model::Term;
            use rio_api::parser::QuadsParser;
            use rio_api::sophia::RioStreamError;
            use sophia_api::quad::stream::*;
            use sophia_api::quad::streaming_mode::*;
            use std::error::Error;
            use std::io::BufRead;

            impl<B: BufRead> QuadSource for $parser<B> {
                type Error = <$parser<B> as QuadsParser>::Error;
                type Quad = ScopedRioSourceQuad;
                fn try_for_some_quad<F, EF>(
                    &mut self,
                    f: &mut F,
                ) -> StreamResult<bool, Self::Error, EF>
                where
                    F: FnMut(StreamedQuad<'_, Self::Quad>) -> Result<(), EF>,
                    EF: Error,
                {
                    if self.is_end() {
                        return Ok(false);
                    }
                    self.parse_step(&mut |q| -> Result<(), RioStreamError<Self::Error, EF>> {
                        f(StreamedQuad::scoped((
                            [q.subject.into(), q.predicate.into(), q.object],
                            q.graph_name.map(|g| g.into()),
                        )))
                        .map_err(|e| SinkError(e).into())
                    })
                    .map_err(|e| e.into())
                    .and(Ok(true))
                }
            }

            /// Convenient type alias.
            type RioSourceQuad<'a> = ([Term<'a>; 3], Option<Term<'a>>);
            sophia_api::make_scoped_quad_streaming_mode!(ScopedRioSourceQuad, RioSourceQuad);
        }
    };
}

#[cfg(feature = "generalized")]
#[macro_export]
/// Implement Sophia's `QuadSource` for a Rio `GeneralizedQuadsParser`.
macro_rules! impl_quad_source_generalized {
    ($parser:ident) => {
        mod as_sophia_quad_source {
            use super::*;
            use rio_api::model::{GeneralizedQuad, Term};
            use rio_api::parser::GeneralizedQuadsParser;
            use rio_api::sophia::RioStreamError;
            use sophia_api::quad::stream::*;
            use sophia_api::quad::streaming_mode::*;
            use std::error::Error;
            use std::io::BufRead;

            impl<B: BufRead> QuadSource for $parser<B> {
                type Error = <$parser<B> as GeneralizedQuadsParser>::Error;
                type Quad = ScopedGeneralizedQuad;
                fn try_for_some_quad<F, EF>(
                    &mut self,
                    f: &mut F,
                ) -> StreamResult<bool, Self::Error, EF>
                where
                    F: FnMut(StreamedQuad<'_, Self::Quad>) -> Result<(), EF>,
                    EF: Error,
                {
                    if self.is_end() {
                        return Ok(false);
                    }
                    self.parse_step(&mut |q| -> Result<(), RioStreamError<Self::Error, EF>> {
                        f(StreamedQuad::scoped(q)).map_err(|e| SinkError(e).into())
                    })
                    .map_err(|e| e.into())
                    .and(Ok(true))
                }
            }

            sophia_api::make_scoped_quad_streaming_mode!(ScopedGeneralizedQuad, GeneralizedQuad);
        }
    };
}

// A wrapper around Sophia's `StreamError`
// fullfilling Rio's expectation that the error type of `triple_handler`/`quad_handler`
// implement From<TurtleError> (or whatever Rio-specific error returned by the parser).
pub struct RioStreamError<E1, E2>(StreamError<E1, E2>)
where
    E1: Error + 'static,
    E2: Error + 'static;

impl<E1, E2> From<E1> for RioStreamError<E1, E2>
where
    E1: Error + 'static,
    E2: Error + 'static,
{
    #[inline]
    fn from(other: E1) -> Self {
        RioStreamError(SourceError(other))
    }
}

impl<E1, E2> From<StreamError<E1, E2>> for RioStreamError<E1, E2>
where
    E1: Error + 'static,
    E2: Error + 'static,
{
    #[inline]
    fn from(other: StreamError<E1, E2>) -> Self {
        RioStreamError(other)
    }
}

impl<E1, E2> From<RioStreamError<E1, E2>> for StreamError<E1, E2>
where
    E1: Error + 'static,
    E2: Error + 'static,
{
    #[inline]
    fn from(other: RioStreamError<E1, E2>) -> Self {
        other.0
    }
}
