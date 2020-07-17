//! Implementation of sophia TripleSource / QuadSource for rio parsers

use crate::model::*;
use crate::parser::*;
use sophia_api::quad::stream::*;
use sophia_api::quad::streaming_mode::StreamedQuad;
use sophia_api::triple::stream::*;
use sophia_api::triple::streaming_mode::StreamedTriple;
use std::error::Error;
use std::result::Result as StdResult;

/// TripleSource / QuadSource adapter for TripleParser / QuadParser
pub enum StrictRioSource<T, E> {
    Parser(T),
    Error(Option<E>),
}

impl<T, E> From<StdResult<T, E>> for StrictRioSource<T, E> {
    fn from(res: StdResult<T, E>) -> Self {
        match res {
            Ok(parser) => StrictRioSource::Parser(parser),
            Err(error) => StrictRioSource::Error(Some(error)),
        }
    }
}

// This intermediate type is required,
// because Rio requires that the error type of triple_handler/quad_handler
// implement From<TurtleError> (or whatever Rio-specific error returned by the parser).
//
// This is costless, though,
// because MyStreamError's internal representation is identical to StreamError,
// so the final type conversion performed by into_stream_error is actually
// just for pleasing the compiler.
enum MyStreamError<E1, E2> {
    Source(E1),
    Sink(E2),
}
impl<E1, E2> MyStreamError<E1, E2>
where
    E1: Error + 'static,
    E2: Error + 'static,
{
    fn from_sink_error(err: E2) -> Self {
        MyStreamError::Sink(err)
    }
    fn into_stream_error(self) -> StreamError<E1, E2> {
        match self {
            MyStreamError::Source(err) => SourceError(err),
            MyStreamError::Sink(err) => SinkError(err),
        }
    }
}
impl<E1, E2> From<E1> for MyStreamError<E1, E2>
where
    E1: Error + 'static,
    E2: Error + 'static,
{
    fn from(other: E1) -> Self {
        MyStreamError::Source(other)
    }
}

pub type RioSourceTriple<'a> = [Term<'a>; 3];
sophia_api::make_scoped_triple_streaming_mode!(ScopedRioSourceTriple, RioSourceTriple);

impl<T, E> TripleSource for StrictRioSource<T, E>
where
    T: TriplesParser<Error = E>,
    E: Error + 'static,
{
    type Error = E;
    //type Triple = crate::triple::streaming_mode::ByValue<RioSourceTriple<'static>>;
    type Triple = ScopedRioSourceTriple;

    fn try_for_some_triple<F, EF>(&mut self, f: &mut F) -> StreamResult<bool, E, EF>
    where
        F: FnMut(StreamedTriple<'_, Self::Triple>) -> Result<(), EF>,
        EF: Error,
    {
        match self {
            StrictRioSource::Error(opt) => Err(SourceError(consume_err(opt))),
            StrictRioSource::Parser(parser) => {
                if parser.is_end() {
                    return Ok(false);
                }
                parser
                    .parse_step(&mut |t| -> StdResult<(), MyStreamError<E, EF>> {
                        f(StreamedTriple::scoped([
                            t.subject.into(),
                            t.predicate.into(),
                            t.object,
                        ]))
                        .map_err(MyStreamError::from_sink_error)
                    })
                    .map_err(|e| e.into_stream_error())
                    .and(Ok(true))
            }
        }
    }
}

pub type RioSourceQuad<'a> = ([Term<'a>; 3], Option<Term<'a>>);
sophia_api::make_scoped_quad_streaming_mode!(ScopedRioSourceQuad, RioSourceQuad);

impl<T, E> QuadSource for StrictRioSource<T, E>
where
    T: QuadsParser<Error = E>,
    E: Error + 'static,
{
    type Error = E;
    type Quad = ScopedRioSourceQuad;

    fn try_for_some_quad<F, EF>(&mut self, f: &mut F) -> StreamResult<bool, E, EF>
    where
        F: FnMut(StreamedQuad<'_, Self::Quad>) -> Result<(), EF>,
        EF: Error,
    {
        match self {
            StrictRioSource::Error(opt) => Err(SourceError(consume_err(opt))),
            StrictRioSource::Parser(parser) => {
                if parser.is_end() {
                    return Ok(false);
                }
                parser
                    .parse_step(&mut |q| -> StdResult<(), MyStreamError<E, EF>> {
                        f(StreamedQuad::scoped((
                            [q.subject.into(), q.predicate.into(), q.object],
                            q.graph_name.map(|g| g.into()),
                        )))
                        .map_err(MyStreamError::from_sink_error)
                    })
                    .map_err(|e| e.into_stream_error())
                    .and(Ok(true))
            }
        }
    }
}

#[cfg(feature = "generalized")]
mod generalized {
    use super::*;

    /// QuadSource adapter for RIO GeneralizedQuadParser
    pub enum GeneralizedRioSource<T, E> {
        Parser(T),
        Error(Option<E>),
    }

    impl<T, E> From<StdResult<T, E>> for GeneralizedRioSource<T, E> {
        fn from(res: StdResult<T, E>) -> Self {
            match res {
                Ok(parser) => GeneralizedRioSource::Parser(parser),
                Err(error) => GeneralizedRioSource::Error(Some(error)),
            }
        }
    }

    sophia_api::make_scoped_quad_streaming_mode!(ScopedGeneralizedQuad, GeneralizedQuad);

    impl<T, E> QuadSource for GeneralizedRioSource<T, E>
    where
        T: GeneralizedQuadsParser<Error = E>,
        E: Error + 'static,
    {
        type Error = E;
        type Quad = ScopedGeneralizedQuad;

        fn try_for_some_quad<F, EF>(&mut self, f: &mut F) -> StreamResult<bool, E, EF>
        where
            F: FnMut(StreamedQuad<'_, Self::Quad>) -> Result<(), EF>,
            EF: Error,
        {
            match self {
                GeneralizedRioSource::Error(opt) => Err(SourceError(consume_err(opt))),
                GeneralizedRioSource::Parser(parser) => {
                    if parser.is_end() {
                        return Ok(false);
                    }
                    parser
                        .parse_step(&mut |q| -> StdResult<(), MyStreamError<E, EF>> {
                            f(StreamedQuad::scoped(q)).map_err(MyStreamError::from_sink_error)
                        })
                        .map_err(|e| e.into_stream_error())
                        .and(Ok(true))
                }
            }
        }
    }
}
#[cfg(feature = "generalized")]
pub use generalized::*;

/// Consume inner error and convert it to Error
fn consume_err<E>(opt: &mut Option<E>) -> E {
    opt.take().unwrap_or_else(|| {
        panic!("This parser has failed previously, and can not be used anymore");
    })
}
