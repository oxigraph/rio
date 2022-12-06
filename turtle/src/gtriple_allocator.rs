//! I define [`GeneralizedTripleAllocator`]

#![allow(unsafe_code)]
use crate::utils::StringBufferStack;
use rio_api::model::*;
use std::mem::transmute;

/// A stack allocator for storing generalized RDF and RDF-star triples.
///
/// # Implementation
/// This type uses `&'static str` internally to reference text that it allocates.
/// It therefore contains unsafe code to cheat the borrow checker,
/// but ensures that the referenced data lives as long as the referencing struct.
pub struct GeneralizedTripleAllocator {
    incomplete_stack: Vec<[GeneralizedTerm<'static>; 3]>,
    incomplete_len: usize,
    #[allow(clippy::vec_box)]
    complete_stack: Vec<Box<[GeneralizedTerm<'static>; 3]>>,
    complete_len: usize,
    string_stack: StringBufferStack,
}

impl GeneralizedTripleAllocator {
    pub fn new() -> GeneralizedTripleAllocator {
        GeneralizedTripleAllocator {
            incomplete_stack: Vec::with_capacity(1),
            incomplete_len: 0,
            complete_stack: Vec::with_capacity(1),
            complete_len: 0,
            string_stack: StringBufferStack::with_capacity(4),
        }
    }

    pub fn complete_len(&self) -> usize {
        self.complete_len
    }

    pub fn incomplete_len(&self) -> usize {
        self.incomplete_len
    }

    /// Return the last completed triple
    pub fn top(&self) -> &[GeneralizedTerm<'_>; 3] {
        debug_assert!(self.complete_len > 0);
        &self.complete_stack[self.complete_len - 1]
    }

    /// Return the subject of the current (in-progress) triple, if any.
    pub fn current_subject(&self) -> Option<GeneralizedTerm> {
        if self.incomplete_len == 0 {
            None
        } else {
            debug_assert!(!dummy(self.incomplete_stack[self.incomplete_len - 1][0]));
            Some(self.incomplete_stack[self.incomplete_len - 1][0])
        }
    }

    /// Generate a quad from the top completed triple,
    /// with the given graph name.
    pub fn top_quad<'s>(&'s self, graph_name: Option<GeneralizedTerm<'s>>) -> GeneralizedQuad<'s> {
        debug_assert!(self.complete_len > 0);
        let [subject, predicate, object] = *self.top();
        GeneralizedQuad {
            subject,
            predicate,
            object,
            graph_name,
        }
    }

    pub fn push_triple_start(&mut self) {
        let dummy: GeneralizedTerm = DUMMY_IRI.into();
        if self.incomplete_len == self.incomplete_stack.len() {
            self.incomplete_stack.push([dummy, dummy, dummy])
        }
        #[cfg(debug_assertions)] // to ensure that dummy() assertions work
        {
            self.incomplete_stack[self.incomplete_len] = [dummy, dummy, dummy];
        }
        self.incomplete_len += 1;
    }

    /// Push an atomic term,
    /// produced by `subject_factory`,
    /// at the position `pos` of the current triple.
    ///
    /// # Pre-condition
    /// In standard RDF, any term is acceptable.
    /// In RDF-star embedded triples [`GeneralizedTerm::Triple`] are *not allowed*.
    /// For adding an embedded triple, use [`TripleAllocator::push_quoted_triple`] instead.
    pub fn try_push_atom<E, F>(&mut self, pos: usize, term_factory: F) -> Result<(), E>
    where
        F: for<'x> FnOnce(&'x mut String, &'x mut String) -> Result<GeneralizedTerm<'x>, E>,
    {
        debug_assert!(pos < 3);
        debug_assert!(pos == 0 || !dummy(self.current()[pos - 1]));
        debug_assert!(dummy(self.current()[pos]));
        let buffers = self.string_stack.push2();
        let atom = term_factory(buffers.0, buffers.1)?;
        debug_assert!(!matches!(atom, GeneralizedTerm::Triple(_)));
        let atom: GeneralizedTerm<'static> = unsafe { transmute(atom) };
        // The unsafe code above changes the lifetime parameter of atom to `'static`.
        // This is ok because:
        // * we will only expose it with a shorter lifetime, and
        // * this implementation guarantees that the pointed `str` lives as long as the atom
        if pos < 2 {
            self.current()[pos] = atom;
        } else {
            self.complete_triple(atom);
        }
        Ok(())
    }

    /// Use the [top](TripleAllocator::top) triple of this stash at the position `pos` of the current triple.
    pub fn push_quoted_triple(&mut self, pos: usize) {
        debug_assert!(pos < 3);
        debug_assert!(dummy(self.current()[pos]));
        debug_assert!(self.complete_len > 0);
        let triple = &*self.complete_stack[self.complete_len - 1];

        debug_assert!((0..pos).all(
            |i| match self.incomplete_stack[self.incomplete_len - 1][i] {
                GeneralizedTerm::Triple(s) => {
                    let ptr_s: *const _ = s;
                    ptr_s != triple
                }
                _ => true,
            }
        ));

        let triple: &'static [GeneralizedTerm<'static>; 3] = unsafe { transmute(triple) };
        // The unsafe code above changes the lifetime of the ref to `'static`.
        // This is ok because:
        // * we will only expose it with a shorter lifetime, and
        // * this implementation guarantees that the pointed `Triple` lives as long as the `Term` embedding it
        let quoted = GeneralizedTerm::Triple(triple);
        if pos < 2 {
            self.current()[pos] = quoted;
        } else {
            self.complete_triple(quoted);
        }
    }

    pub fn pop_term(&mut self, pos: usize) {
        debug_assert!(pos < 3);
        if pos == 2 {
            debug_assert!(self.complete_len > 0);
            self.complete_len -= 1;
            let inc_triple = *self.complete_stack[self.complete_len];
            if self.incomplete_len == self.incomplete_stack.len() {
                self.incomplete_stack.push(inc_triple)
            } else {
                self.incomplete_stack[self.incomplete_len] = inc_triple;
            }
            self.incomplete_len += 1;
        } else {
            debug_assert!(dummy(self.current()[pos + 1]));
        };

        match self.current()[pos] {
            GeneralizedTerm::Triple(_) => self.pop_top_triple(),
            _ => {
                // we allocate two buffers for any atomic object, not just literals
                self.string_stack.pop();
                self.string_stack.pop()
            }
        }
        #[cfg(debug_assertions)] // to ensure that dummy() assertions work
        {
            self.current()[pos] = DUMMY_IRI.into();
        }
    }

    #[inline(always)]
    /// Pops the latest complete triple, and recursively pops all its constituent triples.
    /// Equivalent to pop_object, pop_predicate, pop_subject, pop_empty_triple
    pub fn pop_top_triple(&mut self) {
        self.pop_term(2);
        self.pop_term(1);
        self.pop_term(0);
        self.incomplete_len -= 1;
    }

    /// Pops the top-most empty triple, created with push_triple_start,
    /// but with all its components having been popped (or never pushed)
    #[inline(always)]
    pub fn pop_top_empty_triple(&mut self) {
        debug_assert!(self.incomplete_len > 0);
        debug_assert!(dummy(self.current()[1]));
        debug_assert!(dummy(self.current()[0]));
        self.incomplete_len -= 1;
    }

    /// Pops the top-most annotation triple, i.e.
    /// a triple on the incomplete stack with only its subject pushed,
    /// and that subject is an embedded triple.
    ///
    /// The goal is to remove this triple *without* freeing the subject triple.
    #[inline(always)]
    pub fn pop_annotation_triple(&mut self) {
        debug_assert!(self.incomplete_len > 0);
        debug_assert!(dummy(self.current()[1]));
        debug_assert!(!dummy(self.current()[0]));
        debug_assert!(matches!(self.current()[0], GeneralizedTerm::Triple(_)));
        self.incomplete_len -= 1;
    }

    pub fn clear(&mut self) {
        self.incomplete_len = 0;
        self.incomplete_stack.clear();
        self.complete_len = 0;
        self.complete_stack.clear();
        self.string_stack.clear();
    }

    fn complete_triple(&mut self, object: GeneralizedTerm<'static>) {
        self.incomplete_len -= 1;
        let mut triple = self.incomplete_stack[self.incomplete_len];
        triple[2] = object;
        if self.complete_len == self.complete_stack.len() {
            self.complete_stack.push(Box::new(triple))
        } else {
            *self.complete_stack[self.complete_len] = triple;
        }
        self.complete_len += 1;
    }

    fn current(&mut self) -> &mut [GeneralizedTerm<'static>; 3] {
        debug_assert!(self.incomplete_len > 0);
        &mut self.incomplete_stack[self.incomplete_len - 1]
    }
}

#[cfg(debug_assertions)] // debug assertions need DUMMY to have a static address
static DUMMY_IRI: NamedNode<'static> = NamedNode { iri: "" };
#[cfg(not(debug_assertions))] // otherwise, a const is sufficient
const DUMMY_IRI: NamedNode<'static> = NamedNode { iri: "" };

fn dummy<'a, T: std::fmt::Debug + Into<GeneralizedTerm<'a>>>(t: T) -> bool {
    match t.into() {
        GeneralizedTerm::NamedNode(n) => {
            let ptr_iri: *const str = n.iri;
            ptr_iri == DUMMY_IRI.iri
        }
        _ => false,
    }
}

#[cfg(test)]
mod test {
    use super::*;
    use std::convert::Infallible;

    #[cfg(debug_assertions)]
    #[test]
    fn dummy_works() {
        assert!(dummy(DUMMY_IRI));
        let b = "foo".to_string();
        let n = NamedNode { iri: &b[..0] };
        assert!(DUMMY_IRI.iri == n.iri);
        // and yet:
        assert!(!dummy(n));
    }

    fn iri<'a, T: From<NamedNode<'a>>>(
        buffer: &'a mut String,
        value: &str,
    ) -> Result<T, Infallible> {
        buffer.push_str(value);
        Ok(NamedNode { iri: &*buffer }.into())
    }

    fn bn<'a, T: From<BlankNode<'a>>>(
        buffer: &'a mut String,
        value: &str,
    ) -> Result<T, Infallible> {
        buffer.push_str(value);
        Ok(BlankNode { id: &*buffer }.into())
    }

    fn sl<'a, T: From<Literal<'a>>>(buffer: &'a mut String, value: &str) -> Result<T, Infallible> {
        buffer.push_str(value);
        Ok(Literal::Simple { value: &*buffer }.into())
    }

    fn lt<'a, T: From<Literal<'a>>>(
        buffer1: &'a mut String,
        buffer2: &'a mut String,
        value: &str,
        tag: &str,
    ) -> Result<T, Infallible> {
        buffer1.push_str(value);
        buffer2.push_str(tag);
        Ok(Literal::LanguageTaggedString {
            value: &*buffer1,
            language: &*buffer2,
        }
        .into())
    }

    fn dt<'a, T: From<Literal<'a>>>(
        buffer1: &'a mut String,
        buffer2: &'a mut String,
        value: &str,
        dt: &str,
    ) -> Result<T, Infallible> {
        buffer1.push_str(value);
        buffer2.push_str(dt);
        Ok(Literal::Typed {
            value: &*buffer1,
            datatype: NamedNode { iri: &*buffer2 },
        }
        .into())
    }

    #[test]
    fn simple_triple_w_named() -> Result<(), Infallible> {
        let mut ta = GeneralizedTripleAllocator::new();
        ta.push_triple_start();
        ta.try_push_atom(0, |b, _| iri(b, "a"))?;
        ta.try_push_atom(1, |b, _| iri(b, "b"))?;
        ta.try_push_atom(2, |b, _| iri(b, "c"))?;
        assert_eq!(display(ta.top()), r#"<a> <b> <c>"#);
        Ok(())
    }

    #[test]
    fn simple_triple_w_blank() -> Result<(), Infallible> {
        let mut ta = GeneralizedTripleAllocator::new();
        ta.push_triple_start();
        ta.try_push_atom(0, |b, _| bn(b, "a"))?;
        ta.try_push_atom(1, |b, _| bn(b, "b"))?;
        ta.try_push_atom(2, |b, _| bn(b, "c"))?;
        assert_eq!(display(ta.top()), r#"_:a _:b _:c"#);
        Ok(())
    }

    #[test]
    fn simple_triple_w_simple_lit() -> Result<(), Infallible> {
        let mut ta = GeneralizedTripleAllocator::new();
        ta.push_triple_start();
        ta.try_push_atom(0, |b, _| sl(b, "a"))?;
        ta.try_push_atom(1, |b, _| sl(b, "b"))?;
        ta.try_push_atom(2, |b, _| sl(b, "c"))?;
        assert_eq!(display(ta.top()), r#""a" "b" "c""#);
        Ok(())
    }

    #[test]
    fn simple_triple_w_lang_lit() -> Result<(), Infallible> {
        let mut ta = GeneralizedTripleAllocator::new();
        ta.push_triple_start();
        ta.try_push_atom(0, |b1, b2| lt(b1, b2, "a", "en"))?;
        ta.try_push_atom(1, |b1, b2| lt(b1, b2, "b", "fr"))?;
        ta.try_push_atom(2, |b1, b2| lt(b1, b2, "c", "ge"))?;
        assert_eq!(display(ta.top()), r#""a"@en "b"@fr "c"@ge"#);
        Ok(())
    }

    #[test]
    fn simple_triple_w_typed_lit() -> Result<(), Infallible> {
        let mut ta = GeneralizedTripleAllocator::new();
        ta.push_triple_start();
        ta.try_push_atom(0, |b1, b2| dt(b1, b2, "a", "b"))?;
        ta.try_push_atom(1, |b1, b2| dt(b1, b2, "c", "d"))?;
        ta.try_push_atom(2, |b1, b2| dt(b1, b2, "e", "f"))?;
        assert_eq!(display(ta.top()), r#""a"^^<b> "c"^^<d> "e"^^<f>"#);
        Ok(())
    }

    #[test]
    fn simple_triples_pop() -> Result<(), Infallible> {
        let mut ta = GeneralizedTripleAllocator::new();
        ta.push_triple_start();
        ta.try_push_atom(0, |b, _| iri(b, "a"))?;
        ta.try_push_atom(1, |b, _| iri(b, "b"))?;
        ta.try_push_atom(2, |b, _| iri(b, "c"))?;
        assert_eq!(display(ta.top()), r#"<a> <b> <c>"#);
        ta.pop_term(2);
        ta.try_push_atom(2, |b, _| iri(b, "d"))?;
        assert_eq!(display(ta.top()), r#"<a> <b> <d>"#);
        ta.pop_term(2);
        ta.pop_term(1);
        ta.try_push_atom(1, |b, _| iri(b, "e"))?;
        ta.try_push_atom(2, |b, _| iri(b, "f"))?;
        assert_eq!(display(ta.top()), r#"<a> <e> <f>"#);
        ta.pop_term(2);
        ta.try_push_atom(2, |b, _| iri(b, "g"))?;
        assert_eq!(display(ta.top()), r#"<a> <e> <g>"#);
        ta.pop_term(2);
        ta.pop_term(1);
        ta.pop_term(0);
        ta.try_push_atom(0, |b, _| iri(b, "h"))?;
        ta.try_push_atom(1, |b, _| iri(b, "i"))?;
        ta.try_push_atom(2, |b, _| iri(b, "j"))?;
        assert_eq!(display(ta.top()), r#"<h> <i> <j>"#);
        Ok(())
    }

    #[test]
    fn simple_triples_stacked() -> Result<(), Infallible> {
        let mut ta = GeneralizedTripleAllocator::new();
        ta.push_triple_start();
        ta.try_push_atom(0, |b, _| iri(b, "a"))?;
        ta.try_push_atom(1, |b, _| iri(b, "b"))?;
        ta.try_push_atom(2, |b, _| iri(b, "c"))?;
        assert_eq!(display(ta.top()), r#"<a> <b> <c>"#);
        ta.push_triple_start();
        ta.try_push_atom(0, |b, _| iri(b, "d"))?;
        ta.try_push_atom(1, |b, _| iri(b, "e"))?;
        ta.try_push_atom(2, |b, _| iri(b, "f"))?;
        assert_eq!(display(ta.top()), r#"<d> <e> <f>"#);
        ta.pop_top_triple();
        assert_eq!(display(ta.top()), r#"<a> <b> <c>"#);
        ta.pop_top_triple();
        assert_eq!(ta.complete_len, 0);
        assert_eq!(ta.incomplete_len, 0);
        Ok(())
    }

    #[test]
    fn quoted_triple_as_subject() -> Result<(), Infallible> {
        let mut ta = GeneralizedTripleAllocator::new();
        ta.push_triple_start();
        {
            ta.push_triple_start();
            ta.try_push_atom(0, |b, _| bn(b, "a"))?;
            ta.try_push_atom(1, |b, _| iri(b, "b"))?;
            ta.try_push_atom(2, |b, _| sl(b, "c"))?;
            assert_eq!(display(ta.top()), r#"_:a <b> "c""#);
        }
        ta.push_quoted_triple(0);
        ta.try_push_atom(1, |b, _| iri(b, "d"))?;
        ta.try_push_atom(2, |b, _| sl(b, "e"))?;
        assert_eq!(display(ta.top()), r#"<< _:a <b> "c" >> <d> "e""#);
        Ok(())
    }

    #[test]
    fn quoted_triple_as_predicate() -> Result<(), Infallible> {
        let mut ta = GeneralizedTripleAllocator::new();
        ta.push_triple_start();
        ta.try_push_atom(0, |b, _| bn(b, "a"))?;
        {
            ta.push_triple_start();
            ta.try_push_atom(0, |b, _| bn(b, "b"))?;
            ta.try_push_atom(1, |b, _| iri(b, "c"))?;
            ta.try_push_atom(2, |b, _| sl(b, "d"))?;
            assert_eq!(display(ta.top()), r#"_:b <c> "d""#);
        }
        ta.push_quoted_triple(1);
        ta.try_push_atom(2, |b, _| bn(b, "e"))?;
        assert_eq!(display(ta.top()), r#"_:a << _:b <c> "d" >> _:e"#);
        Ok(())
    }

    #[test]
    fn quoted_triple_as_object() -> Result<(), Infallible> {
        let mut ta = GeneralizedTripleAllocator::new();
        ta.push_triple_start();
        ta.try_push_atom(0, |b, _| bn(b, "a"))?;
        ta.try_push_atom(1, |b, _| iri(b, "b"))?;
        {
            ta.push_triple_start();
            ta.try_push_atom(0, |b, _| bn(b, "c"))?;
            ta.try_push_atom(1, |b, _| iri(b, "d"))?;
            ta.try_push_atom(2, |b, _| sl(b, "e"))?;
            assert_eq!(display(ta.top()), r#"_:c <d> "e""#);
        }
        ta.push_quoted_triple(2);
        assert_eq!(display(ta.top()), r#"_:a <b> << _:c <d> "e" >>"#);
        Ok(())
    }

    #[test]
    fn quoted_triple_as_both() -> Result<(), Infallible> {
        let mut ta = GeneralizedTripleAllocator::new();
        ta.push_triple_start();
        {
            ta.push_triple_start();
            ta.try_push_atom(0, |b, _| bn(b, "a"))?;
            ta.try_push_atom(1, |b, _| iri(b, "b"))?;
            ta.try_push_atom(2, |b, _| sl(b, "c"))?;
            assert_eq!(display(ta.top()), r#"_:a <b> "c""#);
        }
        ta.push_quoted_triple(0);
        ta.try_push_atom(1, |b, _| iri(b, "d"))?;
        {
            ta.push_triple_start();
            ta.try_push_atom(0, |b, _| bn(b, "e"))?;
            ta.try_push_atom(1, |b, _| iri(b, "f"))?;
            ta.try_push_atom(2, |b, _| sl(b, "g"))?;
            assert_eq!(display(ta.top()), r#"_:e <f> "g""#);
        }
        ta.push_quoted_triple(2);
        assert_eq!(
            display(ta.top()),
            r#"<< _:a <b> "c" >> <d> << _:e <f> "g" >>"#
        );
        Ok(())
    }

    #[test]
    fn quoted_triple_deep() -> Result<(), Infallible> {
        let mut ta = GeneralizedTripleAllocator::new();
        ta.push_triple_start();
        {
            ta.push_triple_start();
            ta.try_push_atom(0, |b, _| bn(b, "a"))?;
            ta.try_push_atom(1, |b, _| iri(b, "b"))?;
            {
                ta.push_triple_start();
                ta.try_push_atom(0, |b, _| bn(b, "c"))?;
                ta.try_push_atom(1, |b, _| iri(b, "d"))?;
                ta.try_push_atom(2, |b, _| sl(b, "e"))?;
                assert_eq!(display(ta.top()), r#"_:c <d> "e""#);
            }
            ta.push_quoted_triple(2);
            assert_eq!(display(ta.top()), r#"_:a <b> << _:c <d> "e" >>"#);
        }
        ta.push_quoted_triple(0);
        ta.try_push_atom(1, |b, _| iri(b, "f"))?;
        {
            ta.push_triple_start();
            {
                ta.push_triple_start();
                ta.try_push_atom(0, |b, _| bn(b, "g"))?;
                ta.try_push_atom(1, |b, _| iri(b, "h"))?;
                ta.try_push_atom(2, |b, _| sl(b, "i"))?;
                assert_eq!(display(ta.top()), r#"_:g <h> "i""#);
            }
            ta.push_quoted_triple(0);
            ta.try_push_atom(1, |b, _| iri(b, "j"))?;
            ta.try_push_atom(2, |b, _| sl(b, "k"))?;
            assert_eq!(display(ta.top()), r#"<< _:g <h> "i" >> <j> "k""#);
        }
        ta.push_quoted_triple(2);
        assert_eq!(
            display(ta.top()),
            r#"<< _:a <b> << _:c <d> "e" >> >> <f> << << _:g <h> "i" >> <j> "k" >>"#
        );

        ta.pop_top_triple();
        assert_eq!(ta.complete_len, 0);
        assert_eq!(ta.incomplete_len, 0);
        Ok(())
    }

    fn display(triple: &[GeneralizedTerm<'_>; 3]) -> String {
        format!("{} {} {}", triple[0], triple[1], triple[2])
    }
}
