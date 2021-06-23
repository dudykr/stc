use crate::util::contains_mark;
use stc_ts_types::Type;
use swc_common::{Globals, Mark, Span, Spanned, SyntaxContext};

/// Currently this should be shared between multiple runes, because of builtin
/// types.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct Marks {
    pub(crate) top_level_mark: Mark,
    /// If the mark is applied, it means that the literal should not be
    /// generalized.
    pub(crate) prevent_generalization_mark: Mark,

    pub(crate) prevent_tuple_to_array: Mark,

    /// If this mark is applied, type will not be inferred (based on constraint)
    /// while simplifying.
    pub(super) prevent_complex_simplification_mark: Mark,

    pub(super) implicit_type_mark: Mark,

    ///  WHen applied to a type, it prevents expansion of the type.
    pub(super) no_expand_mark: Mark,

    /// Has a precedence over `no_expand_mark`.
    pub(super) ignore_no_expand_mark: Mark,

    pub(super) contains_infer_type_mark: Mark,

    pub(super) infected_by_this_in_object_literal: Mark,

    pub(super) prevent_converting_to_children: Mark,

    /// This mark is applied to types resolved from variables.
    ///
    /// Used to distinguish object literal with a reference to object literal.
    pub(crate) resolved_from_var: Mark,
}

impl Marks {
    pub fn top_level_mark(&self) -> Mark {
        self.top_level_mark
    }

    pub fn new(globals: &Globals) -> Self {
        fn m() -> Mark {
            Mark::fresh(Mark::root())
        }

        swc_common::GLOBALS.set(globals, || Self {
            top_level_mark: m(),
            prevent_generalization_mark: m(),
            prevent_tuple_to_array: m(),
            prevent_complex_simplification_mark: m(),
            implicit_type_mark: m(),
            no_expand_mark: m(),
            contains_infer_type_mark: m(),
            ignore_no_expand_mark: m(),
            infected_by_this_in_object_literal: m(),
            prevent_converting_to_children: m(),
            resolved_from_var: m(),
        })
    }
}

pub(crate) trait MarkExt: Copy + Into<Mark> {
    fn as_ctxt(self) -> SyntaxContext {
        let ctxt = SyntaxContext::empty();
        ctxt.apply_mark(self.into())
    }
    fn apply_to_type(self, ty: &mut Type) {
        let span = ty.span();
        let span = self.apply_to_span(span);
        ty.respan(span);
    }

    fn apply_to_span(self, span: Span) -> Span {
        span.apply_mark(self.into())
    }

    fn is_marked<S>(self, node: S) -> bool
    where
        S: Spanned,
    {
        let target_mark = self.into();
        let mut ctxt: SyntaxContext = node.span().ctxt;

        loop {
            let mark = ctxt.remove_mark();
            if mark == Mark::root() {
                return false;
            }

            if mark == target_mark {
                return true;
            }
        }
    }

    fn contained_in_type(self, ty: &Type) -> bool {
        contains_mark(&ty, self.into())
    }
}

impl MarkExt for Mark {}
