use crate::util::contains_mark;
use stc_ts_types::Type;
use swc_common::{Globals, Mark, Span, Spanned, SyntaxContext};
use tracing::info;

/// Currently this should be shared between multiple runes, because of builtin
/// types.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct Marks {
    pub(crate) top_level_mark: Mark,
}

impl Marks {
    pub fn top_level_mark(&self) -> Mark {
        self.top_level_mark
    }

    pub fn new(globals: &Globals) -> Self {
        fn m(name: &str) -> Mark {
            let m = Mark::fresh(Mark::root());
            info!("Mark ({}): {:?}", name, SyntaxContext::empty().apply_mark(m));
            m
        }

        swc_common::GLOBALS.set(globals, || Self {
            top_level_mark: m("top level"),
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
