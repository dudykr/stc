use swc_common::{Mark, SyntaxContext};
use tracing::info;

/// The caller should ensure that [swc_common::GLOBALS] and `Marks` is matched.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct Marks {
    pub(crate) unresolved_mark: Mark,
    unresolved_ctxt: SyntaxContext,
}

impl Marks {
    pub fn unresolved_mark(self) -> Mark {
        self.unresolved_mark
    }

    pub fn unresolved_ctxt(self) -> SyntaxContext {
        self.unresolved_ctxt
    }
}

impl Default for Marks {
    fn default() -> Self {
        fn m(name: &str) -> Mark {
            let m = Mark::fresh(Mark::root());
            info!("Mark ({}): {:?}", name, SyntaxContext::empty().apply_mark(m));
            m
        }

        let unresolved_mark = m("unresolved");

        Self {
            unresolved_mark,
            unresolved_ctxt: SyntaxContext::empty().apply_mark(unresolved_mark),
        }
    }
}

pub trait MarkExt: Copy + Into<Mark> {
    fn as_ctxt(self) -> SyntaxContext {
        let ctxt = SyntaxContext::empty();
        ctxt.apply_mark(self.into())
    }
}

impl MarkExt for Mark {}
