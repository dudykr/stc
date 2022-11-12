use swc_common::{Globals, Mark, SyntaxContext};
use tracing::info;

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct Marks {
    pub(crate) unresolved_mark: Mark,
}

impl Marks {
    pub fn unresolved_mark(self) -> Mark {
        self.unresolved_mark
    }

    pub fn new(globals: &Globals) -> Self {
        fn m(name: &str) -> Mark {
            let m = Mark::fresh(Mark::root());
            info!("Mark ({}): {:?}", name, SyntaxContext::empty().apply_mark(m));
            m
        }

        swc_common::GLOBALS.set(globals, || Self {
            unresolved_mark: m("unresolved"),
        })
    }
}

pub trait MarkExt: Copy + Into<Mark> {
    fn as_ctxt(self) -> SyntaxContext {
        let ctxt = SyntaxContext::empty();
        ctxt.apply_mark(self.into())
    }
}

impl MarkExt for Mark {}
