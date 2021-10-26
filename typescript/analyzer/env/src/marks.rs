use swc_common::{Globals, Mark, SyntaxContext};
use tracing::info;

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

pub trait MarkExt: Copy + Into<Mark> {
    fn as_ctxt(self) -> SyntaxContext {
        let ctxt = SyntaxContext::empty();
        ctxt.apply_mark(self.into())
    }
}

impl MarkExt for Mark {}
