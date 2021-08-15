use swc_common::{sync::Lrc, SourceFile, SourceMap};

pub mod cache;

/// For efficient cacing, this uses one [swc_common::SourceMap] per file.

///
///
/// Note that [swc_common::Globals] is shared.
pub trait TypeChecker {
    fn check(&self, cm: Lrc<SourceMap>, fm: Lrc<SourceFile>) -> FileResult;
}

#[derive(Clone)]
pub struct FileResult {
    pub cm: Lrc<SourceMap>,
    pub fm: Lrc<SourceFile>,
}
