use auto_impl::auto_impl;
use swc_common::{errors::Handler, sync::Lrc, SourceFile, SourceMap};

pub mod cache;

/// For efficient cacing, this uses one [swc_common::SourceMap] per file.
///
///
/// Note that [swc_common::Globals] is shared.
#[auto_impl(Arc, Box)]
pub trait TypeChecker {
    fn check(&self, cm: Lrc<SourceMap>, handler: Lrc<Handler>, fm: Lrc<SourceFile>) -> FileResult;
}

#[derive(Clone)]
pub struct FileResult {
    pub cm: Lrc<SourceMap>,
    pub fm: Lrc<SourceFile>,
}
