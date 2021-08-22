use async_trait::async_trait;
use auto_impl::auto_impl;
use stc_utils::path::intern::FileId;
use swc_common::{sync::Lrc, SourceFile, SourceMap};

pub mod cache;
pub mod dedup;

/// For efficient cacing, this uses one [swc_common::SourceMap] per file.
///
///
/// Note that [swc_common::Globals] is shared.
///
///
/// Recmmended type: `Cached<Deduplicated<ActualImpl>>`
#[async_trait]
#[auto_impl(Arc, Box)]
pub trait TypeChecker: Sized + Send + Sync {
    async fn check(&self, name: FileId, src: &str) -> FileData;
}

/// This is cheap to clone.
#[derive(Clone)]
pub struct FileData {
    pub cm: Lrc<SourceMap>,
    pub fm: Lrc<SourceFile>,
}
