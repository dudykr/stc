use anyhow::Error;
use derivative::Derivative;
use stc_utils::path::intern::FileId;
use std::sync::Arc;
use swc_common::{sync::Lrc, SourceFile, SourceMap};
use swc_ecma_ast::Module;

#[derive(Debug, Clone)]
pub enum Chunk {
    Cycle(Vec<LoadedModule>),
    Single(Arc<LoadedModule>),
}

/// Cheap to clone. [SourceMap] and [SourceFile] are stored inline to make cache
/// efficient.
#[derive(Derivative)]
#[derivative(Debug)]
#[derive(Clone)]
pub struct LoadedModule {
    #[derivative(Debug = "ignore")]
    pub cm: Lrc<SourceMap>,
    #[derivative(Debug = "ignore")]
    pub fm: Lrc<SourceFile>,
    pub module: Arc<Module>,
}

pub trait ModuleLoader {
    fn load(&self, path: FileId) -> Result<Chunk, Error>;
}
