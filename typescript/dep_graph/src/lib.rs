use anyhow::Error;
use stc_utils::path::intern::FileId;
use std::sync::Arc;
use swc_ecma_ast::Module;

#[derive(Debug, Clone)]
pub enum Chunk {
    Cycle(Vec<Arc<Module>>),
    Single(Arc<Module>),
}

pub trait ModuleLoader {
    fn load(&self, path: FileId) -> Result<Chunk, Error>;
}
