use std::sync::Arc;

use anyhow::Result;
use swc_common::{FileName, Mark};
use swc_ecma_ast::Module;

pub mod resolver;
pub mod store;

#[derive(Debug)]
pub struct ModuleRecord {
    pub top_level_mark: Mark,
    pub data: Module,
}

/// A module loader.
pub trait LoadModule: 'static + Send + Sync {
    /// This method should return **all modules in a cycle**.
    fn load_module(&self, filename: &Arc<FileName>) -> Result<Vec<Arc<ModuleRecord>>>;
}
