use std::sync::Arc;

use anyhow::Result;
use auto_impl::auto_impl;
use swc_common::{FileName, Mark};
use swc_ecma_ast::Module;
use swc_ecma_loader::resolve::Resolve;

pub mod resolver;
pub mod store;

#[derive(Debug)]
pub struct ModuleRecord {
    pub top_level_mark: Mark,
    pub data: Module,
}

/// A module loader.
#[auto_impl(&, Box, Arc)]
pub trait LoadModule: 'static + Send + Sync {
    /// This method should return **all modules in a cycle**.
    fn load_module(&self, filename: &Arc<FileName>) -> Result<Vec<Arc<ModuleRecord>>>;
}

/// A simple implementation of [LoadModule].
pub struct ModuleLoader<R>
where
    R: Resolve,
{
    resolver: R,
}

impl<R> ModuleLoader<R>
where
    R: Resolve,
{
    pub fn new(resolver: R) -> Self {
        Self { resolver }
    }
}
