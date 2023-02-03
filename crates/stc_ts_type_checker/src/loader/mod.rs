use std::sync::Arc;

use anyhow::Result;
use auto_impl::auto_impl;
use stc_ts_utils::StcComments;
use swc_common::{FileName, SyntaxContext};
use swc_ecma_ast::Module;
use swc_ecma_loader::resolve::Resolve;

pub mod resolver;
pub mod store;

pub struct ModuleRecord {
    pub top_level_ctxt: SyntaxContext,
    pub ast: Module,
    /// **All modules in one cycle should share same instance**.
    pub comments: StcComments,
}

/// A module loader.
#[auto_impl(&, Box, Arc)]
pub trait LoadModule: 'static + Send + Sync {
    /// This method should
    ///
    /// - Return **all modules in a cycle**.
    /// - Handle `declare module "foo"`.
    /// - Apply `resolver`.
    ///
    /// ## Tip
    ///
    /// Because of the cycles, this method would load all dependencies
    /// recursively.
    fn load_module(&self, filename: &Arc<FileName>) -> Result<Vec<Arc<ModuleRecord>>>;

    /// Same constraints for [`LoadModule::load_module`] applies.
    fn load_dep(&self, base: &Arc<FileName>, module_specifier: &str) -> Result<Vec<Arc<ModuleRecord>>>;
}

/// A simple implementation of [LoadModule].
pub struct ModuleLoader<R>
where
    R: 'static + Sync + Send + Resolve,
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

impl<R> LoadModule for ModuleLoader<R> where R: 'static + Sync + Send + Resolve {}
