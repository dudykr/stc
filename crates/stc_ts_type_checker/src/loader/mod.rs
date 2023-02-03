use std::sync::Arc;

use anyhow::Result;
use auto_impl::auto_impl;
use stc_ts_types::ModuleId;
use stc_ts_utils::StcComments;
use swc_common::{FileName, SyntaxContext};
use swc_ecma_ast::Module;
use swc_ecma_loader::resolve::Resolve;

pub mod resolver;
pub mod store;

pub struct ModuleRecord {
    pub id: ModuleId,
    pub filename: Arc<FileName>,
    pub top_level_ctxt: SyntaxContext,
    pub ast: Module,
}

pub struct Records {
    pub modules: Vec<Arc<ModuleRecord>>,
    pub comments: StcComments,
}

/// A module loader.
#[auto_impl(&, Box, Arc)]
pub trait LoadModule: 'static + Send + Sync {
    /// This method should
    ///
    /// - Should never return empty vector.
    /// - The first item should be the file for `filename`.
    /// - Return **all modules in a cycle**.
    /// - Handle `declare module "foo"`.
    /// - Apply `resolver`.
    ///
    /// ## Tip
    ///
    /// Because of the cycles, this method would load all dependencies
    /// recursively.
    fn load_module(&self, filename: &Arc<FileName>) -> Result<Records>;

    /// Same constraints for [`LoadModule::load_module`] applies.
    fn load_dep(&self, base: &Arc<FileName>, module_specifier: &str) -> Result<Records>;
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

impl<R> LoadModule for ModuleLoader<R>
where
    R: 'static + Sync + Send + Resolve,
{
    fn load_module(&self, filename: &Arc<FileName>) -> Result<Records> {
        todo!()
    }

    fn load_dep(&self, base: &Arc<FileName>, module_specifier: &str) -> Result<Records> {
        todo!()
    }
}
