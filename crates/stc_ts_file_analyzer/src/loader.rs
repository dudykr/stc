use std::sync::Arc;

use auto_impl::auto_impl;
use stc_ts_types::{ModuleId, ModuleTypeData, Type};
use swc_atoms::JsWord;
use swc_common::FileName;

use crate::VResult;

#[derive(Debug, Clone)]
pub struct ModuleInfo {
    pub module_id: ModuleId,
    /// Must be [Type::Arc] of [Type::Module]
    pub data: Type,
}

///
///
/// Group of circular imports are handled by one thread. This

#[auto_impl(Box, Arc)]
pub trait Load: 'static + Send + Sync {
    fn module_id(&self, base: &Arc<FileName>, src: &JsWord) -> Option<ModuleId>;

    /// Note: This method called within a thread
    fn is_in_same_circular_group(&self, base: ModuleId, dep: ModuleId) -> bool;

    /// This method can be called multiple time for same module.
    ///
    /// Also note that this method is called within a single thread.
    ///
    /// `partial` denotes the types and variables which the [Analyzer] successed
    /// processing, with resolved imports.
    ///
    ///
    /// Returned value must be [Type::Arc] of [Type::Module]
    fn load_circular_dep(
        &self,
        base: ModuleId,
        dep: ModuleId,
        partial: &ModuleTypeData,
    ) -> VResult<Type>;

    /// Note: This method is called in parallel.
    ///
    /// Returned value must be [Type::Arc] of [Type::Module]
    fn load_non_circular_dep(&self, base: ModuleId, dep: ModuleId) -> VResult<Type>;

    /// `module` should be [Type::Arc] of [Type::Module].
    fn declare_module(&self, name: &JsWord, module: Type);
}
