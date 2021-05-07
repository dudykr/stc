use crate::ValidationResult;
use stc_ts_types::{ModuleId, ModuleTypeData};
use std::{path::PathBuf, sync::Arc};
use swc_atoms::JsWord;

#[derive(Debug, Clone)]
pub struct ModuleInfo {
    pub module_id: ModuleId,
    pub data: Arc<ModuleTypeData>,
}

///
///
/// Group of circular imports are handled by one thread. This
pub trait Load: 'static + Send + Sync {
    fn module_id(&self, base: &Arc<PathBuf>, src: &JsWord) -> Option<ModuleId>;

    /// Note: This method called within a thread
    fn is_in_same_circular_group(&self, dep: ModuleId) -> bool;

    /// This method can be called multiple time for same module.
    ///
    /// Also note that this method is called within a single thread.
    ///
    /// `partial` denotes the types and variables which the [Analyzer] successed
    /// processing, with resolved imports.
    fn load_circular_dep(&self, dep: ModuleId, partial: &ModuleTypeData) -> ValidationResult<ModuleInfo>;

    /// Note: This method is called in parallel.
    fn load_non_circular_dep(&self, dep: ModuleId) -> ValidationResult<ModuleInfo>;
}

impl<T> Load for Arc<T>
where
    T: ?Sized + Load,
{
    fn module_id(&self, base: &Arc<PathBuf>, src: &JsWord) -> Option<ModuleId> {
        (**self).module_id(base, src)
    }

    fn is_in_same_circular_group(&self, dep: ModuleId) -> bool {
        (**self).is_in_same_circular_group(dep)
    }

    fn load_circular_dep(&self, dep: ModuleId, partial: &ModuleTypeData) -> ValidationResult<ModuleInfo> {
        (**self).load_circular_dep(dep, partial)
    }

    fn load_non_circular_dep(&self, dep: ModuleId) -> ValidationResult<ModuleInfo> {
        (**self).load_non_circular_dep(dep)
    }
}

impl<T> Load for Box<T>
where
    T: ?Sized + Load,
{
    fn module_id(&self, base: &Arc<PathBuf>, src: &JsWord) -> Option<ModuleId> {
        (**self).module_id(base, src)
    }

    fn is_in_same_circular_group(&self, dep: ModuleId) -> bool {
        (**self).is_in_same_circular_group(dep)
    }

    fn load_circular_dep(&self, dep: ModuleId, partial: &ModuleTypeData) -> ValidationResult<ModuleInfo> {
        (**self).load_circular_dep(dep, partial)
    }

    fn load_non_circular_dep(&self, dep: ModuleId) -> ValidationResult<ModuleInfo> {
        (**self).load_non_circular_dep(dep)
    }
}
