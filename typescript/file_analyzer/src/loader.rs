use crate::ValidationResult;
use stc_ts_types::{ModuleId, ModuleTypeData};
use stc_utils::path::intern::FileId;
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
    fn module_id(&self, base: FileId, src: &JsWord) -> Option<ModuleId>;

    /// Note: This method called within a thread
    fn is_in_same_circular_group(&self, base: ModuleId, dep: ModuleId) -> bool;

    /// This method can be called multiple time for same module.
    ///
    /// Also note that this method is called within a single thread.
    ///
    /// `partial` denotes the types and variables which the [Analyzer] successed
    /// processing, with resolved imports.
    fn load_circular_dep(
        &self,
        base: ModuleId,
        dep: ModuleId,
        partial: &ModuleTypeData,
    ) -> ValidationResult<ModuleInfo>;

    /// Note: This method is called in parallel.
    fn load_non_circular_dep(&self, base: ModuleId, dep: ModuleId) -> ValidationResult<ModuleInfo>;
}

impl<T> Load for Arc<T>
where
    T: ?Sized + Load,
{
    fn module_id(&self, base: FileId, src: &JsWord) -> Option<ModuleId> {
        (**self).module_id(base, src)
    }

    fn is_in_same_circular_group(&self, base: ModuleId, dep: ModuleId) -> bool {
        (**self).is_in_same_circular_group(base, dep)
    }

    fn load_circular_dep(
        &self,
        base: ModuleId,
        dep: ModuleId,
        partial: &ModuleTypeData,
    ) -> ValidationResult<ModuleInfo> {
        (**self).load_circular_dep(base, dep, partial)
    }

    fn load_non_circular_dep(&self, base: ModuleId, dep: ModuleId) -> ValidationResult<ModuleInfo> {
        (**self).load_non_circular_dep(base, dep)
    }
}

impl<T> Load for Box<T>
where
    T: ?Sized + Load,
{
    fn module_id(&self, base: FileId, src: &JsWord) -> Option<ModuleId> {
        (**self).module_id(base, src)
    }

    fn is_in_same_circular_group(&self, base: ModuleId, dep: ModuleId) -> bool {
        (**self).is_in_same_circular_group(base, dep)
    }

    fn load_circular_dep(
        &self,
        base: ModuleId,
        dep: ModuleId,
        partial: &ModuleTypeData,
    ) -> ValidationResult<ModuleInfo> {
        (**self).load_circular_dep(base, dep, partial)
    }

    fn load_non_circular_dep(&self, base: ModuleId, dep: ModuleId) -> ValidationResult<ModuleInfo> {
        (**self).load_non_circular_dep(base, dep)
    }
}
