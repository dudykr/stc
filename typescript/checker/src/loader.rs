use super::Checker;
use crate::{errors::Error, util::dashmap::DashMapExt, DepInfo, Specifier};
use dashmap::SharedValue;
use stc_types::{Id, ModuleId, ModuleTypeData, Ref, Type};
use std::{any::type_name, path::PathBuf, sync::Arc, time::Duration};
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
    fn module_id(&self, base: &Arc<PathBuf>, src: &JsWord) -> ModuleId;

    /// Note: This method called within a thread
    fn is_in_same_circular_group(&self, base: &Arc<PathBuf>, src: &JsWord) -> bool;

    /// This method can be called multiple time for same module.
    ///
    /// Also note that this method is called within a single thread.
    ///
    /// `partial` denotes the types and variables which the [Analyzer] successed
    /// processing, with resolved imports.
    fn load_circular_dep(
        &self,
        base: Arc<PathBuf>,
        partial: &ModuleTypeData,
        import: &DepInfo,
    ) -> Result<ModuleInfo, Error>;

    /// Note: This method is called in parallel.
    fn load_non_circular_dep(
        &self,
        base: Arc<PathBuf>,
        import: &DepInfo,
    ) -> Result<ModuleInfo, Error>;
}

impl Load for Checker {
    fn is_in_same_circular_group(&self, base: &Arc<PathBuf>, src: &JsWord) -> bool {
        let id = self.module_graph.id(&base);

        let path = self.module_graph.resolve(&base, src).unwrap();
        let target = self.module_graph.id(&path);

        let circular_set = self.module_graph.get_circular(id);

        match circular_set {
            Some(set) => set.contains(&target),
            None => false,
        }
    }

    fn load_circular_dep(
        &self,
        base: Arc<PathBuf>,
        partial: &ModuleTypeData,
        import: &DepInfo,
    ) -> Result<ModuleInfo, Error> {
        let base_id = self.module_graph.id(&base);
        let path = self.module_graph.resolve(&base, &import.src).unwrap();
        let id = self.module_graph.id(&path);

        let data = self.analyze_module(Some(base.clone()), path.clone());

        return Ok(ModuleInfo {
            module_id: id,
            data,
        });
    }

    fn load_non_circular_dep(
        &self,
        base: Arc<PathBuf>,
        import: &DepInfo,
    ) -> Result<ModuleInfo, Error> {
        let mut result = ModuleTypeData::default();

        // TODO: Use ModuleId for analyze_module
        let path = self.module_graph.resolve(&base, &import.src).unwrap();
        slog::info!(
            self.logger,
            "({}): Loading {}",
            base.display(),
            path.display()
        );
        let id = self.module_graph.id(&path);

        let data = self.analyze_module(Some(base.clone()), path.clone());

        return Ok(ModuleInfo {
            module_id: id,
            data,
        });
    }

    fn module_id(&self, base: &Arc<PathBuf>, src: &JsWord) -> ModuleId {
        let path = self.module_graph.resolve(&base, src).unwrap();
        let id = self.module_graph.id(&path);
        id
    }
}

impl<T> Load for Arc<T>
where
    T: ?Sized + Load,
{
    fn is_in_same_circular_group(&self, base: &Arc<PathBuf>, src: &JsWord) -> bool {
        (**self).is_in_same_circular_group(base, src)
    }

    fn load_non_circular_dep(
        &self,
        base: Arc<PathBuf>,
        import: &DepInfo,
    ) -> Result<ModuleInfo, Error> {
        (**self).load_non_circular_dep(base, import)
    }

    fn load_circular_dep(
        &self,
        base: Arc<PathBuf>,
        partial: &ModuleTypeData,
        import: &DepInfo,
    ) -> Result<ModuleInfo, Error> {
        (**self).load_circular_dep(base, partial, import)
    }

    fn module_id(&self, base: &Arc<PathBuf>, src: &JsWord) -> ModuleId {
        (**self).module_id(base, src)
    }
}

impl<T> Load for Box<T>
where
    T: ?Sized + Load,
{
    fn is_in_same_circular_group(&self, base: &Arc<PathBuf>, src: &JsWord) -> bool {
        (**self).is_in_same_circular_group(base, src)
    }

    fn load_circular_dep(
        &self,
        base: Arc<PathBuf>,
        partial: &ModuleTypeData,
        import: &DepInfo,
    ) -> Result<ModuleInfo, Error> {
        (**self).load_circular_dep(base, partial, import)
    }

    fn load_non_circular_dep(
        &self,
        base: Arc<PathBuf>,
        import: &DepInfo,
    ) -> Result<ModuleInfo, Error> {
        (**self).load_non_circular_dep(base, import)
    }

    fn module_id(&self, base: &Arc<PathBuf>, src: &JsWord) -> ModuleId {
        (**self).module_id(base, src)
    }
}
