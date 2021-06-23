use fxhash::FxHashMap;
use parking_lot::Mutex;
use stc_visit::Visit;
use std::{path::PathBuf, sync::Arc};
use swc_common::{EqIgnoreSpan, TypeEq};

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, EqIgnoreSpan, TypeEq, Visit)]
pub struct ModuleId(u32);

impl ModuleId {
    pub const fn builtin() -> Self {
        ModuleId(0)
    }

    pub fn is_builtin(self) -> bool {
        self.0 == 0
    }
}

#[derive(Clone, Default)]
pub struct Generator {
    cache: Arc<Mutex<(u32, FxHashMap<Arc<PathBuf>, ModuleId>)>>,
}

impl Generator {
    /// Returns `(true, id)` if it's generated, and returns `(false, id)` if
    /// it's found from cache.
    pub fn generate(&self, path: &Arc<PathBuf>) -> (bool, ModuleId) {
        let mut cache = self.cache.lock();
        if let Some(v) = cache.1.get(path) {
            return (false, *v);
        }
        cache.0 += 1;
        let module_id = ModuleId(cache.0);
        let res = cache.1.insert(path.clone(), module_id);
        debug_assert_eq!(res, None, "Found multiple module id for one file");
        (true, module_id)
    }
}
