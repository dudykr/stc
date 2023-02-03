use std::sync::{Arc, RwLock};

use rustc_hash::FxHashMap;
use serde::{Deserialize, Serialize};
use stc_visit::Visit;
use swc_common::{EqIgnoreSpan, FileName, Mark, TypeEq};

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, EqIgnoreSpan, TypeEq, Visit, Serialize, Deserialize)]
pub struct ModuleId(u32);

impl ModuleId {
    pub const fn builtin() -> Self {
        ModuleId(0)
    }

    pub fn is_builtin(self) -> bool {
        self.0 == 0
    }
}

/// Each module has a unique id and [Mark] for top-level items

#[derive(Default)]
pub struct ModuleIdGenerator {
    cache: RwLock<Data>,
}

#[derive(Default)]
struct Data {
    cur: u32,
    modules: FxHashMap<Arc<FileName>, (ModuleId, Mark)>,
    paths: FxHashMap<ModuleId, (Arc<FileName>, Mark)>,
}

impl ModuleIdGenerator {
    /// Returns `(module_id, top_level_mark)`
    pub fn generate(&self, path: &Arc<FileName>) -> (ModuleId, Mark) {
        let mut data = self.cache.write().unwrap();
        if let Some(v) = data.modules.get(path) {
            return *v;
        }

        data.cur += 1;

        let top_level_mark = Mark::new();

        let module_id = ModuleId(data.cur);
        let res = data.modules.insert(path.clone(), (module_id, top_level_mark));
        data.paths.insert(module_id, (path.clone(), top_level_mark));

        debug_assert_eq!(res, None, "Found multiple module id for one file");

        (module_id, top_level_mark)
    }

    pub fn path(&self, module_id: ModuleId) -> Arc<FileName> {
        self.cache.read().unwrap().paths.get(&module_id).cloned().unwrap().0
    }

    pub fn top_level_mark(&self, module_id: ModuleId) -> Mark {
        self.cache.read().unwrap().paths.get(&module_id).cloned().unwrap().1
    }
}
