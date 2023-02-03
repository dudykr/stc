use std::sync::Arc;

use stc_ts_types::{module_id::ModuleIdGenerator, ModuleId};
use swc_common::{FileName, Mark};

#[derive(Default)]
pub struct ModuleStore {
    ids: ModuleIdGenerator,
}

impl ModuleStore {
    /// Returns `(module id, top level syntax context)`
    pub fn get(&self, path: &Arc<FileName>) -> (ModuleId, Mark) {
        self.ids.generate(path)
    }
}
