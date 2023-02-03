use std::sync::{Arc, RwLock};

use fxhash::FxHashMap;
use stc_ts_types::ModuleId;
use swc_common::FileName;

#[derive(Default)]
pub struct ModuleStore {
    data: RwLock<Data>,
}

#[derive(Default)]
struct Data {
    filename_to_id: FxHashMap<Arc<FileName>, ModuleId>,
    id_to_filename: FxHashMap<ModuleId, Arc<FileName>>,
}
