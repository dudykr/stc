use stc_ts_types::module_id::ModuleIdGenerator;

#[derive(Default)]
pub struct ModuleStore {
    ids: ModuleIdGenerator,
}
