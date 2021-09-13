use stc_ts_types::{Id, Type};
use stc_utils::cache::CacheMap;

#[derive(Debug, Default)]
pub struct TypeCache {
    pub normalize: CacheMap<Type, Type>,
    pub keyof: CacheMap<Type, Type>,
}

impl TypeCache {
    pub fn remove(&mut self, _key: &Id) {}
}
