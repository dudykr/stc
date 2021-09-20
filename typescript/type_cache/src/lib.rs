use stc_ts_types::{Id, Type, TypeLit};
use stc_utils::cache::CacheMap;

#[derive(Debug, Default)]
pub struct TypeCache {
    pub keyof_type_lit: CacheMap<TypeLit, Type>,
}

impl TypeCache {
    pub fn remove(&mut self, _key: &Id) {}
}
