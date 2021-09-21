use stc_ts_ast_rnode::RTsEntityName;
use stc_ts_types::{Id, Type, TypeParamInstantiation};
use stc_utils::cache::CacheMap;

#[derive(Debug, Default)]
pub struct TypeCache {
    pub expand_cache: CacheMap<(RTsEntityName, Option<TypeParamInstantiation>), Type>,

    /// Key should be [Type::Arc] of [Type::TypeLit].
    pub keyof_type_lit: CacheMap<Type, Type>,
}

impl TypeCache {
    pub fn remove(&mut self, _key: &Id) {}
}
