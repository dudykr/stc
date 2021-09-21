use stc_ts_types::{Id, Type};
use stc_utils::cache::{mode::CacheMode, CacheMap};

#[derive(Debug, Default)]
pub struct TypeCache {
    // pub expand_cache: CacheMap<(RTsEntityName, Option<TypeParamInstantiation>), Type, RevokeOnTypeDecl>,
    /// Key should be [Type::Arc] of [Type::TypeLit].
    pub keyof_type_lit: CacheMap<Type, Type, NoRevoke>,
}

impl TypeCache {
    pub fn remove(&mut self, _key: &Id) {}
}

#[derive(Debug)]
pub struct NoRevoke {}

impl CacheMode for NoRevoke {}

#[derive(Debug)]
pub struct RevokeOnTypeDecl {}

impl CacheMode for RevokeOnTypeDecl {}
