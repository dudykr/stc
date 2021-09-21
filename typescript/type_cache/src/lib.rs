use crate::{cache_map::CacheMap, cache_mode::CacheMode};
use stc_ts_types::{Id, Type};

pub mod cache_map;
pub mod cache_mode;

/// TODO: pub expand_cache: CacheMap<(RTsEntityName,
/// Option<TypeParamInstantiation>), Type, RevokeOnTypeDecl>,
#[derive(Debug, Default)]
pub struct TypeCache {
    /// Key should be [Type::Arc] of [Type::Mapped].
    pub expand_mapped: CacheMap<Type, Type, NoRefInKey>,

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
pub struct NoRefInKey {}

impl CacheMode for NoRefInKey {}
