#![allow(incomplete_features)]
#![feature(specialization)]

use crate::{cache_map::CacheMap, cache_mode::CacheMode, key::CacheKey};
use stc_ts_types::{Id, Mapped, Ref, Type};
use stc_visit::{Visit, VisitWith};

pub mod cache_map;
pub mod cache_mode;
pub mod key;

/// TODO: pub expand_cache: CacheMap<(RTsEntityName,
/// Option<TypeParamInstantiation>), Type, RevokeOnTypeDecl>,
#[derive(Debug, Default)]
pub struct TypeCache {
    pub expand_mapped: CacheMap<Mapped, Option<Type>, NoRefInKey>,

    /// Key should be [Type::Arc] of [Type::TypeLit].
    pub keyof_type_lit: CacheMap<Type, Type, NoRevoke>,
}

impl TypeCache {
    pub fn remove(&mut self, _key: &Id) {}
}

#[derive(Debug)]
pub struct NoRevoke {}

impl<K> CacheMode<K> for NoRevoke
where
    K: CacheKey,
{
    #[inline(always)]
    fn can_cache(_: &K) -> bool {
        true
    }
}

#[derive(Debug)]
pub struct NoRefInKey {}

impl<K> CacheMode<K> for NoRefInKey
where
    K: CacheKey + VisitWith<RefFinder>,
{
    fn can_cache(key: &K) -> bool {
        let mut v = RefFinder { found: false };
        key.visit_with(&mut v);
        !v.found
    }
}

pub struct RefFinder {
    found: bool,
}

impl Visit<Ref> for RefFinder {
    fn visit(&mut self, _: &Ref) {
        self.found = true;
    }
}
