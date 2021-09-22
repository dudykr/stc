use swc_common::TypeEq;

pub trait CacheKey: TypeEq {}

impl<K> CacheKey for K where K: TypeEq {}
