use std::marker::PhantomData;

use stc_utils::cache::Freeze;
use swc_common::TypeEq;

use crate::cache_mode::CacheMode;

#[derive(Debug)]
pub struct CacheMap<K, V, M>
where
    K: TypeEq,
    V: Freeze,
    M: CacheMode<K>,
{
    data: Vec<(K, V)>,
    _marker: PhantomData<M>,
}

impl<K, V, M> Default for CacheMap<K, V, M>
where
    K: TypeEq,
    V: Freeze,
    M: CacheMode<K>,
{
    fn default() -> Self {
        Self {
            data: Default::default(),
            _marker: Default::default(),
        }
    }
}

impl<K, V, M> CacheMap<K, V, M>
where
    K: TypeEq,
    V: Freeze,
    M: CacheMode<K>,
{
    #[inline]
    pub fn can_cache(&self, key: &K) -> bool {
        M::can_cache(key)
    }

    pub fn get(&self, key: &K) -> Option<V> {
        for (k, v) in &self.data {
            if k.type_eq(key) {
                return Some(v.clone());
            }
        }

        None
    }

    /// Returns the inserted value.
    pub fn insert(&mut self, key: K, mut value: V) -> V {
        value.make_clone_cheap();

        self.data.push((key, value.clone()));

        value
    }
}
