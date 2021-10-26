use crate::key::CacheKey;

pub trait CacheMode<K>
where
    K: CacheKey,
{
    fn can_cache(key: &K) -> bool;
}
