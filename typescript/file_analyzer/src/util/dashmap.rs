use dashmap::{DashMap, SharedValue};
use std::hash::BuildHasher;
use std::hash::Hash;

pub(crate) trait DashMapExt<'a, K, V, H>
where
    K: 'a,
    V: 'a,
{
    fn try_insert_default(&self, k: K)
    where
        V: Default;

    fn try_get<F, Ret>(&self, k: &K, op: F) -> Option<Ret>
    where
        F: FnOnce(&V) -> Option<Ret>;

    fn try_contains_key(&self, k: &K) -> bool {
        self.try_get(k, |_| Some(())).is_some()
    }
}

impl<'a, K, V, T, H> DashMapExt<'a, K, V, H> for T
where
    T: AsRef<DashMap<K, V, H>>,
    K: 'a + Eq + Hash,
    V: 'a,
    H: 'a + BuildHasher + Clone,
{
    fn try_get<F, Ret>(&self, k: &K, op: F) -> Option<Ret>
    where
        F: FnOnce(&V) -> Option<Ret>,
    {
        let map = self.as_ref();
        let shards = map.shards();
        let idx = map.determine_map(k);

        let lock = shards[idx].try_read()?;
        let v = lock.get(k)?;

        op(v.get())
    }

    fn try_insert_default(&self, k: K)
    where
        V: Default,
    {
        let map = self.as_ref();
        let shards = map.shards();
        let idx = map.determine_map(&k);

        let lock = shards[idx].try_write();
        let mut lock = match lock {
            Some(v) => v,
            None => return,
        };
        lock.entry(k).or_insert_with(|| SharedValue::new(Default::default()));
    }
}
