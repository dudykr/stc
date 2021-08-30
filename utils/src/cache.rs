#[derive(Debug)]
pub struct CacheMap<K, V>
where
    K: PartialEq,
    V: Freeze,
{
    data: Vec<(K, V)>,
}

impl<K, V> Default for CacheMap<K, V>
where
    K: PartialEq,
    V: Freeze,
{
    fn default() -> Self {
        Self {
            data: Default::default(),
        }
    }
}

impl<K, V> CacheMap<K, V>
where
    K: PartialEq,
    V: Freeze,
{
    pub fn get(&self, key: &K) -> Option<V> {
        for (k, v) in &self.data {
            if *k == *key {
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

pub trait Freeze: Sized + Clone {
    fn make_clone_cheap(&mut self);
}

impl<T> Freeze for Option<T>
where
    T: Freeze,
{
    fn make_clone_cheap(&mut self) {
        match self {
            Some(v) => v.make_clone_cheap(),
            None => {}
        }
    }
}

#[macro_export]
macro_rules! try_cache {
    ($cache:expr, $key:expr, $default_op:expr) => {{
        let key = $key;

        let cached = $cache.get(&key);

        if let Some(v) = cached {
            Ok(v)
        } else {
            let v = $default_op;
            let v = match v {
                Ok(v) => v,
                Err(err) => return Err(err),
            };
            let v = $cache.insert(key, v);

            Ok(v)
        }
    }};
}
