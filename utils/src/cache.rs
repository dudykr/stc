use std::borrow::Cow;
use swc_common::TypeEq;

#[derive(Debug)]
pub struct CacheMap<K, V>
where
    K: TypeEq,
    V: Freeze,
{
    data: Vec<(K, V)>,
}

impl<K, V> Default for CacheMap<K, V>
where
    K: TypeEq,
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
    K: TypeEq,
    V: Freeze,
{
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

impl<T> Freeze for Vec<T>
where
    T: Freeze,
{
    fn make_clone_cheap(&mut self) {
        self.iter_mut().for_each(|v| v.make_clone_cheap())
    }
}

impl<T> Freeze for Box<T>
where
    T: Freeze,
{
    fn make_clone_cheap(&mut self) {
        (**self).make_clone_cheap()
    }
}

/// TODO(kdy1): This can be confusing.
impl<T> Freeze for Cow<'_, T>
where
    T: Clone + Freeze,
{
    fn make_clone_cheap(&mut self) {
        match self {
            Cow::Borrowed(_) => {}
            Cow::Owned(v) => {
                v.make_clone_cheap();
            }
        }
    }
}

#[macro_export]
macro_rules! try_cache {
    ($cache:expr, $key:expr, $default_op:expr) => {{
        let key = $key;

        let cached = $cache.get(&key);

        if let Some(v) = cached {
            v
        } else {
            let v: Result<_, _> = (|| $default_op)();
            let v = match v {
                Ok(v) => v,
                Err(err) => return Err(err),
            };
            let v = $cache.insert(key, v);

            v
        }
    }};
}
