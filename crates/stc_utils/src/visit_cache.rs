use std::thread::LocalKey;

use rustc_hash::FxHashMap;

/// A cache for `Visit`.
///
/// This prefers the first call.
///
///
/// # Implementation details
///
/// This is inspired by `scoped_tls` and `tracing::subscriber::set_default()`.
#[derive(Debug)]
pub struct VisitCache<T>
where
    T: 'static,
{
    data: &'static LocalKey<FxHashMap<*const (), T>>,
}

impl<T> VisitCache<T>
where
    T: 'static,
{
    pub fn current(&'static self) -> Option<Self> {}
}

/// Cleans up the cache on drop.
pub struct VisitCacheGuard<'a, T>
where
    T: 'static,
{
    data: &'a VisitCache<T>,
}
