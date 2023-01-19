use std::cell::RefCell;

use rustc_hash::FxHashMap;
use scoped_tls::ScopedKey;

#[cfg(test)]
mod tests;

pub struct CacheData<T>(RefCell<FxHashMap<*const (), T>>);

impl<T> Default for CacheData<T> {
    fn default() -> Self {
        Self(Default::default())
    }
}

#[macro_export]
macro_rules! visit_cache {
    ($(#[$attrs:meta])* $vis:vis static $name:ident: $ty:ty) => (


        $(#[$attrs])*
        $vis static $name: $crate::visit_cache::VisitCache<$ty> = $crate::visit_cache::VisitCache {
            inner: {
                $crate::scoped_tls::scoped_thread_local!(static CACHE: $crate::visit_cache::CacheData<$ty>);

                &CACHE
            },
        };
    )
}

/// A cache for `Visit`.
///
/// This prefers the first call.
///
///
/// # Implementation details
///
/// This is inspired by `scoped_tls` and `tracing::subscriber::set_default()`.
pub struct VisitCache<T>
where
    T: 'static,
{
    #[doc(hidden)]
    pub(super) inner: &'static ScopedKey<CacheData<T>>,
}

impl<T> VisitCache<T>
where
    T: 'static,
{
    /// Runs `op` with the cache configured.
    ///
    /// If the cache is already configured, this does nothing.
    #[inline]
    pub fn configure<Ret>(&'static self, op: impl FnOnce() -> Ret) -> Ret {
        if self.inner.is_set() {
            op()
        } else {
            self.inner.set(&Default::default(), op)
        }
    }

    /// Insert a value into the cache. Noop if the cache is not configured.
    #[inline]
    pub fn insert(&'static self, key: *const (), value: T) {
        if !self.inner.is_set() {
            return;
        }

        self.inner.with(|cache| {
            cache.0.borrow_mut().insert(key, value);
        })
    }

    #[inline]
    pub fn get_copied(&'static self, key: *const ()) -> Option<T>
    where
        T: Copy,
    {
        if !self.inner.is_set() {
            return None;
        }

        self.inner.with(|cache| cache.0.borrow().get(&key).copied())
    }

    #[inline]
    pub fn get<F, Ret>(&'static self, key: *const (), op: F) -> Option<Ret>
    where
        F: FnOnce(&T) -> Ret,
    {
        if !self.inner.is_set() {
            return None;
        }

        self.inner.with(|cache| {
            let b = cache.0.borrow();
            let cached = b.get(&key);

            cached.map(op)
        })
    }

    #[inline(always)]
    pub fn is_set(&'static self) -> bool {
        self.inner.is_set()
    }
}
