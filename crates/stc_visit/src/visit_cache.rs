use std::{
    cell::{Cell, RefCell},
    thread::LocalKey,
};

use rustc_hash::FxHashMap;

thread_local! {
    static CURRENT_STATE: State = State {
        default: RefCell::new(None),
    };
}

struct State {
    default: RefCell<Option<Dispatch>>,
}

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
    data: &'static LocalKey<Cell<FxHashMap<*const (), T>>>,
}

impl<T> VisitCache<T>
where
    T: 'static,
{
    pub fn configure(&'static self) -> Option<VisitCacheGuard> {}

    pub fn current(&'static self) -> Option<Self> {}
}

/// Cleans up the cache on drop.
pub struct VisitCacheGuard<'a, T>
where
    T: 'static,
{
    data: &'a VisitCache<T>,
}
