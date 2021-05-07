use once_cell::sync::Lazy;
use std::collections::HashMap;
use std::collections::HashSet;
use std::env;

pub mod error;
pub mod ext;
pub mod panic_context;
pub mod stack;

pub type FastHashMap<K, V> = HashMap<K, V, ahash::RandomState>;

pub type FastHashSet<V> = HashSet<V, ahash::RandomState>;

#[cfg(all(unix, not(target_env = "musl")))]
#[global_allocator]
static ALLOC: jemallocator::Jemalloc = jemallocator::Jemalloc;

#[cfg(windows)]
#[global_allocator]
static ALLOC: mimalloc::MiMalloc = mimalloc::MiMalloc;

/// If true, errors will not be buffered.
pub fn early_error() -> bool {
    static EARLY_ERROR: Lazy<bool> = Lazy::new(|| env::var("STC_EARLY_ERROR").map(|s| s == "1").unwrap_or(false));

    *EARLY_ERROR
}

pub trait TryOpt<T, E>: Sized + Into<Option<Result<T, E>>> {
    fn try_opt(self) -> Result<Option<T>, E> {
        match self.into() {
            Some(res) => Ok(Some(res?)),
            None => Ok(None),
        }
    }
}

impl<T, E> TryOpt<T, E> for Option<Result<T, E>> {}
