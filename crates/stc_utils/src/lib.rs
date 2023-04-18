#![feature(never_type)]

/// Use good memory allocator.
extern crate swc_node_base;

use std::{
    collections::{HashMap, HashSet},
    env, fmt,
    mem::take,
    ops::{Deref, DerefMut},
    str::FromStr,
};

use once_cell::sync::Lazy;
use swc_common::SyntaxContext;

pub mod cache;
pub mod error;
pub mod ext;
pub mod panic_context;
pub mod stack;

pub type ABuilderHasher = ahash::RandomState;

pub type AHashMap<K, V> = HashMap<K, V, ahash::RandomState>;

pub type AHashSet<V> = HashSet<V, ahash::RandomState>;

/// Syntax context for builtin modules.
pub const BUILTIN_CTXT: SyntaxContext = SyntaxContext::empty();

/// If true, errors will not be buffered.
pub fn early_error() -> bool {
    static EARLY_ERROR: Lazy<bool> = Lazy::new(|| env::var("STC_EARLY_ERROR").map(|s| s == "1").unwrap_or(false));

    *EARLY_ERROR
}

pub trait TryOpt<T>: Sized + Into<Option<T>> {
    fn try_map<F, U, E>(self, op: F) -> Result<Option<U>, E>
    where
        Self: Into<Option<T>>,
        F: FnOnce(T) -> Result<U, E>,
    {
        match self.into() {
            Some(v) => match op(v) {
                Ok(v) => Ok(Some(v)),
                Err(err) => Err(err),
            },
            None => Ok(None),
        }
    }
}

impl<T> TryOpt<T> for Option<T> {}

pub fn dedup<T>(v: &mut Vec<T>)
where
    T: Eq,
{
    let mut remove_list = vec![];

    for (i, i1) in v.iter().enumerate() {
        for (j, j1) in v.iter().enumerate() {
            if i < j && i1 == j1 {
                remove_list.push(j);
            }
        }
    }

    // Fast path. We don't face real duplicates in most cases.
    if remove_list.is_empty() {
        return;
    }

    let new = take(v)
        .into_iter()
        .enumerate()
        .filter_map(|(idx, value)| if remove_list.contains(&idx) { None } else { Some(value) })
        .collect::<Vec<_>>();

    *v = new;
}

/// A newtype wrapper that causes the field within to be ignored while printing
/// out `Debug` output.
///
/// For more, see the [crate documentation](self).
#[derive(Copy, Clone, Default, Eq, Hash, Ord, PartialEq, PartialOrd)]
#[repr(transparent)]
pub struct DebugIgnore<T: ?Sized>(pub T);

/// The point of this struct.
impl<T: ?Sized> fmt::Debug for DebugIgnore<T> {
    #[inline]
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "...")
    }
}

// ---
// Other trait impls
// ---

impl<T> From<T> for DebugIgnore<T> {
    #[inline]
    fn from(t: T) -> Self {
        Self(t)
    }
}

impl<T: ?Sized> Deref for DebugIgnore<T> {
    type Target = T;

    #[inline]
    fn deref(&self) -> &Self::Target {
        &self.0
    }
}

impl<T: ?Sized> DerefMut for DebugIgnore<T> {
    #[inline]
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.0
    }
}

impl<T: FromStr> FromStr for DebugIgnore<T> {
    type Err = T::Err;

    #[inline]
    fn from_str(s: &str) -> Result<Self, Self::Err> {
        s.parse().map(DebugIgnore)
    }
}

impl<T: ?Sized + fmt::Display> fmt::Display for DebugIgnore<T> {
    #[inline]
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        self.0.fmt(f)
    }
}

impl<T: ?Sized, Q: ?Sized> AsRef<Q> for DebugIgnore<T>
where
    T: AsRef<Q>,
{
    #[inline]
    fn as_ref(&self) -> &Q {
        self.0.as_ref()
    }
}
