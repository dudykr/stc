#![feature(never_type)]

/// Use good memory allocator.
extern crate swc_node_base;

use std::{
    collections::{HashMap, HashSet},
    env,
    fmt::Debug,
    ops::{Deref, DerefMut},
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

#[repr(transparent)]
#[derive(Default, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct DebugIgnore<T>(pub T);

impl<T> Deref for DebugIgnore<T> {
    type Target = T;

    fn deref(&self) -> &Self::Target {
        &self.0
    }
}

impl<T> DerefMut for DebugIgnore<T> {
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.0
    }
}

impl<T> Debug for DebugIgnore<T> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_tuple("<unknown>").finish()
    }
}
