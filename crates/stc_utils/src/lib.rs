#![feature(never_type)]

/// Use good memory allocator.
extern crate swc_node_base;

use std::{
    collections::{HashMap, HashSet},
    env,
    mem::take,
};

use once_cell::sync::Lazy;
use swc_common::SyntaxContext;

pub mod cache;
pub mod error;
pub mod ext;
pub mod panic_context;
pub mod stack;
pub mod visit_cache;

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
