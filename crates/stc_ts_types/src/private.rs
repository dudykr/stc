use serde::{Deserialize, Serialize};
use swc_common::{EqIgnoreSpan, TypeEq};
use tracing::info;

/// A type used to track all type creations. You can construct this type using
/// `Default::default()` and it will print a log message.
#[derive(Debug, Clone, Copy, PartialEq, Serialize, Deserialize)]
pub(crate) struct Tracker<const N: &'static str> {
    _priv: (),
}

impl<const N: &'static str> EqIgnoreSpan for Tracker<N> {
    #[inline(always)]
    fn eq_ignore_span(&self, _: &Self) -> bool {
        true
    }
}

impl<const N: &'static str> TypeEq for Tracker<N> {
    #[inline(always)]
    fn type_eq(&self, _: &Self) -> bool {
        true
    }
}

impl<const N: &'static str> Default for Tracker<N> {
    #[cfg_attr(debug_assertions, inline(never))]
    #[cfg_attr(not(debug_assertions), inline(always))]
    fn default() -> Self {
        #[cfg(debug_assertions)]
        info!("Creating `{}`", N);

        Self { _priv: () }
    }
}
