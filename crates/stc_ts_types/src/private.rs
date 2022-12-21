use serde::{Deserialize, Serialize};
use swc_common::{EqIgnoreSpan, TypeEq};
use tracing::info;

/// A type used to make types private, so we can track all type creation.
#[derive(Debug, Clone, Copy, PartialEq, Serialize, Deserialize)]
pub(crate) struct Private<const N: &'static str> {
    _priv: (),
}

impl<const N: &'static str> EqIgnoreSpan for Private<N> {
    #[inline(always)]
    fn eq_ignore_span(&self, _: &Self) -> bool {
        true
    }
}

impl<const N: &'static str> TypeEq for Private<N> {
    #[inline(always)]
    fn type_eq(&self, _: &Self) -> bool {
        true
    }
}

impl<const N: &'static str> Default for Private<N> {
    #[cfg_attr(debug_assertions, inline(never))]
    #[cfg_attr(not(debug_assertions), inline(always))]
    fn default() -> Self {
        #[cfg(debug_assertions)]
        info!("Creating `{}`", N);

        Self { _priv: () }
    }
}
