use std::panic::Location;

use rnode::{FoldWith, VisitMutWith, VisitWith};
use serde::{Deserialize, Serialize};
use stc_visit::Visitable;
use swc_common::{EqIgnoreSpan, TypeEq};
use tracing::info;

/// A type used to track all type creations. You can construct this type using
/// `Default::default()` and it will print a log message.
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Serialize, Deserialize)]
pub struct Tracker<const N: &'static str> {
    _priv: (),
}

impl<const N: &'static str> EqIgnoreSpan for Tracker<N> {
    #[inline]
    fn eq_ignore_span(&self, _: &Self) -> bool {
        true
    }
}

impl<const N: &'static str> TypeEq for Tracker<N> {
    #[inline]
    fn type_eq(&self, _: &Self) -> bool {
        true
    }
}

impl<const N: &'static str> Default for Tracker<N> {
    #[cfg_attr(debug_assertions, inline(never))]
    #[cfg_attr(not(debug_assertions), inline(always))]
    #[track_caller]
    fn default() -> Self {
        #[cfg(debug_assertions)]
        {
            let loc = Location::caller();
            info!("Creating `{}` from {:?}", N, loc);
        }

        Self { _priv: () }
    }
}

impl<const N: &'static str> Visitable for Tracker<N> {}

impl<const N: &'static str, V: ?Sized> VisitWith<V> for Tracker<N> {
    #[inline]
    fn visit_children_with(&self, _: &mut V) {}
}

impl<const N: &'static str, V: ?Sized> VisitMutWith<V> for Tracker<N> {
    #[inline]
    fn visit_mut_children_with(&mut self, _: &mut V) {}
}

impl<const N: &'static str, V: ?Sized> FoldWith<V> for Tracker<N> {
    #[inline]
    fn fold_children_with(self, _: &mut V) -> Self {
        self
    }
}
