use rnode::Visit;
use serde::{Deserialize, Serialize};
use swc_common::{EqIgnoreSpan, TypeEq};

/// A type used to make types private, so we can track all type creation.
#[derive(Debug, Default, Clone, Copy, PartialEq, Visit, Serialize, Deserialize)]
pub(crate) struct Private {}

impl EqIgnoreSpan for Private {
    #[inline(always)]
    fn eq_ignore_span(&self, _: &Self) -> bool {
        true
    }
}

impl TypeEq for Private {
    #[inline(always)]
    fn type_eq(&self, _: &Self) -> bool {
        true
    }
}
