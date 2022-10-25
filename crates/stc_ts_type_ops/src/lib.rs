//! Operations related to types.
//!
//! This crate exists to reduce compile time.
#![feature(box_syntax)]
#![feature(box_patterns)]
#![feature(specialization)]
#![allow(incomplete_features)]

use rnode::{VisitMut, VisitMutWith};
use stc_ts_ast_rnode::RIdent;
pub use stc_ts_base_type_ops::{fix::Fix, is_str_lit_or_union};
use stc_ts_types::Type;

pub mod expansion;
pub mod generalization;
pub mod metadata;
pub mod this;
pub mod tuple_normalization;
pub mod tuple_to_array;
pub mod union_finder;
pub mod union_normalization;
pub mod widen;

pub struct PreventComplexSimplification;

impl VisitMut<Type> for PreventComplexSimplification {
    fn visit_mut(&mut self, ty: &mut Type) {
        // TODO(kdy1): PERF
        ty.nm();
        ty.metadata_mut().prevent_complex_simplification = true;

        ty.visit_mut_children_with(self);
    }
}

/// Prevent interop with hygiene.
impl VisitMut<RIdent> for PreventComplexSimplification {
    fn visit_mut(&mut self, _: &mut RIdent) {}
}
