#![feature(box_patterns)]
#![feature(box_syntax)]
#![feature(specialization)]
#![allow(incomplete_features)]
#![allow(unused_variables)]
#![allow(dead_code)]
#![allow(clippy::needless_update)]

use swc_common::TypeEq;

pub mod expander;
pub mod type_param;

#[derive(Debug, Clone, Copy, Default, PartialEq, Eq, TypeEq)]
pub struct ExpandGenericOpts {
    //// If `true`, we will not expand values.
    pub ignore_values: bool,
}
