#![feature(box_patterns)]
#![feature(box_syntax)]
#![feature(specialization)]
#![allow(incomplete_features)]
#![allow(unused_variables)]
#![allow(dead_code)]

use swc_common::TypeEq;

pub mod expander;
pub mod type_param;

#[derive(Debug, Clone, Copy, Default, PartialEq, TypeEq)]
pub struct ExpandGenericOpts {
    //// If `true`, we will not expand values.
    pub process_only_key: bool,
}
