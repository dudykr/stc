#![feature(specialization)]
#![allow(incomplete_features)]

use swc_common::TypeEq;

pub mod type_param;

#[derive(Debug, Clone, Copy, Default, PartialEq, TypeEq)]
pub struct ExpandGenericOpts {}
