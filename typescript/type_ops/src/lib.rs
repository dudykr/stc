//! Operations related to types.
//!
//! This crate exists to reduce compile time.
#![feature(specialization)]
#![allow(incomplete_features)]

pub use stc_ts_base_type_ops::{fix::Fix, is_str_lit_or_union};

pub mod generalization;
pub mod metadata;
