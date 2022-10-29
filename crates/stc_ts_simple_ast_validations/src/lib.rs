//! Crate for pure ast validation logics.
//!
//! Used to reduce compile time.

#![feature(box_syntax)]
#![feature(box_patterns)]
#![feature(specialization)]
#![allow(incomplete_features)]

pub mod ambient_fn;
pub mod consturctor;
pub mod yield_finder;
pub mod yield_check;