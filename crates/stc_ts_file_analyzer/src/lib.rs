//! Main analyzer.
//!
//! Codes splitted over multiple crates to reduce compile time.

#![allow(incomplete_features)]
#![allow(unused_variables)] // temporary
#![allow(clippy::collapsible_if)]
#![allow(clippy::collapsible_else_if)]
#![allow(clippy::needless_update)]
#![allow(clippy::only_used_in_recursion)]
#![deny(unused_must_use)]
#![deny(unreachable_patterns)]
#![deny(irrefutable_let_patterns)]
#![feature(box_patterns)]
#![feature(try_blocks)]
#![feature(specialization)]
#![recursion_limit = "1024"]

use stc_ts_env::StableEnv;
use stc_ts_errors::Error;
use stc_ts_file_analyzer_macros::validator;
pub use stc_ts_types::{Id, ModuleTypeData};
use swc_atoms::JsWord;
use swc_common::Span;

pub mod analyzer;
pub mod env;
pub mod loader;
#[cfg(test)]
mod tests;
pub mod ty;
mod type_facts;
pub mod util;
pub mod validator;

/// Validation result
pub type VResult<T> = Result<T, Error>;

#[derive(Debug, PartialEq, Eq)]
pub struct DepInfo {
    pub span: Span,
    pub src: JsWord,
}

#[derive(Debug, PartialEq, Eq)]
pub struct Specifier {
    pub local: Id,
    pub export: Id,
}

#[derive(Debug)]
pub struct Config {
    pub env: StableEnv,
}
