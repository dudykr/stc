//! Main analyzer.
//!
//! Codes splitted over multiple crates to reduce compile time.

#![allow(incomplete_features)]
#![allow(unused_variables)] // temporary
#![deny(unused_must_use)]
#![deny(unreachable_patterns)]
#![deny(unused_imports)]
#![deny(irrefutable_let_patterns)]
#![feature(box_patterns)]
#![feature(box_syntax)]
#![feature(try_blocks)]
#![feature(specialization)]
#![recursion_limit = "1024"]

use stc_ts_env::StableEnv;
use stc_ts_errors::Error;
use stc_ts_file_analyzer_macros::validator;
use stc_ts_types::Type;
pub use stc_ts_types::{Id, ModuleTypeData};
use std::path::PathBuf;
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

pub type ValidationResult<T = Type> = Result<T, Error>;

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
    /// Should we generate .d.ts?
    declaration: bool,
    /// Directory to store .d.ts files.
    declaration_dir: PathBuf,

    pub env: StableEnv,
}
