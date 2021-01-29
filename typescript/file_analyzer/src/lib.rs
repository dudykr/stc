#![allow(incomplete_features)]
#![allow(unused_variables)] // temporary
#![allow(unused_mut)] // temporary
#![allow(dead_code)] // temporary
#![deny(unused_must_use)]
#![deny(unreachable_patterns)]
#![deny(mutable_borrow_reservation_conflict)]
#![deny(irrefutable_let_patterns)]
#![feature(box_patterns)]
#![feature(box_syntax)]
#![feature(try_blocks)]
#![feature(specialization)]
#![feature(vec_remove_item)]
#![feature(option_expect_none)]
#![feature(option_unwrap_none)]
#![recursion_limit = "1024"]

pub use self::analyzer::Marks;
use self::env::StableEnv;
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

pub type ValidationResult<T = Box<Type>> = Result<T, Box<Error>>;

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

#[derive(Debug, Clone, Copy, Default)]
pub struct Rule {
    pub no_implicit_any: bool,
    pub no_implicit_this: bool,
    pub always_strict: bool,
    pub strict_null_checks: bool,
    pub strict_function_types: bool,

    pub allow_unreachable_code: bool,
    pub allow_unused_labels: bool,
    pub no_fallthrough_cases_in_switch: bool,
    pub no_implicit_returns: bool,
    pub suppress_excess_property_errors: bool,
    pub suppress_implicit_any_index_errors: bool,
    pub no_strict_generic_checks: bool,
    pub no_unused_locals: bool,
    pub no_unused_parameters: bool,
}
