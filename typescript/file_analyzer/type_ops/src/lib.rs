//! Operations related to types.
//!
//! This crate exists to reduce compile time.
#![feature(specialization)]
#![allow(incomplete_features)]

pub use self::fix::Fix;
use stc_ts_ast_rnode::RTsLit;
use stc_ts_types::{LitType, Type, Union};

mod fix;
mod generalization;
pub mod metadata;

pub fn is_str_lit_or_union(t: &Type) -> bool {
    match t {
        Type::Lit(LitType {
            lit: RTsLit::Str(..), ..
        }) => true,
        Type::Union(Union { ref types, .. }) => types.iter().all(|ty| is_str_lit_or_union(&ty)),
        _ => false,
    }
}
