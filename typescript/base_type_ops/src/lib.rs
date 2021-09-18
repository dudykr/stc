//! Shared operations on TypeScript types.
//!
//! This crate is used to reduce compile time.
#![feature(box_patterns)]
#![feature(box_syntax)]
#![feature(specialization)]
#![allow(incomplete_features)]

use stc_ts_ast_rnode::RTsLit;
use stc_ts_types::{LitType, Type, TypeElement, Union};
use swc_ecma_ast::TruePlusMinus;

pub mod fix;

pub fn apply_mapped_flags(el: &mut TypeElement, optional: Option<TruePlusMinus>, readonly: Option<TruePlusMinus>) {
    match optional {
        Some(v) => match el {
            TypeElement::Call(_) => {}
            TypeElement::Constructor(_) => {}
            TypeElement::Property(p) => match v {
                TruePlusMinus::True => {
                    p.optional = true;
                }
                TruePlusMinus::Plus => {
                    p.optional = true;
                }
                TruePlusMinus::Minus => {
                    p.optional = false;
                }
            },
            TypeElement::Method(m) => match v {
                TruePlusMinus::True => {
                    m.optional = true;
                }
                TruePlusMinus::Plus => {
                    m.optional = true;
                }
                TruePlusMinus::Minus => {
                    m.optional = false;
                }
            },
            TypeElement::Index(_) => {}
        },
        None => {}
    }

    match readonly {
        Some(v) => match el {
            TypeElement::Call(_) => {}
            TypeElement::Constructor(_) => {}
            TypeElement::Property(p) => match v {
                TruePlusMinus::True => {
                    p.readonly = true;
                }
                TruePlusMinus::Plus => {
                    p.readonly = true;
                }
                TruePlusMinus::Minus => {
                    p.readonly = false;
                }
            },
            TypeElement::Index(_) => {}
            TypeElement::Method(m) => match v {
                TruePlusMinus::True => {
                    m.readonly = true;
                }
                TruePlusMinus::Plus => {
                    m.readonly = true;
                }
                TruePlusMinus::Minus => {
                    m.readonly = false;
                }
            },
        },
        None => {}
    }
}

pub fn is_str_lit_or_union(t: &Type) -> bool {
    match t {
        Type::Lit(LitType {
            lit: RTsLit::Str(..), ..
        }) => true,
        Type::Union(Union { ref types, .. }) => types.iter().all(|ty| is_str_lit_or_union(&ty)),
        _ => false,
    }
}
