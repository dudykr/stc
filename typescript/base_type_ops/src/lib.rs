//! Shared operations on TypeScript types.
//!
//! This crate is used to reduce compile time.

use stc_ts_types::TypeElement;
use swc_ecma_ast::TruePlusMinus;

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
