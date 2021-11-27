//! Module to precomput known bindings.

use stc_ts_types::Id;
use swc_common::{collections::AHashMap, EqIgnoreSpan, TypeEq};
use swc_ecma_ast::VarDeclKind;

#[derive(Clone, Copy, Eq, PartialEq, PartialOrd, Ord, Hash, EqIgnoreSpan, TypeEq)]
pub enum BindingKind {
    Import,
    Class,
    Fn,
    Namespace,
    Module,
    Var(#[use_eq_ignore_span] VarDeclKind),
}

pub fn collect_bindings() -> AHashMap<Id, Vec<BindingKind>> {
    todo!()
}
