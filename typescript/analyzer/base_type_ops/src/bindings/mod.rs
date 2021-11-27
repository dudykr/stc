//! Module to precomput known bindings.

use rnode::{Visit, VisitWith};
use stc_ts_ast_rnode::*;
use stc_ts_types::Id;
use swc_common::{collections::AHashMap, EqIgnoreSpan, TypeEq};
use swc_ecma_ast::VarDeclKind;

#[derive(Clone, Copy, Eq, PartialEq, PartialOrd, Ord, Hash, EqIgnoreSpan, TypeEq)]
pub enum BindingKind {
    Import,
    Class,
    Function,
    Enum,
    Inteface,
    Namespace,
    Module,
    Var(#[use_eq_ignore_span] VarDeclKind),
}

pub fn collect_bindings<N>(n: &N) -> AHashMap<Id, Vec<BindingKind>>
where
    N: for<'aa> VisitWith<BindingCollector<'aa>>,
{
    let mut data = AHashMap::default();

    n.visit_with(&mut BindingCollector { data: &mut data });

    data
}

pub struct BindingCollector<'a> {
    data: &'a mut AHashMap<Id, Vec<BindingKind>>,
}

impl Visit<RTsNamespaceDecl> for BindingCollector<'_> {
    fn visit(&mut self, decl: &RTsNamespaceDecl) {
        self.data
            .entry(decl.id.clone().into())
            .or_default()
            .push(BindingKind::Namespace);

        decl.visit_children_with(self);
    }
}

impl Visit<RTsInterfaceDecl> for BindingCollector<'_> {
    fn visit(&mut self, decl: &RTsInterfaceDecl) {
        self.data
            .entry(decl.id.clone().into())
            .or_default()
            .push(BindingKind::Inteface);

        decl.visit_children_with(self);
    }
}

impl Visit<RClassDecl> for BindingCollector<'_> {
    fn visit(&mut self, decl: &RClassDecl) {
        self.data
            .entry(decl.ident.clone().into())
            .or_default()
            .push(BindingKind::Class);

        decl.visit_children_with(self);
    }
}

impl Visit<RFnDecl> for BindingCollector<'_> {
    fn visit(&mut self, decl: &RFnDecl) {
        self.data
            .entry(decl.ident.clone().into())
            .or_default()
            .push(BindingKind::Function);

        decl.visit_children_with(self);
    }
}

impl Visit<RTsEnumDecl> for BindingCollector<'_> {
    fn visit(&mut self, decl: &RTsEnumDecl) {
        self.data
            .entry(decl.id.clone().into())
            .or_default()
            .push(BindingKind::Enum);

        decl.visit_children_with(self);
    }
}
