//! Module to precomput known bindings.

use rnode::{Visit, VisitWith};
use stc_ts_ast_rnode::*;
use stc_ts_types::Id;
use swc_common::{
    collections::{AHashMap, AHashSet},
    EqIgnoreSpan, TypeEq,
};
use swc_ecma_ast::VarDeclKind;

#[derive(Debug, Clone, Copy, Eq, PartialEq, PartialOrd, Ord, Hash, EqIgnoreSpan, TypeEq)]
pub enum BindingKind {
    Import,
    Class,
    Function,
    Enum,
    TypeAlias,
    /// Modules declared using `module` keyword.
    TsModule,
    Inteface,
    Namespace,
    Module,
    Var(#[use_eq_ignore_span] VarDeclKind),
}

#[derive(Debug, Default)]
pub struct Bindings {
    pub collected: bool,
    pub all: AHashMap<Id, Vec<BindingKind>>,
    pub types: AHashSet<Id>,
}

pub fn collect_bindings<N>(n: &N) -> Bindings
where
    N: Send + Sync + for<'aa> VisitWith<BindingCollector<'aa>> + VisitWith<KnownTypeVisitor>,
{
    let (all, types) = rayon::join(
        || {
            let mut all = AHashMap::default();

            n.visit_with(&mut BindingCollector { data: &mut all });
            all
        },
        || {
            let mut v = KnownTypeVisitor::default();
            n.visit_with(&mut v);
            v.types
        },
    );

    Bindings {
        collected: true,
        all,
        types,
    }
}

pub struct BindingCollector<'a> {
    data: &'a mut AHashMap<Id, Vec<BindingKind>>,
}

impl Visit<RTsNamespaceDecl> for BindingCollector<'_> {
    fn visit(&mut self, decl: &RTsNamespaceDecl) {
        self.data.entry(decl.id.clone().into()).or_default().push(BindingKind::Namespace);

        decl.visit_children_with(self);
    }
}

impl Visit<RTsInterfaceDecl> for BindingCollector<'_> {
    fn visit(&mut self, decl: &RTsInterfaceDecl) {
        self.data.entry(decl.id.clone().into()).or_default().push(BindingKind::Inteface);

        decl.visit_children_with(self);
    }
}

impl Visit<RClassDecl> for BindingCollector<'_> {
    fn visit(&mut self, decl: &RClassDecl) {
        self.data.entry(decl.ident.clone().into()).or_default().push(BindingKind::Class);

        decl.visit_children_with(self);
    }
}

impl Visit<RFnDecl> for BindingCollector<'_> {
    fn visit(&mut self, decl: &RFnDecl) {
        self.data.entry(decl.ident.clone().into()).or_default().push(BindingKind::Function);

        decl.visit_children_with(self);
    }
}

impl Visit<RTsEnumDecl> for BindingCollector<'_> {
    fn visit(&mut self, decl: &RTsEnumDecl) {
        self.data.entry(decl.id.clone().into()).or_default().push(BindingKind::Enum);

        decl.visit_children_with(self);
    }
}

impl Visit<RTsTypeAliasDecl> for BindingCollector<'_> {
    fn visit(&mut self, decl: &RTsTypeAliasDecl) {
        self.data.entry(decl.id.clone().into()).or_default().push(BindingKind::TypeAlias);

        decl.visit_children_with(self);
    }
}

impl Visit<RTsModuleDecl> for BindingCollector<'_> {
    fn visit(&mut self, d: &RTsModuleDecl) {
        d.visit_children_with(self);

        match &d.id {
            RTsModuleName::Ident(i) => {
                self.data.entry(i.clone().into()).or_default().push(BindingKind::TsModule);
            }
            RTsModuleName::Str(_) => {}
        }
    }
}

#[derive(Default)]
pub struct KnownTypeVisitor {
    types: AHashSet<Id>,
}

impl KnownTypeVisitor {
    fn add(&mut self, id: &RIdent) {
        self.types.insert(id.into());
    }
}

impl Visit<RClassDecl> for KnownTypeVisitor {
    fn visit(&mut self, d: &RClassDecl) {
        d.visit_children_with(self);

        self.add(&d.ident);
    }
}

impl Visit<RTsInterfaceDecl> for KnownTypeVisitor {
    fn visit(&mut self, d: &RTsInterfaceDecl) {
        d.visit_children_with(self);

        self.add(&d.id);
    }
}

impl Visit<RTsTypeAliasDecl> for KnownTypeVisitor {
    fn visit(&mut self, d: &RTsTypeAliasDecl) {
        d.visit_children_with(self);

        self.add(&d.id);
    }
}

impl Visit<RTsEnumDecl> for KnownTypeVisitor {
    fn visit(&mut self, d: &RTsEnumDecl) {
        d.visit_children_with(self);

        self.add(&d.id);
    }
}

impl Visit<RTsModuleDecl> for KnownTypeVisitor {
    fn visit(&mut self, d: &RTsModuleDecl) {
        d.visit_children_with(self);

        match &d.id {
            RTsModuleName::Ident(i) => {
                self.add(&i);
            }
            RTsModuleName::Str(_) => {}
        }
    }
}
