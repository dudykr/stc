//! Dependency analyzer for statements.

use crate::types::Sortable;
use fxhash::FxHashSet;
use rnode::Visit;
use rnode::VisitWith;
use stc_ts_ast_rnode::RBindingIdent;
use stc_ts_ast_rnode::RDecl;
use stc_ts_ast_rnode::RExpr;
use stc_ts_ast_rnode::RMemberExpr;
use stc_ts_ast_rnode::RModuleDecl;
use stc_ts_ast_rnode::RModuleItem;
use stc_ts_ast_rnode::RStmt;
use stc_ts_ast_rnode::RTsModuleDecl;
use stc_ts_ast_rnode::RTsModuleName;
use stc_ts_ast_rnode::RVarDeclarator;
use stc_ts_types::Id;
use stc_ts_utils::find_ids_in_pat;
use stc_ts_utils::AsModuleDecl;

impl Sortable for RStmt {
    type Id = Id;

    fn declares(&self) -> FxHashSet<Self::Id> {
        vars_declared_by(self)
    }

    fn uses(&self) -> FxHashSet<Self::Id> {
        let mut v = DepAnalyzer::default();
        self.visit_with(&mut v);
        v.used
    }
}

impl Sortable for RModuleItem {
    type Id = Id;

    fn declares(&self) -> FxHashSet<Self::Id> {
        vars_declared_by(self)
    }

    fn uses(&self) -> FxHashSet<Self::Id> {
        let mut v = DepAnalyzer::default();
        self.visit_with(&mut v);
        v.used
    }
}

fn vars_declared_by<T>(node: &T) -> FxHashSet<Id>
where
    T: AsModuleDecl,
{
    fn ids(d: &RDecl) -> FxHashSet<Id> {
        let id: Id = match d {
            RDecl::Class(c) => c.ident.clone().into(),
            RDecl::Fn(f) => f.ident.clone().into(),
            RDecl::Var(v) => {
                let mut ids: Vec<Id> = find_ids_in_pat(&v.decls);
                return ids.into_iter().collect();
            }
            RDecl::TsEnum(e) => e.id.clone().into(),
            RDecl::TsModule(RTsModuleDecl {
                id: RTsModuleName::Ident(i),
                ..
            }) => i.clone().into(),

            RDecl::TsInterface(_) | RDecl::TsTypeAlias(_) | RDecl::TsModule(_) => return Default::default(),
        };

        let mut set = FxHashSet::with_capacity_and_hasher(1, Default::default());
        set.insert(id);
        set
    }

    match node.as_module_decl() {
        Ok(v) => match v {
            RModuleDecl::ExportDecl(d) => ids(&d.decl),

            RModuleDecl::Import(_)
            | RModuleDecl::ExportNamed(_)
            | RModuleDecl::ExportDefaultExpr(_)
            | RModuleDecl::ExportAll(_) => Default::default(),

            RModuleDecl::ExportDefaultDecl(_) => {
                // TODO
                Default::default()
            }
            RModuleDecl::TsImportEquals(_) => {
                // TODO
                Default::default()
            }
            RModuleDecl::TsExportAssignment(_) => {
                // TODO
                Default::default()
            }
            RModuleDecl::TsNamespaceExport(_) => {
                // TODO
                Default::default()
            }
        },
        Err(stmt) => match stmt {
            RStmt::Decl(d) => ids(d),
            _ => Default::default(),
        },
    }
}

#[derive(Default)]
struct DepAnalyzer {
    used: FxHashSet<Id>,
    in_var_decl: bool,
}

impl Visit<RVarDeclarator> for DepAnalyzer {
    fn visit(&mut self, node: &RVarDeclarator) {
        let old = self.in_var_decl;
        self.in_var_decl = true;
        node.visit_children_with(self);
        self.in_var_decl = old;
    }
}

impl Visit<RMemberExpr> for DepAnalyzer {
    fn visit(&mut self, node: &RMemberExpr) {
        node.obj.visit_with(self);

        if node.computed {
            node.prop.visit_with(self);
        }
    }
}

impl Visit<RBindingIdent> for DepAnalyzer {
    fn visit(&mut self, value: &RBindingIdent) {
        if self.in_var_decl {
            return;
        }
        self.used.insert(value.id.clone().into());
    }
}

impl Visit<RExpr> for DepAnalyzer {
    fn visit(&mut self, node: &RExpr) {
        match node {
            RExpr::Ident(i) => {
                self.used.insert(i.into());
            }
            _ => {}
        }

        node.visit_children_with(self);
    }
}
