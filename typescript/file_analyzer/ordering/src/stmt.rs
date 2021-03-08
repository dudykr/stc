//! Dependency analyzer for statements.

use crate::types::Sortable;
use fxhash::FxHashMap;
use fxhash::FxHashSet;
use rnode::Visit;
use rnode::VisitWith;
use stc_ts_ast_rnode::RBindingIdent;
use stc_ts_ast_rnode::RDecl;
use stc_ts_ast_rnode::RExpr;
use stc_ts_ast_rnode::RForInStmt;
use stc_ts_ast_rnode::RForOfStmt;
use stc_ts_ast_rnode::RMemberExpr;
use stc_ts_ast_rnode::RModuleDecl;
use stc_ts_ast_rnode::RModuleItem;
use stc_ts_ast_rnode::RProp;
use stc_ts_ast_rnode::RStmt;
use stc_ts_ast_rnode::RTsModuleDecl;
use stc_ts_ast_rnode::RTsModuleName;
use stc_ts_ast_rnode::RVarDecl;
use stc_ts_ast_rnode::RVarDeclOrExpr;
use stc_ts_ast_rnode::RVarDeclOrPat;
use stc_ts_ast_rnode::RVarDeclarator;
use stc_ts_types::Id;
use stc_ts_utils::find_ids_in_pat;
use stc_ts_utils::AsModuleDecl;

impl Sortable for RStmt {
    type Id = Id;

    fn get_decls(&self) -> FxHashMap<Self::Id, FxHashSet<Self::Id>> {
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

    fn get_decls(&self) -> FxHashMap<Self::Id, FxHashSet<Self::Id>> {
        vars_declared_by(self)
    }

    fn uses(&self) -> FxHashSet<Self::Id> {
        let mut v = DepAnalyzer::default();
        self.visit_with(&mut v);
        v.used
    }
}

fn vars_used_by<T>(e: &T) -> FxHashSet<Id>
where
    T: VisitWith<DepAnalyzer>,
{
    let mut v = DepAnalyzer::default();
    e.visit_with(&mut v);
    v.used
}

fn vars_declared_by_var_decl(v: &RVarDecl) -> FxHashMap<Id, FxHashSet<Id>> {
    let mut map = FxHashMap::<_, FxHashSet<_>>::default();
    for decl in &v.decls {
        let vars = find_ids_in_pat(&decl.name);
        let used_ids = vars_used_by(&decl.init);
        for id in vars {
            map.entry(id).or_default().extend(used_ids.clone());
        }
    }

    return map;
}

fn vars_declared_by_decl(d: &RDecl) -> FxHashMap<Id, FxHashSet<Id>> {
    let mut map = FxHashMap::default();
    match d {
        RDecl::Class(c) => {
            let used_ids = vars_used_by(&c.class);
            map.insert(c.ident.clone().into(), used_ids);
            return map;
        }
        RDecl::Fn(f) => {
            let used_ids = vars_used_by(&f.function);
            map.insert(f.ident.clone().into(), used_ids);
            return map;
        }
        RDecl::Var(v) => return vars_declared_by_var_decl(v),
        RDecl::TsEnum(e) => {
            map.insert(e.id.clone().into(), Default::default());
            return map;
        }
        RDecl::TsModule(RTsModuleDecl {
            id: RTsModuleName::Ident(i),
            ..
        }) => {
            map.insert(i.into(), Default::default());
            return map;
        }

        RDecl::TsInterface(_) | RDecl::TsTypeAlias(_) | RDecl::TsModule(_) => return Default::default(),
    }
}

pub fn vars_declared_by<T>(node: &T) -> FxHashMap<Id, FxHashSet<Id>>
where
    T: AsModuleDecl,
{
    match node.as_module_decl() {
        Ok(v) => match v {
            RModuleDecl::ExportDecl(d) => vars_declared_by_decl(&d.decl),

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
            RStmt::Decl(d) => vars_declared_by_decl(d),
            RStmt::For(s) => match &s.init {
                Some(RVarDeclOrExpr::VarDecl(v)) => vars_declared_by_var_decl(v),
                _ => Default::default(),
            },
            RStmt::ForOf(RForOfStmt {
                left: RVarDeclOrPat::VarDecl(v),
                right,
                ..
            })
            | RStmt::ForIn(RForInStmt {
                left: RVarDeclOrPat::VarDecl(v),
                right,
                ..
            }) => {
                let mut map = vars_declared_by_var_decl(v);
                let extra_ids = vars_used_by(&right);

                for (_, e) in map.iter_mut() {
                    e.extend(extra_ids.clone());
                }

                return map;
            }
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

impl Visit<RProp> for DepAnalyzer {
    fn visit(&mut self, p: &RProp) {
        p.visit_children_with(self);

        match p {
            RProp::Shorthand(i) => {
                self.used.insert(i.into());
            }
            _ => {}
        }
    }
}
