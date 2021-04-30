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
use stc_ts_ast_rnode::RIdent;
use stc_ts_ast_rnode::RMemberExpr;
use stc_ts_ast_rnode::RModuleDecl;
use stc_ts_ast_rnode::RModuleItem;
use stc_ts_ast_rnode::RProp;
use stc_ts_ast_rnode::RStmt;
use stc_ts_ast_rnode::RTsEntityName;
use stc_ts_ast_rnode::RTsExprWithTypeArgs;
use stc_ts_ast_rnode::RTsIndexSignature;
use stc_ts_ast_rnode::RTsModuleDecl;
use stc_ts_ast_rnode::RTsModuleName;
use stc_ts_ast_rnode::RTsTypeRef;
use stc_ts_ast_rnode::RVarDecl;
use stc_ts_ast_rnode::RVarDeclOrExpr;
use stc_ts_ast_rnode::RVarDeclOrPat;
use stc_ts_ast_rnode::RVarDeclarator;
use stc_ts_types::Id;
use stc_ts_types::IdCtx;
use stc_ts_utils::find_ids_in_pat;
use stc_ts_utils::AsModuleDecl;

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct TypedId {
    pub kind: IdCtx,
    pub id: Id,
}

impl Sortable for RStmt {
    type Id = TypedId;

    fn get_decls(&self) -> FxHashMap<Self::Id, FxHashSet<Self::Id>> {
        ids_declared_by(self)
    }

    fn uses(&self) -> FxHashSet<Self::Id> {
        deps_of(self)
    }
}

impl Sortable for RModuleItem {
    type Id = TypedId;

    fn get_decls(&self) -> FxHashMap<Self::Id, FxHashSet<Self::Id>> {
        ids_declared_by(self)
    }

    fn uses(&self) -> FxHashSet<Self::Id> {
        deps_of(self)
    }
}

fn deps_of<T>(e: &T) -> FxHashSet<TypedId>
where
    T: VisitWith<DepAnalyzer>,
{
    let mut v = DepAnalyzer::default();
    e.visit_with(&mut v);
    v.used
}

fn vars_declared_by_var_decl(v: &RVarDecl) -> FxHashMap<TypedId, FxHashSet<TypedId>> {
    let mut map = FxHashMap::<_, FxHashSet<_>>::default();
    for decl in &v.decls {
        let vars = find_ids_in_pat(&decl.name);

        // Get deps of name.
        let mut type_ids = deps_of(&decl.init);

        // Exclude the variables we are defining.
        for var in vars.iter().cloned() {
            type_ids.remove(&TypedId {
                kind: IdCtx::Var,
                id: var,
            });
        }

        let used_ids = deps_of(&decl.init);
        for id in vars {
            map.entry(TypedId {
                kind: IdCtx::Var,
                id: id.clone(),
            })
            .or_default()
            .extend(used_ids.clone());

            map.entry(TypedId { kind: IdCtx::Var, id })
                .or_default()
                .extend(type_ids.clone());
        }
    }

    return map;
}

fn ids_declared_by_decl(d: &RDecl) -> FxHashMap<TypedId, FxHashSet<TypedId>> {
    let mut map = FxHashMap::default();
    match d {
        RDecl::Class(c) => {
            let used_ids = deps_of(&c.class);
            map.insert(
                TypedId {
                    kind: IdCtx::Type,
                    id: c.ident.clone().into(),
                },
                used_ids.clone(),
            );
            map.insert(
                TypedId {
                    kind: IdCtx::Var,
                    id: c.ident.clone().into(),
                },
                used_ids,
            );
            return map;
        }
        RDecl::Fn(f) => {
            let used_ids = deps_of(&f.function);
            map.insert(
                TypedId {
                    kind: IdCtx::Var,
                    id: f.ident.clone().into(),
                },
                used_ids,
            );
            return map;
        }
        RDecl::Var(v) => return vars_declared_by_var_decl(v),
        RDecl::TsEnum(e) => {
            map.insert(
                TypedId {
                    kind: IdCtx::Type,
                    id: e.id.clone().into(),
                },
                Default::default(),
            );
            map.insert(
                TypedId {
                    kind: IdCtx::Var,
                    id: e.id.clone().into(),
                },
                Default::default(),
            );
            return map;
        }
        RDecl::TsModule(RTsModuleDecl {
            id: RTsModuleName::Ident(i),
            ..
        }) => {
            map.insert(
                TypedId {
                    kind: IdCtx::Type,
                    id: i.clone().into(),
                },
                Default::default(),
            );
            map.insert(
                TypedId {
                    kind: IdCtx::Var,
                    id: i.clone().into(),
                },
                Default::default(),
            );
            return map;
        }

        RDecl::TsInterface(i) => {
            let mut deps = deps_of(&i.extends);
            deps.extend(deps_of(&i.type_params));
            map.insert(
                TypedId {
                    kind: IdCtx::Type,
                    id: i.id.clone().into(),
                },
                deps,
            );
            return map;
        }

        RDecl::TsTypeAlias(a) => {
            let deps = deps_of(&a.type_ann);
            map.insert(
                TypedId {
                    kind: IdCtx::Type,
                    id: a.id.clone().into(),
                },
                deps,
            );

            return map;
        }

        RDecl::TsModule(_) => return Default::default(),
    }
}

fn ids_declared_by<T>(node: &T) -> FxHashMap<TypedId, FxHashSet<TypedId>>
where
    T: AsModuleDecl,
{
    match node.as_module_decl() {
        Ok(v) => match v {
            RModuleDecl::ExportDecl(d) => ids_declared_by_decl(&d.decl),

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
            RStmt::Decl(d) => ids_declared_by_decl(d),
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
                let extra_ids = deps_of(&right);

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
    used: FxHashSet<TypedId>,
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
        self.used.insert(TypedId {
            kind: IdCtx::Var,
            id: value.id.clone().into(),
        });
    }
}

impl Visit<RExpr> for DepAnalyzer {
    fn visit(&mut self, node: &RExpr) {
        match node {
            RExpr::Ident(i) => {
                self.used.insert(TypedId {
                    kind: IdCtx::Var,
                    id: i.into(),
                });
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
                self.used.insert(TypedId {
                    kind: IdCtx::Var,
                    id: i.into(),
                });
            }
            _ => {}
        }
    }
}

impl Visit<RTsExprWithTypeArgs> for DepAnalyzer {
    fn visit(&mut self, e: &RTsExprWithTypeArgs) {
        e.visit_children_with(self);

        let id = left(&e.expr);
        self.used.insert(TypedId {
            kind: IdCtx::Type,
            id: id.into(),
        });
    }
}

impl Visit<RTsTypeRef> for DepAnalyzer {
    fn visit(&mut self, t: &RTsTypeRef) {
        t.visit_children_with(self);

        let id = left(&t.type_name);
        self.used.insert(TypedId {
            kind: IdCtx::Type,
            id: id.into(),
        });
    }
}

/// Noop.
impl Visit<RTsIndexSignature> for DepAnalyzer {
    fn visit(&mut self, _: &RTsIndexSignature) {}
}

fn left(t: &RTsEntityName) -> &RIdent {
    match t {
        RTsEntityName::TsQualifiedName(q) => left(&q.left),
        RTsEntityName::Ident(i) => i,
    }
}
