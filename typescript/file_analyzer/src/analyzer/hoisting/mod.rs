use crate::{analyzer::Analyzer, util::ModuleItemOrStmt};
use fxhash::{FxHashMap, FxHashSet};
use petgraph::graphmap::DiGraphMap;
use petgraph::EdgeDirection::Outgoing;
use rnode::Visit;
use rnode::VisitWith;
use stc_ts_ast_rnode::RArrowExpr;
use stc_ts_ast_rnode::RClassDecl;
use stc_ts_ast_rnode::RDecl;
use stc_ts_ast_rnode::RExportDecl;
use stc_ts_ast_rnode::RExpr;
use stc_ts_ast_rnode::RFnDecl;
use stc_ts_ast_rnode::RForInStmt;
use stc_ts_ast_rnode::RForOfStmt;
use stc_ts_ast_rnode::RForStmt;
use stc_ts_ast_rnode::RFunction;
use stc_ts_ast_rnode::RIdent;
use stc_ts_ast_rnode::RMemberExpr;
use stc_ts_ast_rnode::RModuleDecl;
use stc_ts_ast_rnode::RStmt;
use stc_ts_ast_rnode::RVarDeclOrExpr;
use stc_ts_ast_rnode::RVarDeclOrPat;
use stc_ts_ast_rnode::RVarDeclarator;
use stc_ts_ordering::stmt::TypedId;
use stc_ts_ordering::types::Sortable;
use stc_ts_types::Id;
use stc_ts_utils::AsModuleDecl;
use stc_ts_utils::HasNodeId;
use swc_ecma_utils::DestructuringFinder;

#[cfg(test)]
mod tests;

#[cfg_attr(debug_assertiobs, derive(Debug))]
pub(super) enum TypeOrderItem {
    /// Normal statement.
    Normal(usize),
    /// Processing of a type (or types) name `id` is finished.
    Done(Id),
}

impl Analyzer<'_, '_> {
    /// Note: This method removes all items from `stmts`.
    pub(super) fn validate_stmts_with_hoisting<T>(&mut self, stmts: &Vec<&T>)
    where
        T: AsModuleDecl + ModuleItemOrStmt + VisitWith<Self> + From<RStmt> + HasNodeId + Sortable<Id = TypedId>,
    {
        let (order, skip) = self.reorder_stmts(&stmts);
        let mut type_decls = FxHashMap::<Id, Vec<usize>>::with_capacity_and_hasher(order.len(), Default::default());

        if self.scope.is_root() {
            // We should track type declarations.
            for &idx in &order {
                let type_decl_id = type_decl_id(&*stmts[idx]);
                if let Some(id) = type_decl_id {
                    type_decls.entry(id).or_default().push(idx);
                }
            }
        }

        for idx in order {
            if self.scope.is_root() {
                let module_id = self.storage.module_id(idx);
                self.ctx.module_id = module_id;
            }

            if skip.contains(&idx) {
            } else {
                let type_decl_id = type_decl_id(&*stmts[idx]);

                let node_id = stmts[idx].node_id();
                stmts[idx].visit_with(self);

                if self.scope.is_root() {
                    let prepended = self.prepend_stmts.drain(..);
                    let appended = self.append_stmts.drain(..);

                    if let Some(node_id) = node_id {
                        if let Some(m) = &mut self.mutations {
                            m.for_module_items
                                .entry(node_id)
                                .or_default()
                                .prepend_stmts
                                .extend(prepended);

                            m.for_module_items
                                .entry(node_id)
                                .or_default()
                                .append_stmts
                                .extend(appended);
                        }
                    }
                }
            }
        }
    }

    /// A special method is require code like
    ///
    /// ```ts
    /// function foo() {
    ///     return a;
    /// }
    ///
    /// const a = 5;
    /// const b = foo();
    /// ```
    pub(super) fn validate_stmts_and_collect<T>(&mut self, stmts: &Vec<&T>)
    where
        T: AsModuleDecl + ModuleItemOrStmt + VisitWith<Self> + From<RStmt> + HasNodeId + Sortable<Id = TypedId>,
    {
        self.validate_stmts_with_hoisting(stmts);
    }

    /// Returns (the order of evaluation, skipped index). This methods is used
    /// to handle hoisting properly.
    ///
    /// # Exmaple
    ///
    /// The method will return `[1, 0]` for the code below.
    ///
    /// ```js
    /// function foo() {
    ///     return bar();
    /// }
    ///
    /// function bar (){
    ///     return 1;
    /// }K
    /// ```
    ///
    ///
    /// # Note
    ///
    /// This function prioritze types in order of
    /// - no deps
    /// - resolvable (non-circular)
    /// - others
    ///
    /// `a.ts`:
    /// ```ts
    /// import { B } from './b';
    /// export type C = 5 | 10;
    /// export type B = A;
    /// ```
    ///
    /// `b.ts`:
    /// ```ts
    /// import A from './a';
    /// export type C = 5 | 10;
    /// export type B = A;
    /// ```
    fn reorder_stmts<T>(&mut self, stmts: &[&T]) -> (Vec<usize>, FxHashSet<usize>)
    where
        T: AsModuleDecl + Sortable<Id = TypedId>,
    {
        let mut graph = DiGraphMap::default();
        let mut declared_by = FxHashMap::<TypedId, Vec<usize>>::default();
        let mut unresolved_circular_imports = vec![];
        let mut skip = FxHashSet::default();
        let mut used = FxHashMap::<_, FxHashSet<_>>::default();

        // TODO: Handle loaded circular imports. This is required to prevent deadlock
        // and duplicated work.

        // Caculate declarations.
        for (idx, item) in stmts.iter().enumerate() {
            graph.add_node(idx);

            match item.as_module_decl() {
                Ok(RModuleDecl::Import(import)) => {}

                // We only check declarations because ids are created by declarations.
                Ok(RModuleDecl::ExportDecl(RExportDecl { decl, .. })) | Err(RStmt::Decl(decl)) => {
                    //
                    match decl {
                        RDecl::Class(..)
                        | RDecl::TsInterface(..)
                        | RDecl::TsTypeAlias(..)
                        | RDecl::TsEnum(..)
                        | RDecl::Fn(..)
                        | RDecl::Var(..) => {
                            let mut vars = item.get_decls();

                            for (id, deps) in vars {
                                declared_by.entry(id).or_default().push(idx);

                                used.entry(idx).or_default().extend(deps);
                            }
                        }

                        RDecl::TsModule(_) => {}
                    }
                }
                Err(RStmt::For(RForStmt {
                    init: Some(RVarDeclOrExpr::VarDecl(..)),
                    ..
                }))
                | Err(RStmt::ForOf(RForOfStmt {
                    left: RVarDeclOrPat::VarDecl(..),
                    ..
                }))
                | Err(RStmt::ForIn(RForInStmt {
                    left: RVarDeclOrPat::VarDecl(..),
                    ..
                })) => {
                    let mut vars = item.get_decls();

                    for (id, deps) in vars {
                        declared_by.entry(id).or_default().push(idx);

                        used.entry(idx).or_default().extend(deps);
                    }
                }
                _ => {
                    let mut used_vars = item.uses();
                    used.entry(idx).or_default().extend(used_vars);
                }
            }
        }

        // Fill graph.
        for (idx, deps) in used {
            for dep in deps {
                if let Some(declarator_indexes) = declared_by.get(&dep) {
                    for &declarator_index in declarator_indexes {
                        if declarator_index != idx {
                            graph.add_edge(idx, declarator_index, ());
                        }
                    }
                }
            }
        }

        let len = stmts.len();
        let mut orders = Vec::with_capacity(stmts.len());

        // No dependencies
        loop {
            if graph.all_edges().count() == 0 {
                break;
            }

            let mut did_work = false;
            // Add nodes which does not have any dependencies.
            for i in 0..len {
                if skip.contains(&i) || orders.contains(&i) || unresolved_circular_imports.contains(&i) {
                    continue;
                }

                let dependants = graph.neighbors_directed(i, Outgoing);

                if dependants.count() != 0 {
                    continue;
                }

                did_work = true;
                orders.push(i);

                // Remove dependencies to other node.
                graph.remove_node(i);
            }

            if !did_work {
                break;
            }
        }

        // TODO: More logic

        // Postpone handling of circular imports as much as possible.
        for i in 0..len {
            if skip.contains(&i) {
                continue;
            }
            if !orders.contains(&i) && !unresolved_circular_imports.contains(&i) {
                orders.push(i);
            }
        }

        for i in 0..len {
            if !orders.contains(&i) {
                orders.push(i);
            }
        }

        if self.scope.is_root() {
            slog::warn!(&self.logger, "Order: {:?}", orders);
        }

        (orders, skip)
    }
}

#[derive(Default)]
pub(super) struct StmtDependencyFinder {
    ids_buf: Vec<Id>,
    /// Identifiers created by a statement.
    ///
    /// e.g.
    ///
    /// Value is `[a, b]` for the var declaration below.
    /// ```js
    /// var a, b = foo();
    /// ```
    ids: FxHashSet<Id>,

    /// Dependencies of the id.
    deps: FxHashSet<Id>,

    no_decl: bool,
}

impl Visit<RFnDecl> for StmtDependencyFinder {
    fn visit(&mut self, node: &RFnDecl) {
        if !self.no_decl {
            self.ids.insert(node.ident.clone().into());
        }
        node.visit_children_with(self);
    }
}

impl Visit<RVarDeclarator> for StmtDependencyFinder {
    fn visit(&mut self, node: &RVarDeclarator) {
        if !self.no_decl {
            {
                let mut v = DestructuringFinder {
                    found: &mut self.ids_buf,
                };
                node.name.visit_with(&mut v);
            }

            self.ids.extend(self.ids_buf.drain(..));
        }

        node.init.visit_with(self);
    }
}

impl Visit<RClassDecl> for StmtDependencyFinder {
    fn visit(&mut self, node: &RClassDecl) {
        let old = self.no_decl;
        node.class.visit_with(self);
        self.no_decl = old;
    }
}

impl Visit<RFunction> for StmtDependencyFinder {
    fn visit(&mut self, n: &RFunction) {
        let old = self.no_decl;
        n.visit_children_with(self);
        self.no_decl = old;
    }
}

impl Visit<RArrowExpr> for StmtDependencyFinder {
    fn visit(&mut self, n: &RArrowExpr) {
        let old = self.no_decl;
        n.visit_children_with(self);
        self.no_decl = old;
    }
}

impl Visit<RMemberExpr> for StmtDependencyFinder {
    fn visit(&mut self, node: &RMemberExpr) {
        node.obj.visit_with(self);

        if node.computed {
            node.prop.visit_with(self);
        }
    }
}

impl Visit<RExpr> for StmtDependencyFinder {
    fn visit(&mut self, node: &RExpr) {
        match node {
            RExpr::Ident(ref i) => {
                self.deps.insert(i.into());
            }
            _ => {}
        }

        node.visit_children_with(self);
    }
}
#[derive(Debug)]
struct TypeParamDepFinder<'a> {
    id: &'a Id,
    deps: &'a mut FxHashSet<Id>,
}

impl Visit<RIdent> for TypeParamDepFinder<'_> {
    fn visit(&mut self, node: &RIdent) {
        if *self.id == node {
            return;
        }

        self.deps.insert(node.clone().into());
    }
}

fn type_decl_id<T>(t: &T) -> Option<Id>
where
    T: AsModuleDecl,
{
    let decl = match t.as_module_decl() {
        Ok(decl) => match decl {
            RModuleDecl::ExportDecl(export) => &export.decl,
            _ => return None,
        },
        Err(stmt) => match stmt {
            RStmt::Decl(decl) => decl,
            _ => return None,
        },
    };

    Some(Id::from(match decl {
        RDecl::Class(c) => &c.ident,
        RDecl::TsInterface(i) => &i.id,
        RDecl::TsTypeAlias(a) => &a.id,
        RDecl::TsEnum(e) => &e.id,
        RDecl::TsModule(..) => return None,
        RDecl::Fn(_) | RDecl::Var(_) => return None,
    }))
}
