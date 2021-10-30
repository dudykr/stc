use crate::{analyzer::Analyzer, util::ModuleItemOrStmt};
use fxhash::{FxHashMap, FxHashSet};
use petgraph::{graphmap::DiGraphMap, EdgeDirection::Outgoing};
use rnode::{Visit, VisitWith};
use stc_ts_ast_rnode::{
    RDecl, RExportDecl, RForInStmt, RForOfStmt, RForStmt, RIdent, RModuleDecl, RStmt, RVarDeclOrExpr, RVarDeclOrPat,
};
use stc_ts_ordering::{stmt::TypedId, types::Sortable};
use stc_ts_types::Id;
use stc_ts_utils::{AsModuleDecl, HasNodeId};
use tracing::warn;

#[cfg(test)]
mod tests;

impl Analyzer<'_, '_> {
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
        let unresolved_circular_imports = vec![];
        let skip = FxHashSet::default();
        let mut used = FxHashMap::<_, FxHashSet<_>>::default();

        // TODO(kdy1): Handle loaded circular imports. This is required to prevent deadlock
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
                            let vars = item.get_decls();

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
                    let vars = item.get_decls();

                    for (id, deps) in vars {
                        declared_by.entry(id).or_default().push(idx);

                        used.entry(idx).or_default().extend(deps);
                    }
                }
                _ => {
                    let used_vars = item.uses();
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

                // filter is used to workaround the bug of petgraph.
                let deps = graph
                    .neighbors_directed(i, Outgoing)
                    .filter(|dep| !orders.contains(dep))
                    .collect::<Vec<_>>();

                if deps.len() != 0 {
                    continue;
                }

                did_work = true;
                orders.push(i);

                // Remove dependencies to other node.
                graph.remove_node(i);
                break;
            }

            if !did_work {
                break;
            }
        }

        // TODO(kdy1): More logic

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
            warn!("Order: {:?}", orders);
        }

        (orders, skip)
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
