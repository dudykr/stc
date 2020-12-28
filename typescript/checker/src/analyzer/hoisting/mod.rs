use crate::util::find_ids_in_pat;
use crate::{
    analyzer::Analyzer,
    util::{
        graph::{Inliner, NodeId},
        map_with_mut::MapWithMut,
        AsModuleDecl, ModuleItemOrStmt,
    },
    ValidationResult,
};
use bitflags::bitflags;
use fxhash::{FxHashMap, FxHashSet};
use petgraph::{
    algo::toposort,
    graph::DiGraph,
    graphmap::DiGraphMap,
    visit::{Dfs, DfsPostOrder},
    EdgeDirection::Incoming,
};
use rnode::Visit;
use rnode::VisitMutWith;
use rnode::VisitWith;
use stc_ast_rnode::RArrowExpr;
use stc_ast_rnode::RClassDecl;
use stc_ast_rnode::RDecl;
use stc_ast_rnode::REmptyStmt;
use stc_ast_rnode::RExportDecl;
use stc_ast_rnode::RExportNamedSpecifier;
use stc_ast_rnode::RExpr;
use stc_ast_rnode::RFnDecl;
use stc_ast_rnode::RFunction;
use stc_ast_rnode::RIdent;
use stc_ast_rnode::RMemberExpr;
use stc_ast_rnode::RModuleDecl;
use stc_ast_rnode::RPat;
use stc_ast_rnode::RProp;
use stc_ast_rnode::RStmt;
use stc_ast_rnode::RTsEntityName;
use stc_ast_rnode::RTsEnumDecl;
use stc_ast_rnode::RTsInterfaceDecl;
use stc_ast_rnode::RTsModuleDecl;
use stc_ast_rnode::RTsTypeAliasDecl;
use stc_ast_rnode::RVarDeclarator;
use stc_types::Id;
use std::{cell::RefCell, mem::replace, rc::Rc};
use swc_common::{Span, DUMMY_SP};
use swc_ecma_ast::*;
use swc_ecma_utils::{find_ids, DestructuringFinder, ModuleItemLike, StmtLike};

#[cfg(test)]
mod tests;

type StmtDepGraph = DiGraphMap<usize, IdKind>;

bitflags! {
    struct IdKind: u8 {
        const VAR = 0b00000001;
        const TYPE = 0b00000010;
        const BOTH = Self::VAR.bits | Self::TYPE.bits;
    }
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
struct TypedId {
    id: Id,
    kind: IdKind,
}

#[cfg_attr(debug_assertiobs, derive(Debug))]
pub(super) enum TypeOrderItem {
    /// Normal statement.
    Normal(usize),
    /// Processing of a type (or types) name `id` is finished.
    Done(Id),
}

impl Analyzer<'_, '_> {
    /// Note: This method removes all items from `stmts`.
    pub(super) fn validate_stmts_with_hoisting<T>(&mut self, stmts: &mut Vec<T>) -> Vec<Vec<T>>
    where
        T: AsModuleDecl
            + ModuleItemOrStmt
            + VisitWith<RequirementCalculartor>
            + VisitMutWith<Self>
            + From<RStmt>,
    {
        let mut new: Vec<Vec<T>> = (0..stmts.len()).map(|_| vec![]).collect();
        let (order, skip) = self.reorder_stmts(&stmts);
        let mut type_decls =
            FxHashMap::<Id, Vec<usize>>::with_capacity_and_hasher(order.len(), Default::default());

        if self.scope.is_root() {
            // We should track type declarations.
            for &idx in &order {
                let type_decl_id = type_decl_id(&stmts[idx]);
                if let Some(id) = type_decl_id {
                    type_decls.entry(id).or_default().push(idx);
                }
            }
        }

        for idx in order {
            let module_id = self.storage.module_id(idx);
            self.ctx.module_id = module_id;

            if skip.contains(&idx) {
                new[idx] = vec![replace(
                    &mut stmts[idx],
                    T::from(RStmt::Empty(REmptyStmt { span: DUMMY_SP })),
                )];
            } else {
                let type_decl_id = type_decl_id(&stmts[idx]);

                stmts[idx].visit_mut_with(self);

                new[idx].extend(self.prepend_stmts.drain(..).map(T::from));

                new[idx].push(replace(
                    &mut stmts[idx],
                    T::from(RStmt::Empty(REmptyStmt { span: DUMMY_SP })),
                ));

                new[idx].extend(self.append_stmts.drain(..).map(T::from));
            }
        }

        stmts.clear();

        new
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
    pub(super) fn validate_stmts_and_collect<T>(&mut self, stmts: &mut Vec<T>)
    where
        T: AsModuleDecl
            + ModuleItemOrStmt
            + VisitWith<RequirementCalculartor>
            + VisitMutWith<Self>
            + From<RStmt>,
    {
        let new = self.validate_stmts_with_hoisting(stmts);
        stmts.clear();
        stmts.extend(new.into_iter().flatten())
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
    pub(super) fn reorder_stmts<T>(&mut self, stmts: &[T]) -> (Vec<usize>, FxHashSet<usize>)
    where
        T: AsModuleDecl + VisitWith<RequirementCalculartor>,
    {
        let mut graph = StmtDepGraph::default();
        let mut declared_by = FxHashMap::<TypedId, Vec<usize>>::default();
        let mut unresolved_circular_imports = vec![];
        let mut skip = FxHashSet::default();

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
                        RDecl::Class(RClassDecl { ident, .. }) => {
                            declared_by
                                .entry(TypedId {
                                    id: Id::from(ident),
                                    kind: IdKind::VAR,
                                })
                                .or_default()
                                .push(idx);
                            declared_by
                                .entry(TypedId {
                                    id: Id::from(ident),
                                    kind: IdKind::TYPE,
                                })
                                .or_default()
                                .push(idx);
                        }
                        RDecl::Fn(RFnDecl { ident, .. }) => {
                            declared_by
                                .entry(TypedId {
                                    id: Id::from(ident),
                                    kind: IdKind::VAR,
                                })
                                .or_default()
                                .push(idx);
                        }
                        RDecl::Var(vars) => {
                            for var in &vars.decls {
                                //
                                let ids: Vec<Id> = find_ids_in_pat(&var.name);
                                for id in ids {
                                    declared_by
                                        .entry(TypedId {
                                            id,
                                            kind: IdKind::VAR,
                                        })
                                        .or_default()
                                        .push(idx);
                                }
                            }
                        }
                        RDecl::TsInterface(RTsInterfaceDecl { id, .. })
                        | RDecl::TsTypeAlias(RTsTypeAliasDecl { id, .. })
                        | RDecl::TsEnum(RTsEnumDecl { id, .. }) => {
                            declared_by
                                .entry(TypedId {
                                    id: Id::from(id),
                                    kind: IdKind::TYPE,
                                })
                                .or_default()
                                .push(idx);
                        }
                        RDecl::TsModule(_) => {}
                    }
                }
                _ => {}
            }
        }

        // Calculate requirements, and fill graph.
        for (idx, item) in stmts.iter().enumerate() {
            if skip.contains(&idx) {
                continue;
            }

            match item.as_module_decl() {
                Ok(RModuleDecl::ExportDecl(RExportDecl {
                    decl: RDecl::TsTypeAlias(..),
                    ..
                }))
                | Err(RStmt::Decl(RDecl::TsTypeAlias(..))) => continue,

                Ok(RModuleDecl::ExportDecl(RExportDecl {
                    decl: RDecl::TsModule(RTsModuleDecl { global: true, .. }),
                    ..
                }))
                | Err(RStmt::Decl(RDecl::TsModule(RTsModuleDecl { global: true, .. }))) => continue,
                _ => {}
            }

            let mut visitor = RequirementCalculartor::default();
            item.visit_with(&mut visitor);

            for id in visitor.required_ids {
                if let Some(declarator_indexes) = declared_by.get(&id) {
                    for &declarator_index in declarator_indexes {
                        if declarator_index != idx {
                            match graph.edge_weight_mut(declarator_index, idx) {
                                Some(v) => {
                                    *v |= id.kind;
                                }
                                None => {
                                    graph.add_edge(declarator_index, idx, id.kind);
                                }
                            }
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
                if skip.contains(&i)
                    || orders.contains(&i)
                    || unresolved_circular_imports.contains(&i)
                {
                    continue;
                }

                let dependants = graph.neighbors_directed(i, Incoming);

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
pub(super) struct RequirementCalculartor {
    required_ids: Vec<TypedId>,
    in_var_decl: bool,
}

impl RequirementCalculartor {
    fn insert_var(&mut self, i: &RIdent) {
        self.required_ids.push(TypedId {
            id: i.into(),
            kind: IdKind::VAR,
        });
    }

    fn insert_type(&mut self, i: &RIdent) {
        self.required_ids.push(TypedId {
            id: i.into(),
            kind: IdKind::TYPE,
        });
    }
}

impl Visit<RTsEntityName> for RequirementCalculartor {
    fn visit(&mut self, n: &RTsEntityName) {
        match n {
            RTsEntityName::TsQualifiedName(n) => {
                n.visit_with(self);
            }
            RTsEntityName::Ident(i) => {
                self.insert_type(i);
            }
        }
    }
}

impl Visit<RVarDeclarator> for RequirementCalculartor {
    fn visit(&mut self, var: &RVarDeclarator) {
        let in_var_decl = self.in_var_decl;
        self.in_var_decl = true;

        var.visit_children_with(self);

        self.in_var_decl = in_var_decl;
    }
}

impl Visit<RExportNamedSpecifier> for RequirementCalculartor {
    fn visit(&mut self, n: &RExportNamedSpecifier) {
        self.insert_var(&n.orig);
    }
}

impl Visit<RMemberExpr> for RequirementCalculartor {
    fn visit(&mut self, n: &RMemberExpr) {
        n.obj.visit_with(self);

        if n.computed {
            n.prop.visit_with(self);
        }
    }
}

impl Visit<RExpr> for RequirementCalculartor {
    fn visit(&mut self, n: &RExpr) {
        n.visit_children_with(self);

        match n {
            RExpr::Ident(i) => {
                self.insert_var(i);
            }
            _ => {}
        }
    }
}

impl Visit<RPat> for RequirementCalculartor {
    fn visit(&mut self, n: &RPat) {
        n.visit_children_with(self);

        match n {
            RPat::Ident(i) => {
                if self.in_var_decl {
                    return;
                }
                self.insert_var(i);
            }
            _ => {}
        }
    }
}

impl Visit<RProp> for RequirementCalculartor {
    fn visit(&mut self, n: &RProp) {
        n.visit_children_with(self);

        match n {
            RProp::Shorthand(i) => {
                self.insert_var(i);
            }
            _ => {}
        }
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
