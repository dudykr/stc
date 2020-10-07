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
use stc_types::Id;
use std::{cell::RefCell, mem::replace, rc::Rc};
use swc_common::{Span, DUMMY_SP};
use swc_ecma_ast::*;
use swc_ecma_utils::{find_ids, DestructuringFinder, ModuleItemLike, StmtLike};
use swc_ecma_visit::{Node, Visit, VisitMutWith, VisitWith};

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
        T: AsModuleDecl + ModuleItemOrStmt + VisitWith<RequirementCalculartor> + VisitMutWith<Self>,
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
                    T::from_stmt(Stmt::Empty(EmptyStmt { span: DUMMY_SP })),
                )];
            } else {
                let type_decl_id = type_decl_id(&stmts[idx]);

                stmts[idx].visit_mut_with(self);

                new[idx].extend(self.prepend_stmts.drain(..).map(T::from_stmt));

                new[idx].push(replace(
                    &mut stmts[idx],
                    T::from_stmt(Stmt::Empty(EmptyStmt { span: DUMMY_SP })),
                ));

                new[idx].extend(self.append_stmts.drain(..).map(T::from_stmt));
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
        T: AsModuleDecl + ModuleItemOrStmt + VisitWith<RequirementCalculartor> + VisitMutWith<Self>,
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
                Ok(ModuleDecl::Import(import)) => {}

                // We only check declarations because ids are created by declarations.
                Ok(ModuleDecl::ExportDecl(ExportDecl { decl, .. })) | Err(Stmt::Decl(decl)) => {
                    //
                    match decl {
                        Decl::Class(ClassDecl { ident, .. }) => {
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
                        Decl::Fn(FnDecl { ident, .. }) => {
                            declared_by
                                .entry(TypedId {
                                    id: Id::from(ident),
                                    kind: IdKind::VAR,
                                })
                                .or_default()
                                .push(idx);
                        }
                        Decl::Var(vars) => {
                            for var in &vars.decls {
                                //
                                let ids: Vec<Id> = find_ids(&var.name);
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
                        Decl::TsInterface(TsInterfaceDecl { id, .. })
                        | Decl::TsTypeAlias(TsTypeAliasDecl { id, .. })
                        | Decl::TsEnum(TsEnumDecl { id, .. }) => {
                            declared_by
                                .entry(TypedId {
                                    id: Id::from(id),
                                    kind: IdKind::TYPE,
                                })
                                .or_default()
                                .push(idx);
                        }
                        Decl::TsModule(_) => {}
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
                Ok(ModuleDecl::ExportDecl(ExportDecl {
                    decl: Decl::TsTypeAlias(..),
                    ..
                }))
                | Err(Stmt::Decl(Decl::TsTypeAlias(..))) => continue,

                Ok(ModuleDecl::ExportDecl(ExportDecl {
                    decl: Decl::TsModule(TsModuleDecl { global: true, .. }),
                    ..
                }))
                | Err(Stmt::Decl(Decl::TsModule(TsModuleDecl { global: true, .. }))) => continue,
                _ => {}
            }

            let mut visitor = RequirementCalculartor::default();
            item.visit_with(&Invalid { span: DUMMY_SP }, &mut visitor);

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
    fn insert_var(&mut self, i: &Ident) {
        self.required_ids.push(TypedId {
            id: i.into(),
            kind: IdKind::VAR,
        });
    }

    fn insert_type(&mut self, i: &Ident) {
        self.required_ids.push(TypedId {
            id: i.into(),
            kind: IdKind::TYPE,
        });
    }
}

impl Visit for RequirementCalculartor {
    fn visit_ts_entity_name(&mut self, n: &TsEntityName, _: &dyn Node) {
        match n {
            TsEntityName::TsQualifiedName(n) => {
                n.visit_with(n, self);
            }
            TsEntityName::Ident(i) => {
                self.insert_type(i);
            }
        }
    }

    fn visit_var_declarator(&mut self, var: &VarDeclarator, _: &dyn Node) {
        let in_var_decl = self.in_var_decl;
        self.in_var_decl = true;

        var.visit_children_with(self);

        self.in_var_decl = in_var_decl;
    }

    fn visit_export_named_specifier(&mut self, n: &ExportNamedSpecifier, _: &dyn Node) {
        self.insert_var(&n.orig);
    }

    fn visit_member_expr(&mut self, n: &MemberExpr, _: &dyn Node) {
        n.obj.visit_with(n, self);

        if n.computed {
            n.prop.visit_with(n, self);
        }
    }

    fn visit_expr(&mut self, n: &Expr, _: &dyn Node) {
        n.visit_children_with(self);

        match n {
            Expr::Ident(i) => {
                self.insert_var(i);
            }
            _ => {}
        }
    }

    fn visit_pat(&mut self, n: &Pat, _: &dyn Node) {
        n.visit_children_with(self);

        match n {
            Pat::Ident(i) => {
                if self.in_var_decl {
                    return;
                }
                self.insert_var(i);
            }
            _ => {}
        }
    }

    fn visit_prop(&mut self, n: &Prop, _: &dyn Node) {
        n.visit_children_with(self);

        match n {
            Prop::Shorthand(i) => {
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

impl Visit for StmtDependencyFinder {
    fn visit_fn_decl(&mut self, node: &FnDecl, _: &dyn Node) {
        use swc_ecma_visit::VisitWith;

        if !self.no_decl {
            self.ids.insert(node.ident.clone().into());
        }
        node.visit_children_with(self);
    }

    fn visit_var_declarator(&mut self, node: &VarDeclarator, _: &dyn Node) {
        use swc_ecma_visit::VisitWith;

        if !self.no_decl {
            {
                let mut v = DestructuringFinder {
                    found: &mut self.ids_buf,
                };
                node.name.visit_with(node, &mut v);
            }

            self.ids.extend(self.ids_buf.drain(..));
        }

        node.init.visit_with(node, self);
    }

    fn visit_class_decl(&mut self, node: &ClassDecl, _: &dyn Node) {
        use swc_ecma_visit::VisitWith;

        let old = self.no_decl;
        node.class.visit_with(node, self);
        self.no_decl = old;
    }

    fn visit_function(&mut self, n: &Function, _: &dyn Node) {
        use swc_ecma_visit::VisitWith;

        let old = self.no_decl;
        n.visit_children_with(self);
        self.no_decl = old;
    }

    fn visit_arrow_expr(&mut self, n: &ArrowExpr, _: &dyn Node) {
        use swc_ecma_visit::VisitWith;

        let old = self.no_decl;
        n.visit_children_with(self);
        self.no_decl = old;
    }

    fn visit_member_expr(&mut self, node: &MemberExpr, _: &dyn Node) {
        use swc_ecma_visit::VisitWith;

        node.obj.visit_with(node, self);

        if node.computed {
            node.prop.visit_with(node, self);
        }
    }

    fn visit_expr(&mut self, node: &Expr, _: &dyn Node) {
        use swc_ecma_visit::VisitWith;

        match node {
            Expr::Ident(ref i) => {
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

impl Visit for TypeParamDepFinder<'_> {
    fn visit_ident(&mut self, node: &Ident, _: &dyn Node) {
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
            ModuleDecl::ExportDecl(export) => &export.decl,
            _ => return None,
        },
        Err(stmt) => match stmt {
            Stmt::Decl(decl) => decl,
            _ => return None,
        },
    };

    Some(Id::from(match decl {
        Decl::Class(c) => &c.ident,
        Decl::TsInterface(i) => &i.id,
        Decl::TsTypeAlias(a) => &a.id,
        Decl::TsEnum(e) => &e.id,
        Decl::TsModule(..) => return None,
        Decl::Fn(_) | Decl::Var(_) => return None,
    }))
}
