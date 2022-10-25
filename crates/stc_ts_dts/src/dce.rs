//! Dead code elimination for types.

use fxhash::FxHashSet;
use rnode::{NodeId, Visit, VisitMut, VisitMutWith, VisitWith};
use stc_ts_ast_rnode::{
    RBlockStmt, RClassDecl, RClassMember, RClassMethod, RClassProp, RDecl, REmptyStmt, RExpr,
    RFnDecl, RFunction, RInvalid, RLit, RModuleDecl, RModuleItem, RPat, RPropName, RStmt,
    RTsEnumDecl, RTsEnumMember, RTsKeywordType, RTsModuleDecl, RTsType, RTsTypeAliasDecl,
    RTsTypeAnn, RVarDecl, RVarDeclarator,
};
use stc_ts_types::{rprop_name_to_expr, Id, ModuleTypeData, Type};
use stc_ts_utils::{MapWithMut, PatExt};
use swc_common::{Spanned, DUMMY_SP};
use swc_ecma_ast::*;

#[derive(Debug)]
pub(super) struct DceForDts<'a> {
    pub info: &'a ModuleTypeData,
    pub used: FxHashSet<Id>,
    pub current_class: Option<stc_ts_types::Class>,

    pub in_declare: bool,
    pub top_level: bool,
    pub forced_module: bool,
    pub prevent_empty_export: bool,
}

impl DceForDts<'_> {
    fn get_mapped<F, T>(&self, sym: &Id, mut pred: F) -> Option<T>
    where
        F: FnMut(&Type) -> Option<T>,
    {
        if let Some(types) = self.info.private_types.get(sym) {
            for ty in &*types {
                debug_assert!(
                    ty.is_clone_cheap(),
                    "All exported types must be freezed: {:?}",
                    ty
                );
            }

            types.iter().filter_map(|ty| pred(ty)).next()
        } else {
            None
        }
    }
}

impl VisitMut<RBlockStmt> for DceForDts<'_> {
    fn visit_mut(&mut self, node: &mut RBlockStmt) {
        let old = self.top_level;
        self.top_level = false;
        node.visit_mut_children_with(self);
        self.top_level = old;
    }
}

impl VisitMut<RFunction> for DceForDts<'_> {
    fn visit_mut(&mut self, mut node: &mut RFunction) {
        node.is_generator = false;
        node.is_async = false;

        let old = self.top_level;
        self.top_level = false;
        node.visit_mut_children_with(self);
        self.top_level = old;
    }
}

impl VisitMut<RVarDecl> for DceForDts<'_> {
    fn visit_mut(&mut self, node: &mut RVarDecl) {
        node.visit_mut_children_with(self);
        node.declare = true;

        node.decls.retain(|v| match v.name {
            RPat::Invalid(..) => false,
            _ => true,
        });

        if node.kind != VarDeclKind::Const {
            node.decls.iter_mut().for_each(|node| match node.init {
                Some(box RExpr::Lit(RLit::Num(..))) => {
                    node.init = None;
                    node.name
                        .set_ty(Some(box RTsType::TsKeywordType(RTsKeywordType {
                            span: DUMMY_SP,
                            kind: TsKeywordTypeKind::TsNumberKeyword,
                        })))
                }
                _ => {}
            });
        }
    }
}

impl VisitMut<RVarDeclarator> for DceForDts<'_> {
    fn visit_mut(&mut self, node: &mut RVarDeclarator) {
        if match &node.name {
            RPat::Array(arr) => arr.elems.is_empty(),
            RPat::Object(obj) => obj.props.is_empty(),
            _ => false,
        } {
            node.name = RPat::Invalid(RInvalid { span: DUMMY_SP });
            node.init = None;
            return;
        }

        match node.init {
            Some(box RExpr::Lit(RLit::Null(..))) | Some(box RExpr::Lit(RLit::JSXText(..))) => {
                node.init = None;
            }

            Some(box RExpr::Lit(..)) => {}
            _ => {
                node.init = None;
            }
        };

        if node.init.is_some() {
            node.name.set_ty(None);
            return;
        }

        match node.name {
            RPat::Ident(ref mut i) => {
                if i.type_ann.is_none() {
                    if let Some(ty) = self.info.private_vars.get(&i.id.clone().into()) {
                        i.type_ann = Some(RTsTypeAnn {
                            node_id: NodeId::invalid(),
                            span: DUMMY_SP,
                            type_ann: box ty.clone().into(),
                        });
                    }
                }
            }
            _ => {}
        }

        if node.init.is_none() {
            node.name.visit_mut_with(self);
        }
    }
}

impl VisitMut<RPat> for DceForDts<'_> {
    fn visit_mut(&mut self, node: &mut RPat) {
        node.visit_mut_children_with(self);

        match node {
            RPat::Assign(a) => *node = a.left.take(),
            _ => {}
        }
    }
}

impl VisitMut<RFnDecl> for DceForDts<'_> {
    fn visit_mut(&mut self, node: &mut RFnDecl) {
        node.declare = !self.in_declare;

        node.visit_mut_children_with(self);

        if node.function.return_type.is_some() {
            return;
        }

        node.function.return_type = self.get_mapped(&node.ident.clone().into(), |ty| match ty {
            Type::Function(stc_ts_types::Function { ref ret_ty, .. }) => {
                Some(RTsTypeAnn::from((**ret_ty).clone()))
            }
            _ => None,
        });
    }
}

impl VisitMut<RTsModuleDecl> for DceForDts<'_> {
    fn visit_mut(&mut self, node: &mut RTsModuleDecl) {
        self.prevent_empty_export = true;

        let old_in_declare = self.in_declare;
        let old_top_level = self.top_level;

        self.top_level = false;
        self.in_declare = true;

        node.declare = true;
        if !node.global {
            node.visit_mut_children_with(self);
        }

        self.top_level = old_top_level;
        self.in_declare = old_in_declare;
    }
}

impl VisitMut<RTsEnumDecl> for DceForDts<'_> {
    fn visit_mut(&mut self, node: &mut RTsEnumDecl) {
        let mut is_all_lit = true;
        let mut should_init_only_first = true;
        let has_no_init = node.members.iter().all(|v| v.init.is_none());

        if node.members.iter().any(|m| m.init.is_some()) {
            should_init_only_first = false;
        }

        let _: Option<()> = self.get_mapped(&node.id.clone().into(), |ty| {
            match ty {
                Type::Enum(e) => {
                    //
                    if e.members.iter().any(|m| match *m.val {
                        RExpr::Tpl(..) | RExpr::Lit(..) => false,

                        _ => true,
                    }) {
                        is_all_lit = false;
                    }
                }
                _ => {}
            }

            None
        });

        let members = self.get_mapped(&node.id.clone().into(), |ty| match ty {
            Type::Enum(e) => Some(
                e.members
                    .iter()
                    .enumerate()
                    .map(|(i, member)| RTsEnumMember {
                        node_id: NodeId::invalid(),
                        span: member.span,
                        id: member.id.clone(),
                        init: if is_all_lit {
                            if has_no_init {
                                Some(member.val.clone())
                            } else {
                                if should_init_only_first {
                                    if i == 0 {
                                        Some(member.val.clone())
                                    } else {
                                        None
                                    }
                                } else {
                                    Some(member.val.clone())
                                }
                            }
                        } else {
                            None
                        },
                    })
                    .collect(),
            ),
            _ => None,
        });

        node.declare = !self.in_declare;
        node.members = members.unwrap_or(node.members.take());
    }
}

impl VisitMut<RTsTypeAliasDecl> for DceForDts<'_> {
    fn visit_mut(&mut self, node: &mut RTsTypeAliasDecl) {
        node.declare = !self.in_declare;
        node.visit_mut_children_with(self)
    }
}

impl VisitMut<RClassMember> for DceForDts<'_> {
    fn visit_mut(&mut self, node: &mut RClassMember) {
        match node {
            RClassMember::Method(RClassMethod {
                span,
                key,
                is_static,
                accessibility: Some(Accessibility::Private),
                is_abstract,
                is_optional,
                ..
            }) => {
                *node = RClassMember::ClassProp(RClassProp {
                    node_id: NodeId::invalid(),
                    span: *span,
                    declare: false,
                    computed: match key {
                        RPropName::Computed(..) => true,
                        _ => false,
                    },
                    key: box rprop_name_to_expr(key.take()),
                    value: None,
                    type_ann: None,
                    is_static: *is_static,
                    decorators: vec![],
                    accessibility: Some(Accessibility::Private),
                    is_abstract: *is_abstract,
                    is_optional: *is_optional,
                    readonly: false,
                    definite: false,
                    is_override: false,
                });
                return;
            }
            _ => {}
        }

        node.visit_mut_children_with(self)
    }
}

impl VisitMut<RClassDecl> for DceForDts<'_> {
    fn visit_mut(&mut self, node: &mut RClassDecl) {
        node.declare = !self.in_declare;

        let old_class = self.current_class.take();

        if let Some(class) = self.get_mapped(&node.ident.clone().into(), |ty| match ty {
            Type::Class(class) => Some(class.clone()),
            _ => None,
        }) {
            self.current_class = Some(class);
        }

        node.visit_mut_children_with(self);

        self.current_class = old_class;
    }
}

impl VisitMut<RClassProp> for DceForDts<'_> {
    fn visit_mut(&mut self, node: &mut RClassProp) {
        node.value = None;

        if node.accessibility == Some(Accessibility::Private) {
            node.type_ann = None;
        }

        if node.type_ann.is_some() {
            return;
        }

        node.visit_mut_children_with(self)
    }
}

impl VisitMut<Vec<RStmt>> for DceForDts<'_> {
    fn visit_mut(&mut self, stmts: &mut Vec<RStmt>) {
        if !self.top_level {
            stmts.clear();
            return;
        }

        stmts.visit_mut_children_with(self);
    }
}

impl VisitMut<Vec<RModuleItem>> for DceForDts<'_> {
    fn visit_mut(&mut self, items: &mut Vec<RModuleItem>) {
        items.visit_mut_children_with(self);

        items.retain(|item| match item {
            RModuleItem::Stmt(RStmt::Decl(RDecl::TsInterface(..))) if self.in_declare => false,

            RModuleItem::ModuleDecl(_) | RModuleItem::Stmt(RStmt::Decl(..)) => true,

            _ => false,
        });

        if self.top_level && self.forced_module && !self.prevent_empty_export {
            // items.push(RModuleItem::ModuleDecl(RModuleDecl::ExportNamed(
            //     NamedExport {
            //         span: DUMMY_SP,
            //         specifiers: vec![],
            //         src: None,
            //         type_only: false,
            //     },
            // )));
        }
    }
}

impl VisitMut<RModuleItem> for DceForDts<'_> {
    fn visit_mut(&mut self, node: &mut RModuleItem) {
        node.visit_mut_children_with(self);
        let span = node.span();

        match node {
            RModuleItem::Stmt(RStmt::Decl(RDecl::TsInterface(i))) => {
                if self.used.get(&i.id.clone().into()).is_none() {
                    self.forced_module = true;
                    *node = RStmt::Empty(REmptyStmt { span }).into();
                    return;
                }
                return;
            }

            RModuleItem::ModuleDecl(RModuleDecl::ExportDecl(export)) if self.in_declare => {
                *node = RModuleItem::Stmt(RStmt::Decl(export.decl.take()));
                return;
            }
            _ => {}
        }
    }
}

pub fn get_used(info: &ModuleTypeData) -> FxHashSet<Id> {
    let mut used = FxHashSet::default();

    for (sym, v) in info.private_vars.iter() {
        used.insert(sym.clone());
        track(&mut used, v.normalize());
    }

    for (sym, types) in info.private_types.iter() {
        used.insert(sym.clone());
        for ty in types {
            track(&mut used, ty.normalize());
        }
    }

    used
}

fn track<T>(used: &mut FxHashSet<Id>, node: &T)
where
    T: for<'any> VisitWith<Tracker<'any>>,
{
    let mut v = Tracker { used };
    node.visit_with(&mut v);
}

#[derive(Debug)]
struct Tracker<'a> {
    used: &'a mut FxHashSet<Id>,
}

impl Visit<Id> for Tracker<'_> {
    fn visit(&mut self, node: &Id) {
        self.used.insert(node.clone());
    }
}
