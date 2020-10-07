//! Dead code elimination for types.
use crate::util::{map_with_mut::MapWithMut, PatExt};
use fxhash::FxHashSet;
use stc_types::{Id, ModuleTypeData, Type, TypeNode};
use std::sync::Arc;
use swc_common::{util::move_map::MoveMap, Spanned, DUMMY_SP};
use swc_ecma_ast::*;
use swc_ecma_utils::{prop_name_to_expr, StmtLike};
use swc_ecma_visit::{Fold, FoldWith, VisitMut, VisitMutWith};

#[derive(Debug)]
pub(super) struct DceForDts<'a> {
    pub info: &'a ModuleTypeData,
    pub used: FxHashSet<Id>,
    pub current_class: Option<stc_types::Class>,

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

            types.iter().filter_map(|ty| pred(ty.normalize())).next()
        } else {
            None
        }
    }
}

impl VisitMut for DceForDts<'_> {
    fn visit_mut_block_stmt(&mut self, mut node: &mut BlockStmt) {
        let old = self.top_level;
        self.top_level = false;
        node.visit_mut_children_with(self);
        self.top_level = old;
    }

    fn visit_mut_function(&mut self, mut node: &mut Function) {
        node.is_generator = false;
        node.is_async = false;

        let old = self.top_level;
        self.top_level = false;
        node.visit_mut_children_with(self);
        self.top_level = old;
    }

    fn visit_mut_var_decl(&mut self, node: &mut VarDecl) {
        node.visit_mut_children_with(self);
        node.declare = true;

        node.decls.retain(|v| match v.name {
            Pat::Invalid(..) => false,
            _ => true,
        });

        if node.kind != VarDeclKind::Const {
            node.decls.iter_mut().for_each(|node| match node.init {
                Some(box Expr::Lit(Lit::Num(..))) => {
                    node.init = None;
                    node.name
                        .set_ty(Some(box TsType::TsKeywordType(TsKeywordType {
                            span: DUMMY_SP,
                            kind: TsKeywordTypeKind::TsNumberKeyword,
                        })))
                }
                _ => {}
            });
        }
    }

    fn visit_mut_var_declarator(&mut self, node: &mut VarDeclarator) {
        if match &node.name {
            Pat::Array(arr) => arr.elems.is_empty(),
            Pat::Object(obj) => obj.props.is_empty(),
            _ => false,
        } {
            node.name = Pat::Invalid(Invalid { span: DUMMY_SP });
            node.init = None;
            return;
        }

        match node.init {
            Some(box Expr::Lit(Lit::Null(..))) | Some(box Expr::Lit(Lit::JSXText(..))) => {
                node.init = None;
            }

            Some(box Expr::Lit(..)) => {}
            _ => {
                node.init = None;
            }
        };

        if node.init.is_some() {
            node.name.set_ty(None);
            return;
        }

        match node.name {
            Pat::Ident(ref mut i) => {
                if i.type_ann.is_none() {
                    if let Some(ty) = self.info.private_vars.get(&i.clone().into()) {
                        i.type_ann = Some(TsTypeAnn {
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

    fn visit_mut_pat(&mut self, node: &mut Pat) {
        node.visit_mut_children_with(self);

        match node {
            Pat::Assign(a) => *node = a.left.take(),
            _ => {}
        }
    }

    fn visit_mut_fn_decl(&mut self, node: &mut FnDecl) {
        node.declare = !self.in_declare;

        node.visit_mut_children_with(self);

        if node.function.return_type.is_some() {
            return;
        }

        node.function.return_type = self.get_mapped(&node.ident.clone().into(), |ty| match ty {
            Type::Function(stc_types::Function { ref ret_ty, .. }) => {
                Some(TsTypeAnn::from((**ret_ty).clone()))
            }
            _ => None,
        });
    }

    fn visit_mut_ts_module_decl(&mut self, node: &mut TsModuleDecl) {
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

    fn visit_mut_ts_enum_decl(&mut self, node: &mut TsEnumDecl) {
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
                    if e.members.iter().any(|m| match m.val {
                        Expr::Tpl(..) | Expr::Lit(..) => false,

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
                    .map(|(i, member)| TsEnumMember {
                        span: member.span,
                        id: member.id.clone(),
                        init: if is_all_lit {
                            if has_no_init {
                                Some(box member.val.clone())
                            } else {
                                if should_init_only_first {
                                    if i == 0 {
                                        Some(box member.val.clone())
                                    } else {
                                        None
                                    }
                                } else {
                                    Some(box member.val.clone())
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

    fn visit_mut_ts_type_alias_decl(&mut self, node: &mut TsTypeAliasDecl) {
        node.declare = !self.in_declare;
        node.visit_mut_children_with(self)
    }

    fn visit_mut_class_member(&mut self, node: &mut ClassMember) {
        match node {
            ClassMember::Method(ClassMethod {
                span,
                key,
                is_static,
                accessibility: Some(Accessibility::Private),
                is_abstract,
                is_optional,
                ..
            }) => {
                *node = ClassMember::ClassProp(ClassProp {
                    span: *span,
                    declare: false,
                    computed: match key {
                        PropName::Computed(..) => true,
                        _ => false,
                    },
                    key: box prop_name_to_expr(key.take()),
                    value: None,
                    type_ann: None,
                    is_static: *is_static,
                    decorators: vec![],
                    accessibility: Some(Accessibility::Private),
                    is_abstract: *is_abstract,
                    is_optional: *is_optional,
                    readonly: false,
                    definite: false,
                });
                return;
            }
            _ => {}
        }

        node.visit_mut_children_with(self)
    }

    fn visit_mut_class_decl(&mut self, node: &mut ClassDecl) {
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

    fn visit_mut_class_prop(&mut self, node: &mut ClassProp) {
        node.value = None;

        if node.accessibility == Some(Accessibility::Private) {
            node.type_ann = None;
        }

        if node.type_ann.is_some() {
            return;
        }

        node.visit_mut_children_with(self)
    }

    fn visit_mut_stmts(&mut self, stmts: &mut Vec<Stmt>) {
        if !self.top_level {
            stmts.clear();
            return;
        }

        stmts.visit_mut_children_with(self);
    }

    fn visit_mut_module_items(&mut self, items: &mut Vec<ModuleItem>) {
        items.visit_mut_children_with(self);

        items.retain(|item| match item {
            ModuleItem::Stmt(Stmt::Decl(Decl::TsInterface(..))) if self.in_declare => false,

            ModuleItem::ModuleDecl(_) | ModuleItem::Stmt(Stmt::Decl(..)) => true,

            _ => false,
        });

        if self.top_level && self.forced_module && !self.prevent_empty_export {
            // items.push(ModuleItem::ModuleDecl(ModuleDecl::ExportNamed(
            //     NamedExport {
            //         span: DUMMY_SP,
            //         specifiers: vec![],
            //         src: None,
            //         type_only: false,
            //     },
            // )));
        }
    }

    fn visit_mut_module_item(&mut self, node: &mut ModuleItem) {
        node.visit_mut_children_with(self);
        let span = node.span();

        match node {
            ModuleItem::Stmt(Stmt::Decl(Decl::TsInterface(i))) => {
                if self.used.get(&i.id.clone().into()).is_none() {
                    self.forced_module = true;
                    *node = Stmt::Empty(EmptyStmt { span }).into();
                    return;
                }
                return;
            }

            ModuleItem::ModuleDecl(ModuleDecl::ExportDecl(export)) if self.in_declare => {
                *node = ModuleItem::Stmt(Stmt::Decl(export.decl.take()));
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
    T: for<'any> stc_types::VisitWith<Tracker<'any>>,
{
    let mut v = Tracker { used };
    node.visit_with(&node, &mut v);
}

#[derive(Debug)]
struct Tracker<'a> {
    used: &'a mut FxHashSet<Id>,
}

impl stc_types::Visit for Tracker<'_> {
    fn visit_id(&mut self, node: &Id, _: &dyn TypeNode) {
        self.used.insert(node.clone());
    }
}
