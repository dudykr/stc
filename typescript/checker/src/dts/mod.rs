use self::{
    ambient::RealImplRemover,
    dce::{get_used, DceForDts},
};
use crate::{analyzer::Analyzer, loader::ModuleInfo, util::map_with_mut::MapWithMut};
use fxhash::{FxHashMap, FxHashSet};
use stc_types::ModuleTypeData;
use stc_types::{Id, Type};
use swc_common::DUMMY_SP;
use swc_ecma_ast::*;
use swc_ecma_utils::find_ids;
use swc_ecma_visit::{Node, Visit, VisitMut, VisitMutWith, VisitWith};

mod ambient;
mod dce;

/// Make `module` suitable for .d.ts file.
///
/// - Removes function body
pub(super) fn cleanup_module_for_dts(module: &mut Vec<ModuleItem>, type_data: &ModuleTypeData) {
    let is_module = module.iter().any(|item| match item {
        ModuleItem::ModuleDecl(_) => true,
        ModuleItem::Stmt(_) => false,
    });

    module.visit_mut_with(&mut RealImplRemover::default());

    let (used_types, used_vars) = {
        let mut v = TypeUsageCollector {
            phase: CollectorPhase::SearchExports,
            is_module,
            in_module_item: false,
            used_types: Default::default(),
            used_vars: Default::default(),
        };
        module.visit_with(&Invalid { span: DUMMY_SP }, &mut v);

        v.phase = CollectorPhase::AddTypes;
        module.visit_with(&Invalid { span: DUMMY_SP }, &mut v);
        (v.used_types, v.used_vars)
    };

    module.visit_mut_with(&mut Dts {
        preserve_stmt: false,
        used_types,
        used_vars,
    });

    module.visit_mut_with(&mut DceForDts {
        used: get_used(&type_data),
        info: &type_data,
        current_class: None,
        in_declare: false,
        top_level: true,
        forced_module: false,
        prevent_empty_export: false,
    })
}

enum CollectorPhase {
    SearchExports,
    AddTypes,
}

struct TypeUsageCollector {
    phase: CollectorPhase,
    is_module: bool,
    in_module_item: bool,
    /// This includes types **only** used in public interface.
    used_types: FxHashSet<Id>,
    used_vars: FxHashSet<Id>,
}

impl Visit for TypeUsageCollector {
    fn visit_decl(&mut self, decl: &Decl, _: &dyn Node) {
        match self.phase {
            CollectorPhase::SearchExports => {
                if self.is_module && !self.in_module_item {
                    return;
                }
            }
            CollectorPhase::AddTypes => {
                if self.is_module {
                    match decl {
                        Decl::Class(ClassDecl { ident, .. })
                        | Decl::Fn(FnDecl { ident, .. })
                        | Decl::TsInterface(TsInterfaceDecl { id: ident, .. })
                        | Decl::TsTypeAlias(TsTypeAliasDecl { id: ident, .. })
                        | Decl::TsEnum(TsEnumDecl { id: ident, .. }) => {
                            if !self.used_types.contains(&ident.into()) {
                                return;
                            }
                        }
                        Decl::Var(var) => {
                            let ids: Vec<Id> = find_ids(&var.decls);

                            for id in ids {
                                if self.used_types.contains(&id) {
                                    decl.visit_children_with(self);
                                    return;
                                }
                            }
                            return;
                        }
                        Decl::TsModule(_) => {}
                    }
                }
            }
        }

        decl.visit_children_with(self)
    }

    fn visit_export_decl(&mut self, export: &ExportDecl, _: &dyn Node) {
        let old = self.in_module_item;
        self.in_module_item = true;
        export.decl.visit_with(&Invalid { span: DUMMY_SP }, self);
        self.in_module_item = old;
    }

    fn visit_export_default_expr(&mut self, export: &ExportDefaultExpr, _: &dyn Node) {
        match &*export.expr {
            Expr::Ident(i) => {
                self.used_vars.insert(i.into());
            }
            _ => {}
        }
    }

    fn visit_class(&mut self, class: &Class, _: &dyn Node) {
        class.visit_children_with(self);

        fn left_most(e: &Expr) -> Option<Id> {
            match e {
                Expr::Ident(i) => return Some(i.into()),
                Expr::Member(MemberExpr {
                    obj: ExprOrSuper::Expr(e),
                    computed: false,
                    ..
                }) => return left_most(&e),
                _ => None,
            }
        }

        match &class.super_class {
            Some(e) => {
                if let Some(id) = left_most(&e) {
                    self.used_types.insert(id.clone());
                    self.used_vars.insert(id);
                }
            }
            _ => {}
        }
    }

    fn visit_ts_entity_name(&mut self, n: &TsEntityName, _: &dyn Node) {
        match n {
            TsEntityName::TsQualifiedName(_) => {
                n.visit_children_with(self);
            }
            TsEntityName::Ident(i) => {
                let id = Id::from(i);
                // Exports may refer to module-private types.
                self.used_types.insert(id);
            }
        }
    }

    fn visit_opt_block_stmt(&mut self, _: Option<&BlockStmt>, _: &dyn Node) {}
}

macro_rules! remove_opt {
    ($name:ident, $T:ty) => {
        #[inline]
        fn $name(&mut self, v: &mut Option<$T>) {
            *v = None;
        }
    };
}

struct Dts {
    preserve_stmt: bool,
    used_types: FxHashSet<Id>,
    used_vars: FxHashSet<Id>,
}

impl VisitMut for Dts {
    remove_opt!(visit_mut_opt_block_stmt, BlockStmt);

    fn visit_mut_ts_module_decl(&mut self, _: &mut TsModuleDecl) {}

    fn visit_mut_module_items(&mut self, items: &mut Vec<ModuleItem>) {
        let is_module = items.iter().any(|item| match item {
            ModuleItem::ModuleDecl(_) => true,
            ModuleItem::Stmt(_) => false,
        });
        self.preserve_stmt |= !is_module;

        items.visit_mut_children_with(self);

        if is_module {
            items.retain(|item| match item {
                ModuleItem::ModuleDecl(_) => true,
                ModuleItem::Stmt(Stmt::Decl(decl)) => match decl {
                    Decl::Class(ClassDecl { ident, .. })
                    | Decl::Fn(FnDecl { ident, .. })
                    | Decl::TsEnum(TsEnumDecl { id: ident, .. })
                    | Decl::TsTypeAlias(TsTypeAliasDecl { id: ident, .. })
                    | Decl::TsInterface(TsInterfaceDecl { id: ident, .. }) => {
                        self.used_types.contains(&Id::from(ident))
                    }
                    // Handled by `visit_mut_var_decl`
                    Decl::Var(decl) => !decl.decls.is_empty(),
                    Decl::TsModule(_) => true,
                },
                _ => false,
            });
        }

        items.retain(|item| match item {
            ModuleItem::ModuleDecl(ModuleDecl::Import(ImportDecl { specifiers, .. })) => {
                !specifiers.is_empty()
            }
            ModuleItem::Stmt(Stmt::Empty(..)) => false,
            _ => true,
        });
    }

    fn visit_mut_import_specifiers(&mut self, specifiers: &mut Vec<ImportSpecifier>) {
        specifiers.retain(|specifier| match specifier {
            ImportSpecifier::Named(specifier) => {
                self.used_types.contains(&Id::from(&specifier.local))
            }
            ImportSpecifier::Default(specifier) => {
                self.used_types.contains(&Id::from(&specifier.local))
            }
            ImportSpecifier::Namespace(specifier) => {
                self.used_types.contains(&Id::from(&specifier.local))
            }
        });
    }

    fn visit_mut_export_decl(&mut self, export: &mut ExportDecl) {
        let old = self.preserve_stmt;
        self.preserve_stmt = true;
        export.decl.visit_mut_with(self);
        self.preserve_stmt = old;
    }

    fn visit_mut_var_decl(&mut self, decl: &mut VarDecl) {
        decl.visit_mut_children_with(self);
        decl.declare = true;

        if self.preserve_stmt {
            return;
        }

        decl.decls.retain(|decl| {
            let ids: Vec<Id> = find_ids(&decl.name);
            for id in ids {
                if self.used_vars.contains(&id) {
                    return true;
                }
            }

            false
        });
    }

    fn visit_mut_fn_decl(&mut self, decl: &mut FnDecl) {
        decl.declare = true;

        decl.visit_mut_children_with(self);
    }

    fn visit_mut_class_decl(&mut self, decl: &mut ClassDecl) {
        decl.declare = true;

        decl.visit_mut_children_with(self);
    }

    fn visit_mut_ts_type_alias_decl(&mut self, decl: &mut TsTypeAliasDecl) {
        decl.declare = true;

        decl.visit_mut_children_with(self);
    }

    fn visit_mut_ts_enum_decl(&mut self, decl: &mut TsEnumDecl) {
        decl.declare = true;

        decl.visit_mut_children_with(self);
    }

    fn visit_mut_pat(&mut self, pat: &mut Pat) {
        pat.visit_mut_children_with(self);

        match pat {
            Pat::Assign(assign) => {
                *pat = assign.left.take();
                match pat {
                    Pat::Ident(pat) => pat.optional = true,
                    Pat::Array(pat) => pat.optional = true,
                    Pat::Object(pat) => pat.optional = true,
                    _ => {}
                }
            }
            _ => {}
        }
    }

    fn visit_mut_class_members(&mut self, members: &mut Vec<ClassMember>) {
        members.visit_mut_children_with(self);

        members.map_with_mut(|members| {
            let mut props = Vec::with_capacity(members.len() + 6);
            let mut buf = Vec::with_capacity(members.len());
            //
            let mut has_private = false;

            for mut m in members {
                match m {
                    ClassMember::Constructor(ref mut c) => {
                        for p in c.params.iter_mut() {
                            match p {
                                ParamOrTsParamProp::TsParamProp(ref mut p) => {
                                    if p.accessibility.is_some() || p.readonly {
                                        props.push(ClassMember::ClassProp(ClassProp {
                                            span: Default::default(),
                                            declare: false,
                                            key: box match &p.param {
                                                TsParamPropParam::Ident(p) => {
                                                    Expr::Ident(p.clone())
                                                }
                                                TsParamPropParam::Assign(p) => match &p.left {
                                                    //
                                                    box Pat::Ident(i) => Expr::Ident(i.clone()),
                                                    _ => unreachable!(
                                                        "binding pattern in property initializer"
                                                    ),
                                                },
                                            },
                                            value: None,
                                            type_ann: None,
                                            is_static: false,
                                            decorators: vec![],
                                            computed: false,
                                            accessibility: p.accessibility,
                                            is_abstract: false,
                                            is_optional: false,
                                            readonly: p.readonly,
                                            definite: false,
                                        }));
                                    }

                                    p.accessibility = None;
                                    p.readonly = false;

                                    match &mut p.param {
                                        TsParamPropParam::Ident(_) => {}
                                        TsParamPropParam::Assign(AssignPat {
                                            left: box Pat::Ident(i),
                                            ..
                                        }) => {
                                            // Original pattern has default value, so it should be
                                            // option
                                            i.optional = true;
                                            p.param = TsParamPropParam::Ident(i.clone());
                                        }
                                        _ => {}
                                    }
                                }
                                _ => {}
                            }
                        }
                    }
                    ClassMember::Method(ref mut m) => {
                        if let Some(Accessibility::Public) = m.accessibility {
                            m.accessibility = None;
                        }

                        match &m.key {
                            PropName::Computed(e) => match &*e.expr {
                                Expr::Bin(..) => continue,
                                _ => {}
                            },
                            _ => {}
                        }
                    }

                    ClassMember::PrivateMethod(..) | ClassMember::PrivateProp(..) => {
                        has_private = true;
                        continue;
                    }
                    _ => {}
                }

                buf.push(m);
            }

            if has_private {
                props.insert(
                    0,
                    ClassMember::PrivateProp(PrivateProp {
                        span: DUMMY_SP,
                        key: PrivateName {
                            span: DUMMY_SP,
                            id: Ident::new("private".into(), DUMMY_SP),
                        },
                        value: None,
                        type_ann: None,
                        is_static: false,
                        decorators: Default::default(),
                        computed: false,
                        accessibility: None,
                        is_abstract: false,
                        is_optional: false,
                        readonly: false,
                        definite: false,
                    }),
                );
            }

            props.extend(buf);

            props
        })
    }

    fn visit_mut_ts_index_signature(&mut self, sig: &mut TsIndexSignature) {
        sig.visit_mut_children_with(self);

        if sig.type_ann.is_none() {
            sig.type_ann = Some(TsTypeAnn {
                span: DUMMY_SP,
                type_ann: box TsType::TsKeywordType(TsKeywordType {
                    span: DUMMY_SP,
                    kind: TsKeywordTypeKind::TsAnyKeyword,
                }),
            });
        }
    }
}
