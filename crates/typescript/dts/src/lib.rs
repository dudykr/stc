//! Passes for generating dts.

#![allow(incomplete_features)]
#![feature(box_syntax)]
#![feature(box_patterns)]
#![feature(specialization)]

use self::{
    ambient::RealImplRemover,
    dce::{get_used, DceForDts},
};
use fxhash::FxHashSet;
use rnode::NodeId;
use rnode::Visit;
use rnode::VisitMut;
use rnode::VisitMutWith;
use rnode::VisitWith;
use stc_ts_ast_rnode::RAssignPat;
use stc_ts_ast_rnode::RBlockStmt;
use stc_ts_ast_rnode::RClass;
use stc_ts_ast_rnode::RClassDecl;
use stc_ts_ast_rnode::RClassMember;
use stc_ts_ast_rnode::RClassProp;
use stc_ts_ast_rnode::RDecl;
use stc_ts_ast_rnode::RExportDecl;
use stc_ts_ast_rnode::RExportDefaultExpr;
use stc_ts_ast_rnode::RExpr;
use stc_ts_ast_rnode::RExprOrSuper;
use stc_ts_ast_rnode::RFnDecl;
use stc_ts_ast_rnode::RIdent;
use stc_ts_ast_rnode::RImportDecl;
use stc_ts_ast_rnode::RImportSpecifier;
use stc_ts_ast_rnode::RMemberExpr;
use stc_ts_ast_rnode::RModuleDecl;
use stc_ts_ast_rnode::RModuleItem;
use stc_ts_ast_rnode::RParamOrTsParamProp;
use stc_ts_ast_rnode::RPat;
use stc_ts_ast_rnode::RPrivateName;
use stc_ts_ast_rnode::RPrivateProp;
use stc_ts_ast_rnode::RPropName;
use stc_ts_ast_rnode::RStmt;
use stc_ts_ast_rnode::RTsEntityName;
use stc_ts_ast_rnode::RTsEnumDecl;
use stc_ts_ast_rnode::RTsIndexSignature;
use stc_ts_ast_rnode::RTsInterfaceDecl;
use stc_ts_ast_rnode::RTsKeywordType;
use stc_ts_ast_rnode::RTsModuleDecl;
use stc_ts_ast_rnode::RTsParamPropParam;
use stc_ts_ast_rnode::RTsType;
use stc_ts_ast_rnode::RTsTypeAliasDecl;
use stc_ts_ast_rnode::RTsTypeAnn;
use stc_ts_ast_rnode::RVarDecl;
use stc_ts_types::Id;
use stc_ts_types::ModuleTypeData;
use stc_ts_utils::find_ids_in_pat;
use stc_ts_utils::MapWithMut;
use swc_common::DUMMY_SP;
use swc_ecma_ast::*;

mod ambient;
mod dce;

/// Make `module` suitable for .d.ts file.
///
/// - Removes function body
pub fn cleanup_module_for_dts(module: &mut Vec<RModuleItem>, type_data: &ModuleTypeData) {
    let is_module = module.iter().any(|item| match item {
        RModuleItem::ModuleDecl(_) => true,
        RModuleItem::Stmt(_) => false,
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
        module.visit_with(&mut v);

        v.phase = CollectorPhase::AddTypes;
        module.visit_with(&mut v);
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

impl Visit<RDecl> for TypeUsageCollector {
    fn visit(&mut self, decl: &RDecl) {
        match self.phase {
            CollectorPhase::SearchExports => {
                if self.is_module && !self.in_module_item {
                    return;
                }
            }
            CollectorPhase::AddTypes => {
                if self.is_module {
                    match decl {
                        RDecl::Class(RClassDecl { ident, .. })
                        | RDecl::Fn(RFnDecl { ident, .. })
                        | RDecl::TsInterface(RTsInterfaceDecl { id: ident, .. })
                        | RDecl::TsTypeAlias(RTsTypeAliasDecl { id: ident, .. })
                        | RDecl::TsEnum(RTsEnumDecl { id: ident, .. }) => {
                            if !self.used_types.contains(&ident.into()) {
                                return;
                            }
                        }
                        RDecl::Var(var) => {
                            let ids: Vec<Id> = find_ids_in_pat(&var.decls);

                            for id in ids {
                                if self.used_types.contains(&id) {
                                    decl.visit_children_with(self);
                                    return;
                                }
                            }
                            return;
                        }
                        RDecl::TsModule(_) => {}
                    }
                }
            }
        }

        decl.visit_children_with(self)
    }
}

impl Visit<RExportDecl> for TypeUsageCollector {
    fn visit(&mut self, export: &RExportDecl) {
        let old = self.in_module_item;
        self.in_module_item = true;
        export.decl.visit_with(self);
        self.in_module_item = old;
    }
}

impl Visit<RExportDefaultExpr> for TypeUsageCollector {
    fn visit(&mut self, export: &RExportDefaultExpr) {
        match &*export.expr {
            RExpr::Ident(i) => {
                self.used_vars.insert(i.into());
            }
            _ => {}
        }
    }
}

impl Visit<RClass> for TypeUsageCollector {
    fn visit(&mut self, class: &RClass) {
        class.visit_children_with(self);

        fn left_most(e: &RExpr) -> Option<Id> {
            match e {
                RExpr::Ident(i) => return Some(i.into()),
                RExpr::Member(RMemberExpr {
                    obj: RExprOrSuper::Expr(e),
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
}

impl Visit<RTsEntityName> for TypeUsageCollector {
    fn visit(&mut self, n: &RTsEntityName) {
        match n {
            RTsEntityName::TsQualifiedName(_) => {
                n.visit_children_with(self);
            }
            RTsEntityName::Ident(i) => {
                let id = Id::from(i);
                // Exports may refer to module-private types.
                self.used_types.insert(id);
            }
        }
    }
}

impl Visit<Option<RBlockStmt>> for TypeUsageCollector {
    fn visit(&mut self, _: &Option<RBlockStmt>) {}
}

struct Dts {
    preserve_stmt: bool,
    used_types: FxHashSet<Id>,
    used_vars: FxHashSet<Id>,
}

impl VisitMut<Option<RBlockStmt>> for Dts {
    fn visit_mut(&mut self, value: &mut Option<RBlockStmt>) {
        *value = None;
    }
}

impl VisitMut<RTsModuleDecl> for Dts {
    fn visit_mut(&mut self, _: &mut RTsModuleDecl) {}
}

impl VisitMut<Vec<RModuleItem>> for Dts {
    fn visit_mut(&mut self, items: &mut Vec<RModuleItem>) {
        let is_module = items.iter().any(|item| match item {
            RModuleItem::ModuleDecl(_) => true,
            RModuleItem::Stmt(_) => false,
        });
        self.preserve_stmt |= !is_module;

        items.visit_mut_children_with(self);

        if is_module {
            items.retain(|item| match item {
                RModuleItem::ModuleDecl(_) => true,
                RModuleItem::Stmt(RStmt::Decl(decl)) => match decl {
                    RDecl::Class(RClassDecl { ident, .. })
                    | RDecl::Fn(RFnDecl { ident, .. })
                    | RDecl::TsEnum(RTsEnumDecl { id: ident, .. })
                    | RDecl::TsTypeAlias(RTsTypeAliasDecl { id: ident, .. })
                    | RDecl::TsInterface(RTsInterfaceDecl { id: ident, .. }) => {
                        self.used_types.contains(&Id::from(ident))
                    }
                    // Handled by `visit_mut_var_decl`
                    RDecl::Var(decl) => !decl.decls.is_empty(),
                    RDecl::TsModule(_) => true,
                },
                _ => false,
            });
        }

        items.retain(|item| match item {
            RModuleItem::ModuleDecl(RModuleDecl::Import(RImportDecl { specifiers, .. })) => {
                !specifiers.is_empty()
            }
            RModuleItem::Stmt(RStmt::Empty(..)) => false,
            _ => true,
        });
    }
}

impl VisitMut<Vec<RImportSpecifier>> for Dts {
    fn visit_mut(&mut self, specifiers: &mut Vec<RImportSpecifier>) {
        specifiers.retain(|specifier| match specifier {
            RImportSpecifier::Named(specifier) => {
                self.used_types.contains(&Id::from(&specifier.local))
            }
            RImportSpecifier::Default(specifier) => {
                self.used_types.contains(&Id::from(&specifier.local))
            }
            RImportSpecifier::Namespace(specifier) => {
                self.used_types.contains(&Id::from(&specifier.local))
            }
        });
    }
}

impl VisitMut<RExportDecl> for Dts {
    fn visit_mut(&mut self, export: &mut RExportDecl) {
        let old = self.preserve_stmt;
        self.preserve_stmt = true;
        export.decl.visit_mut_with(self);
        self.preserve_stmt = old;
    }
}

impl VisitMut<RVarDecl> for Dts {
    fn visit_mut(&mut self, decl: &mut RVarDecl) {
        decl.visit_mut_children_with(self);
        decl.declare = true;

        if self.preserve_stmt {
            return;
        }

        decl.decls.retain(|decl| {
            let ids: Vec<Id> = find_ids_in_pat(&decl.name);
            for id in ids {
                if self.used_vars.contains(&id) {
                    return true;
                }
            }

            false
        });
    }
}

impl VisitMut<RFnDecl> for Dts {
    fn visit_mut(&mut self, decl: &mut RFnDecl) {
        decl.declare = true;

        decl.visit_mut_children_with(self);
    }
}

impl VisitMut<RClassDecl> for Dts {
    fn visit_mut(&mut self, decl: &mut RClassDecl) {
        decl.declare = true;

        decl.visit_mut_children_with(self);
    }
}

impl VisitMut<RTsTypeAliasDecl> for Dts {
    fn visit_mut(&mut self, decl: &mut RTsTypeAliasDecl) {
        decl.declare = true;

        decl.visit_mut_children_with(self);
    }
}

impl VisitMut<RTsEnumDecl> for Dts {
    fn visit_mut(&mut self, decl: &mut RTsEnumDecl) {
        decl.declare = true;

        decl.visit_mut_children_with(self);
    }
}

impl VisitMut<RPat> for Dts {
    fn visit_mut(&mut self, pat: &mut RPat) {
        pat.visit_mut_children_with(self);

        match pat {
            RPat::Assign(assign) => {
                *pat = assign.left.take();
                match pat {
                    RPat::Ident(pat) => pat.optional = true,
                    RPat::Array(pat) => pat.optional = true,
                    RPat::Object(pat) => pat.optional = true,
                    _ => {}
                }
            }
            _ => {}
        }
    }
}

impl VisitMut<Vec<RClassMember>> for Dts {
    fn visit_mut(&mut self, members: &mut Vec<RClassMember>) {
        members.visit_mut_children_with(self);

        members.map_with_mut(|members| {
            let mut props = Vec::with_capacity(members.len() + 6);
            let mut buf = Vec::with_capacity(members.len());
            //
            let mut has_private = false;

            for mut m in members {
                match m {
                    RClassMember::Constructor(ref mut c) => {
                        for p in c.params.iter_mut() {
                            match p {
                                RParamOrTsParamProp::TsParamProp(ref mut p) => {
                                    if p.accessibility.is_some() || p.readonly {
                                        props.push(RClassMember::ClassProp(RClassProp {
                                            node_id: NodeId::invalid(),
                                            span: Default::default(),
                                            declare: false,
                                            key: box match &p.param {
                                                RTsParamPropParam::Ident(p) => {
                                                    RExpr::Ident(p.clone())
                                                }
                                                RTsParamPropParam::Assign(p) => match &p.left {
                                                    //
                                                    box RPat::Ident(i) => RExpr::Ident(i.clone()),
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
                                        RTsParamPropParam::Ident(_) => {}
                                        RTsParamPropParam::Assign(RAssignPat {
                                            left: box RPat::Ident(i),
                                            ..
                                        }) => {
                                            // Original pattern has default value, so it should be
                                            // option
                                            i.optional = true;
                                            p.param = RTsParamPropParam::Ident(i.clone());
                                        }
                                        _ => {}
                                    }
                                }
                                _ => {}
                            }
                        }
                    }
                    RClassMember::Method(ref mut m) => {
                        if let Some(Accessibility::Public) = m.accessibility {
                            m.accessibility = None;
                        }

                        match &m.key {
                            RPropName::Computed(e) => match &*e.expr {
                                RExpr::Bin(..) => continue,
                                _ => {}
                            },
                            _ => {}
                        }
                    }

                    RClassMember::PrivateMethod(..) | RClassMember::PrivateProp(..) => {
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
                    RClassMember::PrivateProp(RPrivateProp {
                        node_id: NodeId::invalid(),
                        span: DUMMY_SP,
                        key: RPrivateName {
                            node_id: NodeId::invalid(),
                            span: DUMMY_SP,
                            id: RIdent::new("private".into(), DUMMY_SP),
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
}

impl VisitMut<RTsIndexSignature> for Dts {
    fn visit_mut(&mut self, sig: &mut RTsIndexSignature) {
        sig.visit_mut_children_with(self);

        if sig.type_ann.is_none() {
            sig.type_ann = Some(RTsTypeAnn {
                node_id: NodeId::invalid(),
                span: DUMMY_SP,
                type_ann: box RTsType::TsKeywordType(RTsKeywordType {
                    span: DUMMY_SP,
                    kind: TsKeywordTypeKind::TsAnyKeyword,
                }),
            });
        }
    }
}
