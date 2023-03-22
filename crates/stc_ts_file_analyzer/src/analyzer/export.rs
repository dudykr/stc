use rnode::{NodeId, VisitWith};
use stc_ts_ast_rnode::{
    RBindingIdent, RDecl, RDefaultDecl, RExportAll, RExportDecl, RExportDefaultDecl, RExportDefaultExpr, RExportNamedSpecifier,
    RExportSpecifier, RExpr, RIdent, RModuleExportName, RNamedExport, RPat, RStmt, RTsExportAssignment, RTsModuleName, RTsTypeAnn,
    RVarDecl, RVarDeclarator,
};
use stc_ts_errors::{DebugExt, ErrorKind};
use stc_ts_file_analyzer_macros::extra_validator;
use stc_ts_types::{name::Name, Id, IdCtx, Key};
use stc_ts_utils::find_ids_in_pat;
use stc_utils::{cache::Freeze, dev_span};
use swc_atoms::{js_word, JsWord};
use swc_common::{Span, Spanned, DUMMY_SP};
use swc_ecma_ast::*;

use crate::{
    analyzer::{expr::TypeOfMode, scope::VarKind, util::ResultExt, Analyzer, Ctx},
    ty::Type,
    validator,
    validator::ValidateWith,
    VResult,
};

#[validator]
impl Analyzer<'_, '_> {
    fn validate(&mut self, export: &RExportDecl) {
        let ctx = Ctx {
            in_export_decl: true,
            ..self.ctx
        };
        self.with_ctx(ctx).with(|a: &mut Analyzer| {
            let span = export.span;

            match &export.decl {
                RDecl::Fn(ref f) => {
                    f.visit_with(a);
                    // self.export(f.span(), f.ident.clone().into(), None);
                    a.export_var(f.span(), f.ident.clone().into(), None, f.function.body.is_some());
                }
                RDecl::TsInterface(ref i) => {
                    i.visit_with(a);
                    a.export_type(i.span(), i.id.clone().into(), None)
                }

                RDecl::Class(ref c) => {
                    c.visit_with(a);
                    a.export_type(c.span(), c.ident.clone().into(), None);
                    a.export_var(c.span(), c.ident.clone().into(), None, true);
                }
                RDecl::Var(ref var) => {
                    let span = var.span;
                    var.visit_with(a);

                    let ids: Vec<Id> = find_ids_in_pat(&var.decls);

                    for id in ids {
                        a.export_var(span, id, None, true)
                    }
                }
                RDecl::TsEnum(ref e) => {
                    let span = e.span();

                    let ty = e.validate_with(a).report(&mut a.storage).map(Type::from).map(|ty| ty.freezed());
                    let ty = ty.unwrap_or_else(|| Type::any(span, Default::default()));
                    a.register_type(e.id.clone().into(), ty);

                    a.storage
                        .export_stored_type(span, a.ctx.module_id, e.id.clone().into(), e.id.clone().into());
                    a.storage
                        .export_stored_var(span, a.ctx.module_id, e.id.clone().into(), e.id.clone().into());
                }
                RDecl::TsModule(module) => match &module.id {
                    RTsModuleName::Ident(id) => {
                        module.visit_with(a);
                        a.storage
                            .export_stored_type(span, a.ctx.module_id, id.clone().into(), id.clone().into());
                    }
                    RTsModuleName::Str(..) => {
                        let module: Option<Type> = module.validate_with(a)?;
                        let module = match module {
                            Some(v) => v,
                            None => {
                                unreachable!("global modules cannot be exported")
                            }
                        };
                        // a.storage.export_wildcard_module(s.span, s.value,
                        // module);
                        return Err(ErrorKind::ExportAmbientModule { span }.into());
                    }
                },
                RDecl::TsTypeAlias(ref decl) => {
                    decl.visit_with(a);
                    // export type Foo = 'a' | 'b';
                    // export type Foo = {};

                    // TODO(kdy1): Handle type parameters.
                    a.export_type(span, decl.id.clone().into(), None)
                }
            }

            Ok(())
        })
    }
}

#[validator]
impl Analyzer<'_, '_> {
    fn validate(&mut self, export: &RExportDefaultDecl) {
        let span = export.span();

        match export.decl {
            RDefaultDecl::Fn(ref f) => {
                let i = f.ident.as_ref().map(|v| v.into()).unwrap_or_else(|| Id::word(js_word!("default")));
                let fn_ty = match f.function.validate_with_args(self, f.ident.as_ref()) {
                    Ok(ty) => ty,
                    Err(err) => {
                        self.storage.report(err);
                        return Ok(());
                    }
                };
                if f.function.return_type.is_none() {
                    if let Some(m) = &mut self.mutations {
                        if m.for_fns.entry(f.function.node_id).or_default().ret_ty.is_none() {
                            m.for_fns.entry(f.function.node_id).or_default().ret_ty = Some(*fn_ty.ret_ty.clone());
                        }
                    }
                }

                self.declare_var(span, VarKind::Fn, i.clone(), Some(fn_ty.into()), None, true, true, false, false)
                    .report(&mut self.storage);

                self.export_var(f.span(), Id::word(js_word!("default")), Some(i), f.function.body.is_some());
            }
            RDefaultDecl::Class(ref c) => {
                let id: Option<Id> = c.ident.as_ref().map(|v| v.into());
                let orig_name = id.clone();

                self.scope.this_class_name = id.clone();

                let var_name = id.unwrap_or_else(|| Id::word(js_word!("default")));

                let class_ty = c.class.validate_with_args(self, None)?;
                let class_ty = Type::ClassDef(class_ty).freezed();
                self.register_type(var_name.clone(), class_ty.clone());

                self.export_type(span, Id::word(js_word!("default")), Some(var_name.clone()));

                self.declare_var(span, VarKind::Class, var_name, Some(class_ty), None, true, true, false, false)
                    .report(&mut self.storage);

                self.export_var(c.span(), Id::word(js_word!("default")), orig_name, true);
            }
            RDefaultDecl::TsInterfaceDecl(ref i) => {
                let i = i.id.clone().into();
                export.visit_children_with(self);

                // TODO(kdy1): Register type

                self.export_type(span, Id::word(js_word!("default")), Some(i))
            }
        };

        Ok(())
    }
}

impl Analyzer<'_, '_> {
    /// Currently noop because we need to know if a function is last item among
    /// overloads
    fn report_errors_for_duplicated_exports_of_var(&mut self, span: Span, sym: JsWord) {
        if self.ctx.reevaluating() {
            return;
        }
        let _tracing = dev_span!("report_errors_for_duplicated_exports_of_var");

        let v = self.data.for_module.exports_spans.entry((sym.clone(), IdCtx::Var)).or_default();
        let func_spans = &self.data.fn_impl_spans;
        let is_duplicated_func = func_spans.iter().any(|(_, spans)| spans.len() >= 2);

        v.push(span);

        let is_duplicated_export = v.len() >= 2;
        // TODO(kdy1): Optimize this by emitting same error only once.
        if is_duplicated_export {
            for &span in &*v {
                if sym == js_word!("default") {
                    if is_duplicated_func || func_spans.len() >= 2 {
                        self.storage.report(ErrorKind::DuplicateExport { span }.into());
                    } else {
                        self.storage.report(ErrorKind::DuplicateDefaultExport { span }.into());
                    }
                } else {
                    self.storage.report(ErrorKind::DuplicateExport { span }.into());
                }
            }
        }
    }

    #[extra_validator]
    fn export_var(&mut self, span: Span, name: Id, orig_name: Option<Id>, check_duplicate: bool) {
        let _tracing = dev_span!("export_var", name = tracing::field::debug(&name));

        if check_duplicate {
            self.report_errors_for_duplicated_exports_of_var(span, name.sym().clone());
        }

        self.storage
            .export_stored_var(span, self.ctx.module_id, name.clone(), orig_name.unwrap_or(name));
    }

    /// Exports a type.
    ///
    /// `scope.register_type` should be called before calling this method.
    ///
    ///
    /// Note: We don't freeze types at here because doing so may prevent proper
    /// finalization.
    #[extra_validator]
    fn export_type(&mut self, span: Span, name: Id, orig_name: Option<Id>) {
        let _tracing = dev_span!("export_type", name = tracing::field::debug(&name));

        let orig_name = orig_name.unwrap_or_else(|| name.clone());

        let types = match self.find_type(&orig_name) {
            Ok(v) => v,
            Err(err) => {
                self.storage.report(err);
                return;
            }
        };

        let types = match types {
            Some(ty) => ty,
            None => unreachable!(".register_type() should be called before calling .export({})", orig_name),
        };

        let iter = types.into_iter().map(|v| v.into_owned()).map(|v| v.freezed()).collect::<Vec<_>>();
        for ty in iter {
            self.storage.store_private_type(self.ctx.module_id, name.clone(), ty, false);
        }

        self.storage.export_stored_type(span, self.ctx.module_id, name, orig_name);
    }

    /// Exports a variable.
    fn export_expr(&mut self, span: Span, name: JsWord, item_node_id: NodeId, e: &RExpr) -> VResult<()> {
        self.report_errors_for_duplicated_exports_of_var(e.span(), name.clone());

        let ty = e.validate_with_default(self)?.freezed();

        self.storage.export_var(span, self.ctx.module_id, name.clone(), ty.clone());

        if name == js_word!("default") {
            let var = RVarDeclarator {
                node_id: NodeId::invalid(),
                span: DUMMY_SP,
                name: RPat::Ident(RBindingIdent {
                    node_id: NodeId::invalid(),
                    id: RIdent {
                        node_id: NodeId::invalid(),
                        span: DUMMY_SP,
                        sym: "_default".into(),
                        optional: false,
                    },
                    type_ann: Some(box RTsTypeAnn {
                        node_id: NodeId::invalid(),
                        span: DUMMY_SP,
                        type_ann: ty.into(),
                    }),
                }),
                init: None,
                definite: false,
            };
            self.data.prepend_stmts.push(RStmt::Decl(RDecl::Var(box RVarDecl {
                node_id: NodeId::invalid(),
                span: DUMMY_SP,
                kind: VarDeclKind::Const,
                declare: true,
                decls: vec![var],
            })));

            if let Some(m) = &mut self.mutations {
                m.for_export_defaults.entry(item_node_id).or_default().replace_with =
                    Some(box RExpr::Ident(RIdent::new("_default".into(), DUMMY_SP)));
            }

            return Ok(());
        }

        Ok(())
    }
}

/// Done
#[validator]
impl Analyzer<'_, '_> {
    fn validate(&mut self, node: &RTsExportAssignment) {
        let span = node.span;
        let ctx = Ctx {
            in_export_assignment: true,
            ..self.ctx
        };
        self.with_ctx(ctx)
            .export_expr(node.span, js_word!("default"), node.node_id, &node.expr)?;

        if let Ok(name) = Name::try_from(&*node.expr) {
            let ty = self.type_of_name(span, &name, TypeOfMode::RValue, None);
            if let Ok(ty) = ty {
                self.storage.export_type(span, self.ctx.module_id, js_word!("default"), ty);
            }
        }

        Ok(())
    }
}

/// Done
#[validator]
impl Analyzer<'_, '_> {
    fn validate(&mut self, node: &RExportDefaultExpr) {
        let ctx = Ctx {
            in_export_default_expr: true,
            ..self.ctx
        };
        self.with_ctx(ctx)
            .export_expr(node.span, js_word!("default"), node.node_id, &node.expr)?;

        Ok(())
    }
}

#[validator]
impl Analyzer<'_, '_> {
    fn validate(&mut self, node: &RExportNamedSpecifier) {
        let ctx = Ctx {
            report_error_for_non_local_vars: true,
            in_export_named: true,
            ..self.ctx
        };
        self.with_ctx(ctx).validate_with(|a| {
            let ident = match &node.orig {
                RModuleExportName::Ident(v) => v.clone(),
                RModuleExportName::Str(v) => RIdent::new(v.value.clone(), v.span),
            };

            match &*ident.sym {
                "any" | "never" | "unknown" | "string" | "number" | "bigint" | "boolean" | "undefined" | "symbol" => {
                    return Err(ErrorKind::CannotExportNonLocalVar { span: ident.span }.into())
                }
                _ => a
                    .type_of_var(&ident, TypeOfMode::RValue, None)
                    .context("tried to reexport with named export specifier")?,
            };

            Ok(())
        });

        // TODO(kdy1): Add an export

        Ok(())
    }
}

#[validator]
impl Analyzer<'_, '_> {
    fn validate(&mut self, node: &RExportAll) {
        let span = node.span;
        let ctxt = self.ctx.module_id;

        let (dep, data) = self.get_imported_items(span, &node.src.value);

        if ctxt != dep {
            match data.normalize() {
                Type::Module(data) => {
                    for (id, ty) in data.exports.vars.iter() {
                        self.storage.export_var(span, dep, id.clone(), ty.clone());
                    }
                    for (id, types) in data.exports.types.iter() {
                        for ty in types {
                            self.storage.export_type(span, dep, id.clone(), ty.clone());
                        }
                    }
                }
                _ => {
                    unreachable!()
                }
            }
        }
        Ok(())
    }
}

#[validator]
impl Analyzer<'_, '_> {
    fn validate(&mut self, node: &RNamedExport) -> VResult<()> {
        self.validate_named_export(node)
    }
}

impl Analyzer<'_, '_> {
    fn validate_named_export(&mut self, node: &RNamedExport) -> VResult<()> {
        let span = node.span;
        let base = self.ctx.module_id;

        // Visit export specifiers only if it's not a reexport.
        if node.src.is_none() {
            node.specifiers.visit_with(self);
        }

        match &node.src {
            Some(src) => {
                let (dep, data) = self.get_imported_items(node.span, &src.value);

                for specifier in &node.specifiers {
                    match specifier {
                        RExportSpecifier::Namespace(s) => {
                            // We need
                            match &node.src {
                                Some(src) => {
                                    let name = match &s.name {
                                        RModuleExportName::Ident(v) => v.sym.clone(),
                                        RModuleExportName::Str(v) => v.value.clone(),
                                    };

                                    self.storage.export_type(s.span, self.ctx.module_id, name.clone(), data.clone());
                                    self.storage.export_var(s.span, self.ctx.module_id, name, data.clone());
                                }
                                None => {}
                            }
                        }
                        RExportSpecifier::Default(named) => {}
                        RExportSpecifier::Named(named) => {
                            let span = named.span;

                            let orig_sym = match &named.orig {
                                RModuleExportName::Ident(v) => v.sym.clone(),
                                RModuleExportName::Str(v) => v.value.clone(),
                            };

                            let export_sym = named.exported.as_ref().unwrap_or(&named.orig);
                            let export_sym = match export_sym {
                                RModuleExportName::Ident(v) => v.sym.clone(),
                                RModuleExportName::Str(v) => v.value.clone(),
                            };

                            let var_result = self.access_property(
                                span,
                                &data,
                                &Key::Normal {
                                    span,
                                    sym: orig_sym.clone(),
                                },
                                TypeOfMode::RValue,
                                IdCtx::Var,
                                Default::default(),
                            );
                            let type_result = self.access_property(
                                span,
                                &data,
                                &Key::Normal { span, sym: orig_sym },
                                TypeOfMode::RValue,
                                IdCtx::Var,
                                Default::default(),
                            );

                            if let Ok(mut ty) = var_result {
                                ty.freeze();

                                self.storage.export_var(span, self.ctx.module_id, export_sym.clone(), ty);
                            }

                            if let Ok(mut ty) = type_result {
                                ty.freeze();

                                self.storage.export_type(span, self.ctx.module_id, export_sym, ty);
                            }
                        }
                    }
                }
            }
            None => {
                for specifier in &node.specifiers {
                    match specifier {
                        RExportSpecifier::Namespace(s) => {
                            unreachable!("namespace export should be handled in `export_all`")
                        }
                        RExportSpecifier::Default(named) => {}
                        RExportSpecifier::Named(named) => {
                            let export_sym = named.exported.as_ref().unwrap_or(&named.orig);
                            let export_sym = match export_sym {
                                RModuleExportName::Ident(v) => v.sym.clone(),
                                RModuleExportName::Str(v) => v.value.clone(),
                            };

                            let orig = match &named.orig {
                                RModuleExportName::Ident(v) => v.clone(),
                                RModuleExportName::Str(v) => unreachable!(),
                            };
                            //
                            let var_result = self.type_of_var(&orig, TypeOfMode::RValue, None);
                            let type_result = self.type_of_ts_entity_name(span, &RExpr::Ident(orig.clone()), None);

                            if let Ok(mut ty) = var_result {
                                ty.freeze();

                                self.storage.export_var(span, self.ctx.module_id, export_sym.clone(), ty);
                            }

                            if let Ok(mut ty) = type_result {
                                ty.freeze();

                                self.storage.export_type(span, self.ctx.module_id, export_sym, ty);
                            }
                        }
                    }
                }
            }
        }

        Ok(())
    }
}
