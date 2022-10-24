use rnode::{NodeId, VisitWith};
use stc_ts_ast_rnode::{
    RBindingIdent, RDecl, RDefaultDecl, RExportAll, RExportDecl, RExportDefaultDecl,
    RExportDefaultExpr, RExportNamedSpecifier, RExportSpecifier, RExpr, RIdent, RNamedExport, RPat,
    RStmt, RTsExportAssignment, RTsModuleName, RTsTypeAnn, RVarDecl, RVarDeclarator,
};
use stc_ts_errors::{DebugExt, Error};
use stc_ts_file_analyzer_macros::extra_validator;
use stc_ts_types::{Id, IdCtx, ModuleId};
use stc_ts_utils::find_ids_in_pat;
use stc_utils::cache::Freeze;
use swc_atoms::{js_word, JsWord};
use swc_common::{Span, Spanned, DUMMY_SP};
use swc_ecma_ast::*;

use crate::{
    analyzer::{expr::TypeOfMode, scope::VarKind, util::ResultExt, Analyzer, Ctx},
    ty::Type,
    validator,
    validator::ValidateWith,
    ValidationResult,
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
                    a.export_var(
                        f.span(),
                        f.ident.clone().into(),
                        None,
                        f.function.body.is_some(),
                    );
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

                    let ty = e
                        .validate_with(a)
                        .report(&mut a.storage)
                        .map(Type::from)
                        .map(|ty| ty.cheap());
                    let ty = ty.unwrap_or_else(|| Type::any(span, Default::default()));
                    a.register_type(e.id.clone().into(), ty);

                    a.storage
                        .export_type(span, a.ctx.module_id, e.id.clone().into());
                    a.storage.export_var(
                        span,
                        a.ctx.module_id,
                        e.id.clone().into(),
                        e.id.clone().into(),
                    );
                }
                RDecl::TsModule(module) => match &module.id {
                    RTsModuleName::Ident(id) => {
                        module.visit_with(a);

                        a.storage
                            .export_type(span, a.ctx.module_id, id.clone().into());
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
                        todo!("Exporting module with a wildcard: {:?}", module)
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
                let i = f
                    .ident
                    .as_ref()
                    .map(|v| v.into())
                    .unwrap_or_else(|| Id::word(js_word!("default")));
                let fn_ty = match f.function.validate_with_args(self, f.ident.as_ref()) {
                    Ok(ty) => ty,
                    Err(err) => {
                        self.storage.report(err);
                        return Ok(());
                    }
                };
                if f.function.return_type.is_none() {
                    if let Some(m) = &mut self.mutations {
                        if m.for_fns
                            .entry(f.function.node_id)
                            .or_default()
                            .ret_ty
                            .is_none()
                        {
                            m.for_fns.entry(f.function.node_id).or_default().ret_ty =
                                Some(*fn_ty.ret_ty.clone());
                        }
                    }
                }

                self.declare_var(
                    span,
                    VarKind::Fn,
                    i.clone(),
                    Some(fn_ty.into()),
                    None,
                    true,
                    true,
                    false,
                )
                .report(&mut self.storage);

                self.export_var(
                    f.span(),
                    Id::word(js_word!("default")),
                    Some(i),
                    f.function.body.is_some(),
                );
            }
            RDefaultDecl::Class(ref c) => {
                let id: Option<Id> = c.ident.as_ref().map(|v| v.into());
                let orig_name = id.clone();

                self.scope.this_class_name = id.clone();

                let var_name = id.unwrap_or_else(|| Id::word(js_word!("default")));

                let class_ty = c.class.validate_with(self)?;
                let class_ty = Type::ClassDef(class_ty).cheap();
                self.register_type(var_name.clone(), class_ty.clone());

                self.export_type(span, Id::word(js_word!("default")), Some(var_name.clone()));

                self.declare_var(
                    span,
                    VarKind::Class,
                    var_name.clone(),
                    Some(class_ty),
                    None,
                    true,
                    true,
                    false,
                )
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
    #[cfg_attr(debug_assertions, tracing::instrument(skip_all))]
    fn report_errors_for_duplicated_exports_of_var(&mut self, span: Span, sym: JsWord) {
        if self.ctx.reevaluating() {
            return;
        }
        let v = self
            .data
            .for_module
            .exports_spans
            .entry((sym.clone(), IdCtx::Var))
            .or_default();
        v.push(span);

        // TODO(kdy1): Optimize this by emitting same error only once.
        if v.len() >= 2 {
            for &span in &*v {
                self.storage.report(Error::DuplicateDefaultExport { span });
            }
        }
    }

    #[extra_validator]
    #[cfg_attr(debug_assertions, tracing::instrument(skip_all))]
    fn export_var(&mut self, span: Span, name: Id, orig_name: Option<Id>, check_duplicate: bool) {
        if check_duplicate {
            self.report_errors_for_duplicated_exports_of_var(span, name.sym().clone());
        }

        self.storage.export_var(
            span,
            self.ctx.module_id,
            name.clone(),
            orig_name.unwrap_or(name),
        );
    }

    /// Exports a type.
    ///
    /// `scope.regsiter_type` should be called before calling this method.
    ///
    ///
    /// Note: We don't freeze types at here because doing so may prevent proper
    /// finalization.
    #[extra_validator]
    #[cfg_attr(debug_assertions, tracing::instrument(skip_all))]
    fn export_type(&mut self, span: Span, name: Id, orig_name: Option<Id>) {
        let orig_name = orig_name.unwrap_or_else(|| name.clone());

        let types = match self.find_type(self.ctx.module_id, &orig_name) {
            Ok(v) => v,
            Err(err) => {
                self.storage.report(err);
                return;
            }
        };

        let types = match types {
            Some(ty) => ty,
            None => unreachable!(
                ".register_type() should be called before calling .export({})",
                orig_name
            ),
        };

        let iter = types
            .into_iter()
            .map(|v| v.into_owned())
            .map(|v| v.cheap())
            .collect::<Vec<_>>();

        for ty in iter {
            self.storage
                .store_private_type(self.ctx.module_id, name.clone(), ty, false);
        }

        self.storage.export_type(span, self.ctx.module_id, name);
    }

    /// Exports a variable.
    fn export_expr(&mut self, name: Id, item_node_id: NodeId, e: &RExpr) -> ValidationResult<()> {
        self.report_errors_for_duplicated_exports_of_var(e.span(), name.sym().clone());

        let ty = e.validate_with_default(self)?.freezed();

        if *name.sym() == js_word!("default") {
            match e {
                RExpr::Ident(..) => return Ok(()),
                _ => {}
            }
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
                    type_ann: Some(RTsTypeAnn {
                        node_id: NodeId::invalid(),
                        span: DUMMY_SP,
                        type_ann: ty.clone().into(),
                    }),
                }),
                init: None,
                definite: false,
            };
            self.prepend_stmts.push(RStmt::Decl(RDecl::Var(RVarDecl {
                node_id: NodeId::invalid(),
                span: DUMMY_SP,
                kind: VarDeclKind::Const,
                declare: true,
                decls: vec![var],
            })));

            if let Some(m) = &mut self.mutations {
                m.for_export_defaults
                    .entry(item_node_id)
                    .or_default()
                    .replace_with =
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
        let ctx = Ctx {
            allow_namespace_var: true,
            ..self.ctx
        };
        self.with_ctx(ctx)
            .export_expr(Id::word(js_word!("default")), node.node_id, &node.expr)?;

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
            .export_expr(Id::word(js_word!("default")), node.node_id, &node.expr)?;

        Ok(())
    }
}

#[validator]
impl Analyzer<'_, '_> {
    fn validate(&mut self, node: &RExportNamedSpecifier) {
        let ctx = Ctx {
            report_error_for_non_local_vars: true,
            ..self.ctx
        };
        self.with_ctx(ctx).validate_with(|a| {
            a.type_of_var(&node.orig, TypeOfMode::RValue, None)
                .context("failed to reexport with named export specifier")?;

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
                        self.storage.reexport_var(span, dep, id.clone(), ty.clone());
                    }
                    for (id, types) in data.exports.types.iter() {
                        for ty in types {
                            self.storage
                                .reexport_type(span, dep, id.clone(), ty.clone());
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
    fn validate(&mut self, node: &RNamedExport) {
        let span = node.span;
        let base = self.ctx.module_id;

        // Visit export specifiers only if it's not a reexport.
        if node.src.is_none() {
            node.specifiers.visit_with(self);
        }

        for specifier in &node.specifiers {
            match specifier {
                RExportSpecifier::Namespace(_) => {
                    // We need
                    match &node.src {
                        Some(src) => {
                            let (dep, data) = self.get_imported_items(node.span, &src.value);
                        }
                        None => {}
                    }
                }
                RExportSpecifier::Default(_) => {}
                RExportSpecifier::Named(named) => {
                    //

                    match &node.src {
                        Some(src) => {
                            let (dep, data) = self.get_imported_items(node.span, &src.value);

                            self.reexport(
                                span,
                                base,
                                dep,
                                named
                                    .exported
                                    .as_ref()
                                    .map(Id::from)
                                    .unwrap_or_else(|| Id::from(&named.orig)),
                                Id::from(&named.orig),
                            );
                        }
                        None => {
                            self.export_named(
                                span,
                                base,
                                Id::from(&named.orig),
                                named
                                    .exported
                                    .as_ref()
                                    .map(Id::from)
                                    .unwrap_or_else(|| Id::from(&named.orig)),
                            );
                        }
                    }
                }
            }
        }

        Ok(())
    }
}

impl Analyzer<'_, '_> {
    fn export_named(&mut self, span: Span, ctxt: ModuleId, orig: Id, id: Id) {
        if self.storage.get_local_var(ctxt, orig.clone()).is_some() {
            self.report_errors_for_duplicated_exports_of_var(span, id.sym().clone());

            self.storage
                .export_var(span, ctxt, id.clone(), orig.clone());
        }

        if self.storage.get_local_type(ctxt, orig).is_some() {
            self.storage.export_type(span, ctxt, id);
        }
    }

    fn reexport(&mut self, span: Span, ctxt: ModuleId, from: ModuleId, orig: Id, id: Id) {
        let mut did_work = false;

        // Dependency module is not found.
        if ctxt == from {
            return;
        }

        if let Some(data) = self.imports.get(&(ctxt, from)) {
            match data.normalize() {
                Type::Module(data) => {
                    if let Some(ty) = data.exports.vars.get(orig.sym()) {
                        did_work = true;
                        self.storage
                            .reexport_var(span, ctxt, id.sym().clone(), ty.clone());
                    }

                    if let Some(ty) = data.exports.types.get(orig.sym()) {
                        did_work = true;
                        let ty = Type::union(ty.clone());
                        self.storage.reexport_type(span, ctxt, id.sym().clone(), ty);
                    }
                }
                _ => {
                    unreachable!()
                }
            }
        }

        if !did_work {
            self.storage.report(Error::ExportFailed { span, orig, id })
        }
    }
}
