use super::{Analyzer, Ctx};
use crate::{analyzer::util::ResultExt, ty::Type, validator, validator::ValidateWith, ValidationResult};
use rnode::NodeId;
use rnode::VisitWith;
use stc_ts_ast_rnode::RDecl;
use stc_ts_ast_rnode::RDefaultDecl;
use stc_ts_ast_rnode::RExportAll;
use stc_ts_ast_rnode::RExportDecl;
use stc_ts_ast_rnode::RExportDefaultDecl;
use stc_ts_ast_rnode::RExportDefaultExpr;
use stc_ts_ast_rnode::RExportSpecifier;
use stc_ts_ast_rnode::RExpr;
use stc_ts_ast_rnode::RIdent;
use stc_ts_ast_rnode::RNamedExport;
use stc_ts_ast_rnode::RPat;
use stc_ts_ast_rnode::RStmt;
use stc_ts_ast_rnode::RTsExportAssignment;
use stc_ts_ast_rnode::RTsModuleName;
use stc_ts_ast_rnode::RTsTypeAnn;
use stc_ts_ast_rnode::RVarDecl;
use stc_ts_ast_rnode::RVarDeclarator;
use stc_ts_errors::Error;
use stc_ts_file_analyzer_macros::extra_validator;
use stc_ts_types::Id;
use stc_ts_types::ModuleId;
use stc_ts_utils::find_ids_in_pat;
use swc_atoms::js_word;
use swc_common::{Span, Spanned, DUMMY_SP};
use swc_ecma_ast::*;

// RModuleDecl::ExportNamed(export) => {}
//
// RModuleDecl::ExportAll(export) => unimplemented!("export * from
// 'other-file';"),
//
// RModuleDecl::TsNamespaceExport(ns) =>
// unimplemented!("export namespace"),

impl Analyzer<'_, '_> {
    /// This methods exports unresolved expressions, which depends on
    /// expressions that comes after the expression.
    pub(super) fn handle_pending_exports(&mut self) {
        // let pending_exports: Vec<_> = replace(&mut self.pending_exports,
        // Default::default());

        // for ((sym, span), mut expr) in pending_exports {
        //     // TODO: Allow multiple exports with same name.

        //     let tmp;
        //     let exported_sym = if sym.as_str() != "default" {
        //         Some(&sym)
        //     } else {
        //         match expr {
        //             RExpr::Ident(ref i) => {
        //                 tmp = i.clone().into();
        //                 Some(&tmp)
        //             }
        //             _ => None,
        //         }
        //     };
        //     let ty = match exported_sym
        //         .and_then(|exported_sym|
        // self.scope.types.remove(&exported_sym))     {
        //         Some(export) => {
        //             for ty in export {
        //                 self.storage.store_private_type(self.ctx.module_id,
        // sym, ty);             }

        //             self.storage.export_type(span, self.ctx.module_id, sym);
        //         }
        //         None => match expr.validate_with_default(self) {
        //             Ok(ty) => {
        //                 self.storage.store_private_type(self.ctx.module_id,
        // sym, ty);

        //                 self.storage.export_type(span, self.ctx.module_id,
        // sym);             }
        //             Err(err) => {
        //                 self.storage.report(err);
        //             }
        //         },
        //     };
        // }

        // assert_eq!(self.pending_exports, vec![]);

        // if self.info.exports.types.is_empty() &&
        // self.info.exports.vars.is_empty() {     self.info
        //         .exports
        //         .vars
        //         .extend(self.scope.vars.drain().map(|(k, v)| {
        //             (
        //                 k,
        //                 v.ty.map(|ty| ty.cheap())
        //                     .unwrap_or_else(|| Type::any(DUMMY_SP)),
        //             )
        //         }));
        //     self.info.exports.types.extend(
        //         self.scope
        //             .types
        //             .drain()
        //             .map(|(k, v)| (k, v.into_iter().map(|v|
        // v.cheap()).collect())),     );
        // }
    }

    pub(super) fn export_default_expr(&mut self, expr: &mut RExpr) {
        let span = expr.span();
        // assert_eq!(
        //     self.info.exports.vars.get(&Id::word(js_word!("default"))),
        //     None,
        //     "A module can export only one item as default"
        // );

        let ty = match expr.validate_with_default(self) {
            Ok(ty) => ty,
            Err(err) => {
                match err {
                    // Handle hoisting. This allows
                    //
                    // export = React
                    // declare namespace React {}
                    Error::UndefinedSymbol { .. } => {
                        self.pending_exports
                            .push(((Id::word(js_word!("default")), expr.span()), expr.clone()));
                        return;
                    }
                    _ => {}
                }
                self.storage.report(err);
                return;
            }
        };
        self.storage
            .store_private_var(self.ctx.module_id, Id::word(js_word!("default")), ty);
        self.storage
            .export_var(span, self.ctx.module_id, Id::word(js_word!("default")));
    }
}

#[validator]
impl Analyzer<'_, '_> {
    fn validate(&mut self, export: &RExportDecl) {
        let span = export.span;

        match &export.decl {
            RDecl::Fn(ref f) => {
                f.visit_with(self);
                // self.export(f.span(), f.ident.clone().into(), None);
                self.export_var(f.span(), f.ident.clone().into());
            }
            RDecl::TsInterface(ref i) => {
                i.visit_with(self);

                self.export(i.span(), i.id.clone().into(), None)
            }

            RDecl::Class(ref c) => {
                c.visit_with(self);
                self.export(c.span(), c.ident.clone().into(), None);
                self.export_var(c.span(), c.ident.clone().into());
            }
            RDecl::Var(ref var) => {
                let span = var.span;
                var.visit_with(self);

                let ids: Vec<Id> = find_ids_in_pat(&var.decls);

                for id in ids {
                    self.export_var(span, id)
                }
            }
            RDecl::TsEnum(ref e) => {
                let span = e.span();

                let ty = e
                    .validate_with(self)
                    .report(&mut self.storage)
                    .map(Type::from)
                    .map(|ty| ty.cheap());
                let ty = ty.unwrap_or_else(|| Type::any(span));

                self.storage
                    .store_private_type(self.ctx.module_id, e.id.clone().into(), ty);
                self.storage.export_type(span, self.ctx.module_id, e.id.clone().into());
            }
            RDecl::TsModule(module) => {
                module.visit_with(self);

                match &module.id {
                    RTsModuleName::Ident(id) => {
                        self.storage.export_type(span, self.ctx.module_id, id.clone().into());
                    }
                    RTsModuleName::Str(_) => {
                        unimplemented!("export module with string name")
                    }
                }
            }
            RDecl::TsTypeAlias(ref decl) => {
                decl.visit_with(self);
                // export type Foo = 'a' | 'b';
                // export type Foo = {};

                // TODO: Handle type parameters.

                self.export(span, decl.id.clone().into(), None)
            }
        }

        Ok(())
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
                let fn_ty = match f.function.validate_with(self) {
                    Ok(ty) => ty,
                    Err(err) => {
                        self.storage.report(err);
                        return Ok(());
                    }
                };
                if f.function.return_type.is_none() {
                    if let Some(m) = &mut self.mutations {
                        if m.for_fns.entry(f.function.node_id).or_default().ret_ty.is_none() {
                            m.for_fns.entry(f.function.node_id).or_default().ret_ty = Some(fn_ty.ret_ty.clone());
                        }
                    }
                }
                self.register_type(i.clone(), box fn_ty.clone().into());
                if let Some(ref i) = f.ident {
                    self.override_var(VarDeclKind::Var, i.into(), box fn_ty.into())
                        .report(&mut self.storage);
                }

                self.export(f.span(), Id::word(js_word!("default")), Some(i))
            }
            RDefaultDecl::Class(ref c) => {
                let id = c
                    .ident
                    .as_ref()
                    .map(|v| v.into())
                    .unwrap_or_else(|| Id::word(js_word!("default")));

                let class_ty = c.class.validate_with(self)?;
                self.register_type(id.clone(), box Type::Class(class_ty));

                self.export(span, Id::word(js_word!("default")), Some(id));
            }
            RDefaultDecl::TsInterfaceDecl(ref i) => {
                let i = i.id.clone().into();
                export.visit_children_with(self);

                // TODO: Register type

                self.export(span, Id::word(js_word!("default")), Some(i))
            }
        };

        Ok(())
    }
}

impl Analyzer<'_, '_> {
    #[extra_validator]
    fn export_var(&mut self, span: Span, name: Id) {
        self.storage.export_var(span, self.ctx.module_id, name);
    }

    /// Exports a type.
    ///
    /// `scope.regsiter_type` should be called before calling this method.
    ///
    ///
    /// Note: We don't freeze types at here because doing so may prevent proper
    /// finalization.
    #[extra_validator]
    fn export(&mut self, span: Span, name: Id, orig_name: Option<Id>) {
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
            self.storage.store_private_type(self.ctx.module_id, name.clone(), ty);
        }

        self.storage.export_type(span, self.ctx.module_id, name);
    }

    /// Exports a variable.
    fn export_expr(&mut self, name: Id, item_node_id: NodeId, e: &RExpr) -> ValidationResult<()> {
        let ty = e.validate_with_default(self)?;

        if *name.sym() == js_word!("default") {
            match e {
                RExpr::Ident(..) => return Ok(()),
                _ => {}
            }
            let var = RVarDeclarator {
                node_id: NodeId::invalid(),
                span: DUMMY_SP,
                name: RPat::Ident(RIdent {
                    node_id: NodeId::invalid(),
                    span: DUMMY_SP,
                    sym: "_default".into(),
                    type_ann: Some(RTsTypeAnn {
                        node_id: NodeId::invalid(),
                        span: DUMMY_SP,
                        type_ann: ty.clone().into(),
                    }),
                    optional: false,
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
        self.export_expr(Id::word(js_word!("default")), node.node_id, &node.expr)?;

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
    fn validate(&mut self, node: &RExportAll) {
        let span = node.span;

        let path = self.storage.path(self.ctx.module_id);
        let module_id = self.loader.module_id(&path, &node.src.value);
        let ctxt = self.ctx.module_id;

        match self.imports.get(&(ctxt, module_id)) {
            Some(data) => {
                for (id, ty) in data.vars.iter() {
                    self.storage.reexport_var(span, ctxt, id.clone(), ty.clone());
                }
                for (id, types) in data.types.iter() {
                    for ty in types {
                        self.storage.reexport_type(span, ctxt, id.clone(), ty.clone());
                    }
                }
            }
            None => self.storage.report(Error::ExportAllFailed { span }),
        }

        Ok(())
    }
}

#[validator]
impl Analyzer<'_, '_> {
    fn validate(&mut self, node: &RNamedExport) {
        let span = node.span;
        let ctxt = self.ctx.module_id;
        let base = self.storage.path(ctxt);

        for specifier in &node.specifiers {
            match specifier {
                RExportSpecifier::Namespace(_) => {
                    // We need
                    match &node.src {
                        Some(src) => {
                            let module_id = self.loader.module_id(&base, &src.value);
                        }
                        None => {}
                    }
                }
                RExportSpecifier::Default(_) => {}
                RExportSpecifier::Named(named) => {
                    //

                    match &node.src {
                        Some(src) => {
                            let module_id = self.loader.module_id(&base, &src.value);

                            self.reexport(
                                span,
                                ctxt,
                                module_id,
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
                                ctxt,
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
            self.storage.export_var(span, ctxt, id.clone());
        }

        if self.storage.get_local_type(ctxt, orig).is_some() {
            self.storage.export_type(span, ctxt, id);
        }
    }

    fn reexport(&mut self, span: Span, ctxt: ModuleId, from: ModuleId, orig: Id, id: Id) {
        let mut did_work = false;

        if let Some(data) = self.imports.get(&(ctxt, from)) {
            if let Some(ty) = data.vars.get(orig.sym()) {
                did_work = true;
                self.storage.reexport_var(span, ctxt, id.sym().clone(), ty.clone());
            }

            if let Some(ty) = data.types.get(orig.sym()) {
                did_work = true;
                let ty = Type::union(ty.clone());
                self.storage.reexport_type(span, ctxt, id.sym().clone(), ty);
            }
        }

        if !did_work {
            self.storage.report(Error::ExportFailed { span, orig, id })
        }
    }
}
