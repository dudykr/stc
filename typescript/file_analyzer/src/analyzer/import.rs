use std::sync::Arc;

use super::{util::ResultExt, Analyzer};
use crate::DepInfo;
use crate::{loader::ModuleInfo, validator, ValidationResult};
use rayon::prelude::*;
use rnode::Visit;
use rnode::VisitWith;
use stc_ts_ast_rnode::RCallExpr;
use stc_ts_ast_rnode::RExportAll;
use stc_ts_ast_rnode::RExpr;
use stc_ts_ast_rnode::RExprOrSuper;
use stc_ts_ast_rnode::RImportDecl;
use stc_ts_ast_rnode::RImportSpecifier;
use stc_ts_ast_rnode::RLit;
use stc_ts_ast_rnode::RModuleItem;
use stc_ts_ast_rnode::RNamedExport;
use stc_ts_ast_rnode::RStr;
use stc_ts_errors::Error;
use stc_ts_file_analyzer_macros::extra_validator;
use stc_ts_storage::Storage;
use stc_ts_types::ModuleTypeData;
use stc_ts_types::{Id, Type};
use stc_ts_types::{Module, ModuleId};
use swc_atoms::js_word;
use swc_atoms::JsWord;
use swc_common::Span;
use swc_common::Spanned;
use swc_ecma_ast::VarDeclKind;

impl Analyzer<'_, '_> {
    /// Returns `(dep_module, dep_types)` if an import is valid, and returns
    /// `(cur_mod_id, empty_data)` on import errors.
    ////
    pub(crate) fn get_imported_items(&mut self, span: Span, dst: &JsWord) -> (ModuleId, Arc<ModuleTypeData>) {
        let ctxt = self.ctx.module_id;
        let base = self.storage.path(ctxt);
        let dep_id = self.loader.module_id(&base, &dst);
        let dep_id = match dep_id {
            Some(v) => v,
            None => {
                self.storage.report(Error::ModuleNotFound { span });

                return (ctxt, Default::default());
            }
        };
        let data = match self.imports.get(&(ctxt, dep_id)).cloned() {
            Some(v) => v,
            None => {
                self.storage.report(Error::ModuleNotFound { span });

                return (ctxt, Default::default());
            }
        };

        (dep_id, data)
    }

    pub(super) fn find_imported_var(&self, id: &Id) -> ValidationResult<Option<Type>> {
        if let Some(ModuleInfo { module_id, data }) = self.imports_by_id.get(&id) {
            if let Some(dep) = data.vars.get(id.sym()).cloned() {
                debug_assert!(dep.is_clone_cheap());

                return Ok(Some(dep));
            }
        }

        Ok(None)
    }

    fn insert_import_info(&mut self, ctxt: ModuleId, info: ModuleInfo) -> ValidationResult<()> {
        let mut e = self.imports.entry((ctxt, info.module_id)).or_default();
        *e = info.data;

        Ok(())
    }

    #[extra_validator]
    pub(super) fn load_normal_imports(&mut self, items: &Vec<&RModuleItem>) {
        if self.is_builtin {
            return;
        }
        // We first load non-circular imports.
        let mut imports = ImportFinder::find_imports(&self.storage, &*items);

        let loader = self.loader;
        let mut normal_imports = vec![];
        for (ctxt, import) in imports {
            let span = import.span;

            let base = self.storage.path(ctxt);
            let dep_id = self.loader.module_id(&base, &import.src);
            let dep_id = match dep_id {
                Some(v) => v,
                None => {
                    self.storage.report(Error::ModuleNotFound { span });
                    continue;
                }
            };

            if loader.is_in_same_circular_group(ctxt, dep_id) {
                continue;
            }

            normal_imports.push((ctxt, dep_id, import));
        }

        let import_results = normal_imports
            .into_par_iter()
            .map(|(ctxt, dep_id, import)| {
                let res = loader.load_non_circular_dep(ctxt, dep_id);
                (ctxt, dep_id, import, res)
            })
            .panic_fuse()
            .collect::<Vec<_>>();

        for (ctxt, dep_id, import, res) in import_results {
            let span = import.span;

            match res {
                Ok(info) => {
                    self.insert_import_info(ctxt, info).report(&mut self.storage);
                }
                Err(err) => self.storage.report(err),
            }
        }
    }
}

impl Analyzer<'_, '_> {
    fn handle_import(&mut self, span: Span, ctxt: ModuleId, target: ModuleId, orig: Id, id: Id) {
        let mut found_entry = false;

        // Check for entry only if import was successful.
        if ctxt != target {
            if let Some(data) = self.imports.get(&(ctxt, target)) {
                for (i, ty) in &data.vars {
                    if orig.sym() == i {
                        found_entry = true;
                        self.storage.store_private_var(ctxt, id.clone(), ty.clone());
                    }
                }

                for (i, types) in &data.types {
                    if orig.sym() == i {
                        for ty in types {
                            found_entry = true;
                            self.storage.store_private_type(ctxt, id.clone(), ty.clone(), false);
                        }
                    }
                }
            }
        }

        if !found_entry {
            self.data.unresolved_imports.insert(id.clone());

            self.register_type(id.clone(), Type::any(span));
            self.declare_var(
                span,
                VarDeclKind::Var,
                id.clone(),
                Some(Type::any(span)),
                None,
                true,
                false,
                false,
            )
            .report(&mut self.storage);

            if ctxt != target {
                // If import was successful but the entry is not found, the error should point
                // the specifier.
                self.storage.report(Error::ImportFailed { span, orig, id });
            }
        }
    }
}

#[validator]
impl Analyzer<'_, '_> {
    fn validate(&mut self, node: &RImportDecl) {
        let span = node.span;
        let base = self.ctx.module_id;

        let (dep, data) = self.get_imported_items(span, &node.src.value);

        for specifier in &node.specifiers {
            match specifier {
                RImportSpecifier::Named(named) => {
                    //
                    match &named.imported {
                        Some(imported) => {
                            self.handle_import(named.span, base, dep, Id::from(imported), Id::from(&named.local));
                        }
                        None => {
                            self.handle_import(named.span, base, dep, Id::from(&named.local), Id::from(&named.local));
                        }
                    }
                }
                RImportSpecifier::Default(default) => {
                    self.handle_import(
                        default.span,
                        base,
                        dep,
                        Id::word(js_word!("default")),
                        Id::from(&default.local),
                    );
                }
                RImportSpecifier::Namespace(ns) => {
                    if base == dep {
                        // Import failed
                        self.declare_var(
                            ns.span,
                            VarDeclKind::Var,
                            ns.local.clone().into(),
                            Some(Type::any(ns.span)),
                            None,
                            true,
                            false,
                            false,
                        )?;
                    } else {
                        self.declare_var(
                            ns.span,
                            VarDeclKind::Var,
                            ns.local.clone().into(),
                            Some(Type::Module(Module {
                                span: ns.span,
                                name: ns.local.clone().into(),
                                exports: box (*data).clone(),
                            })),
                            None,
                            true,
                            false,
                            false,
                        )?;
                    }
                }
            }
        }

        Ok(())
    }
}

struct ImportFinder<'a> {
    storage: &'a Storage<'a>,
    cur_ctxt: ModuleId,
    to: Vec<(ModuleId, DepInfo)>,
}

impl<'a> ImportFinder<'a> {
    pub fn find_imports<T>(storage: &'a Storage<'a>, node: &T) -> Vec<(ModuleId, DepInfo)>
    where
        T: for<'any> VisitWith<ImportFinder<'any>>,
    {
        let mut v = Self {
            storage,
            to: Default::default(),
            cur_ctxt: ModuleId::builtin(),
        };

        node.visit_with(&mut v);

        v.to
    }
}

impl Visit<Vec<&'_ RModuleItem>> for ImportFinder<'_> {
    fn visit(&mut self, items: &Vec<&RModuleItem>) {
        for (index, item) in items.iter().enumerate() {
            let ctxt = self.storage.module_id(index);
            self.cur_ctxt = ctxt;

            if cfg!(debug_assertions) {
                // Ensure that it's valid context.
                let _ = self.storage.path(ctxt);
            }

            item.visit_with(self);
        }
    }
}

impl Visit<RCallExpr> for ImportFinder<'_> {
    /// Extracts require('foo')
    fn visit(&mut self, expr: &RCallExpr) {
        let span = expr.span();

        match expr.callee {
            RExprOrSuper::Expr(box RExpr::Ident(ref i)) if i.sym == js_word!("require") => {
                let src = expr
                    .args
                    .iter()
                    .map(|v| match *v.expr {
                        RExpr::Lit(RLit::Str(RStr { ref value, .. })) => value.clone(),
                        _ => unimplemented!("error reporting for dynamic require"),
                    })
                    .next()
                    .unwrap();
                self.to.push((self.cur_ctxt, DepInfo { span, src }));
            }
            _ => return,
        }
    }
}

impl Visit<RImportDecl> for ImportFinder<'_> {
    fn visit(&mut self, import: &RImportDecl) {
        let span = import.span();

        self.to.push((
            self.cur_ctxt,
            DepInfo {
                span,
                src: import.src.value.clone(),
            },
        ));
    }
}

impl Visit<RNamedExport> for ImportFinder<'_> {
    fn visit(&mut self, export: &RNamedExport) {
        if export.src.is_none() {
            return;
        }

        self.to.push((
            self.cur_ctxt,
            DepInfo {
                span: export.span,
                src: export.src.as_ref().unwrap().value.clone(),
            },
        ));
    }
}

impl Visit<RExportAll> for ImportFinder<'_> {
    fn visit(&mut self, export: &RExportAll) {
        self.to.push((
            self.cur_ctxt,
            DepInfo {
                span: export.span,
                src: export.src.value.clone(),
            },
        ));
    }
}
