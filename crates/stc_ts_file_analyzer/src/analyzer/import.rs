use std::sync::Arc;

use rayon::prelude::*;
use rnode::{Visit, VisitWith};
use stc_ts_ast_rnode::{
    RCallExpr, RCallee, RExportAll, RExpr, RImportDecl, RImportSpecifier, RLit, RModuleItem, RNamedExport, RStr, RTsExternalModuleRef,
    RTsImportType,
};
use stc_ts_errors::ErrorKind;
use stc_ts_file_analyzer_macros::extra_validator;
use stc_ts_storage::Storage;
use stc_ts_types::{Id, ModuleId, Type};
use stc_ts_utils::imports::find_imports_in_comments;
use swc_atoms::{js_word, JsWord};
use swc_common::{comments::Comments, FileName, Span, Spanned, GLOBALS};

use crate::{
    analyzer::{scope::VarKind, util::ResultExt, Analyzer},
    loader::ModuleInfo,
    validator, DepInfo, VResult,
};

impl Analyzer<'_, '_> {
    /// Returns `(dep_module, dep_types)` if an import is valid, and returns
    /// `(cur_mod_id, empty_data)` on import errors.
    ///
    /// TODO: Make this returns None when import failed
    pub(crate) fn get_imported_items(&mut self, span: Span, dst: &JsWord) -> (ModuleId, Type) {
        let ctxt = self.ctx.module_id;
        let base = self.storage.path(ctxt);
        let dep_id = self.loader.module_id(&base, dst);
        let dep_id = match dep_id {
            Some(v) => v,
            None => {
                dbg!(&base, dst);
                self.storage.report(ErrorKind::ModuleNotFound { span }.into());

                return (ctxt, Type::any(span, Default::default()));
            }
        };
        let data = match self.data.imports.get(&(ctxt, dep_id)).cloned() {
            Some(v) => v,
            None => {
                dbg!(&base, dst);

                self.storage.report(ErrorKind::ModuleNotFound { span }.into());

                return (ctxt, Type::any(span, Default::default()));
            }
        };

        (dep_id, data)
    }

    pub(super) fn find_imported_var(&self, id: &Id) -> VResult<Option<Type>> {
        if let Some(ModuleInfo { module_id, data }) = self.data.imports_by_id.get(id) {
            match data.normalize() {
                Type::Module(data) => {
                    if let Some(dep) = data.exports.vars.get(id.sym()).cloned() {
                        debug_assert!(dep.is_clone_cheap());

                        return Ok(Some(dep));
                    }
                }
                _ => {
                    unreachable!()
                }
            }
        }

        Ok(None)
    }

    pub(crate) fn insert_import_info(&mut self, ctxt: ModuleId, dep_module_id: ModuleId, ty: Type) -> VResult<()> {
        self.data.imports.entry((ctxt, dep_module_id)).or_insert(ty);

        Ok(())
    }

    #[extra_validator]
    pub(super) fn load_normal_imports(&mut self, module_spans: Vec<(ModuleId, Span)>, items: &Vec<&RModuleItem>) {
        if self.config.is_builtin {
            return;
        }

        #[inline]
        fn is_relative_path(path: &str) -> bool {
            path.starts_with("./") || path.starts_with("../")
        }
        // We first load non-circular imports.
        let imports = ImportFinder::find_imports(&self.comments, module_spans, &self.storage, items);

        let loader = self.loader;
        let mut normal_imports = vec![];
        for (ctxt, import) in imports {
            let span = import.span;
            let base = self.storage.path(ctxt);
            let dep_id = self.loader.module_id(&base, &import.src);
            let dep_id = match dep_id {
                Some(v) => v,
                _ if !is_relative_path(&import.src) => {
                    continue;
                }
                _ => {
                    self.storage.report(ErrorKind::ModuleNotFound { span }.into());
                    continue;
                }
            };

            if loader.is_in_same_circular_group(&base, &import.src) {
                continue;
            }

            normal_imports.push((ctxt, base.clone(), dep_id, import.src.clone(), import));
        }

        let import_results = if cfg!(feature = "no-threading") {
            let iter = normal_imports.into_iter();

            GLOBALS.with(|globals| {
                iter.map(|(ctxt, base, dep_id, module_specifier, import)| {
                    GLOBALS.set(globals, || {
                        let res = loader.load_non_circular_dep(&base, &module_specifier);
                        (ctxt, dep_id, import, res)
                    })
                })
                .collect::<Vec<_>>()
            })
        } else {
            let iter = normal_imports.into_par_iter();

            GLOBALS.with(|globals| {
                iter.map(|(ctxt, base, dep_id, module_specifier, import)| {
                    GLOBALS.set(globals, || {
                        let res = loader.load_non_circular_dep(&base, &module_specifier);
                        (ctxt, dep_id, import, res)
                    })
                })
                .collect::<Vec<_>>()
            })
        };

        for (ctxt, dep_id, import, res) in import_results {
            let span = import.span;

            match res {
                Ok(info) => {
                    self.insert_import_info(ctxt, dep_id, info).report(&mut self.storage);
                }
                Err(err) => self.storage.report(err),
            }
        }
    }
}

impl Analyzer<'_, '_> {
    pub(crate) fn load_import_lazily(
        &mut self,
        span: Span,
        base: &Arc<FileName>,
        dep_id: ModuleId,
        module_specifier: &str,
    ) -> VResult<Type> {
        let ctxt = self.ctx.module_id;
        if ctxt == dep_id {
            return Ok(Type::any(span, Default::default()));
        }

        let ty = self.loader.load_non_circular_dep(base, module_specifier)?;

        self.insert_import_info(ctxt, dep_id, ty.clone()).report(&mut self.storage);

        Ok(ty)
    }

    fn handle_import(&mut self, span: Span, ctxt: ModuleId, target: ModuleId, orig: Id, id: Id) {
        let mut found_entry = false;
        let is_import_successful = ctxt != target;

        // Check for entry only if import was successful.
        if is_import_successful {
            if let Some(data) = self.data.imports.get(&(ctxt, target)) {
                match data.normalize() {
                    Type::Module(data) => {
                        if let Some(ty) = data.exports.vars.get(orig.sym()).cloned() {
                            found_entry = true;
                            self.storage.store_private_var(ctxt, id.clone(), ty);
                        }

                        if let Some(types) = data.exports.types.get(orig.sym()).cloned() {
                            found_entry = true;
                            for ty in types {
                                found_entry = true;
                                self.storage.store_private_type(ctxt, id.clone(), ty.clone(), false);
                            }
                        }
                    }
                    _ => {
                        unreachable!()
                    }
                }
            } else {
                unreachable!("Import should be successful")
            }
        }

        if !found_entry {
            self.data.unresolved_imports.insert(id.clone());

            self.register_type(id.clone(), Type::any(span, Default::default()));
            self.declare_var(
                span,
                VarKind::Import,
                id.clone(),
                Some(Type::any(span, Default::default())),
                None,
                true,
                false,
                false,
                false,
            )
            .report(&mut self.storage);

            if is_import_successful {
                // If import was successful but the entry is not found, the error should point
                // the specifier.
                self.storage.report(ErrorKind::ImportFailed { span, orig, id }.into());
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
                    self.handle_import(default.span, base, dep, Id::word(js_word!("default")), Id::from(&default.local));
                }
                RImportSpecifier::Namespace(ns) => {
                    if base == dep {
                        // Import failed
                        self.declare_var(
                            ns.span,
                            VarKind::Import,
                            ns.local.clone().into(),
                            Some(Type::any(ns.span, Default::default())),
                            None,
                            true,
                            false,
                            false,
                            false,
                        )?;
                    } else {
                        self.declare_var(
                            ns.span,
                            VarKind::Import,
                            ns.local.clone().into(),
                            Some(data.clone()),
                            None,
                            true,
                            false,
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

struct ImportFinder<'a, C>
where
    C: Comments,
{
    storage: &'a Storage<'a>,
    cur_ctxt: ModuleId,
    to: Vec<(ModuleId, DepInfo)>,
    comments: C,
}

impl<'a, C> ImportFinder<'a, C>
where
    C: Comments,
{
    fn check_comments(&mut self, span: Span) {
        if span.is_dummy() {
            return;
        }

        let ctxt = self.cur_ctxt;
        let deps = find_imports_in_comments(&self.comments, span);

        self.to
            .extend(deps.into_iter().map(|src| (ctxt, DepInfo { span, src: src.to_path() })));
    }

    pub fn find_imports<T>(comments: C, module_span: Vec<(ModuleId, Span)>, storage: &'a Storage<'a>, node: &T) -> Vec<(ModuleId, DepInfo)>
    where
        T: for<'any> VisitWith<ImportFinder<'any, C>>,
    {
        let mut v = Self {
            comments,
            storage,
            to: Default::default(),
            cur_ctxt: ModuleId::builtin(),
        };

        for (ctxt, span) in module_span {
            v.cur_ctxt = ctxt;
            v.check_comments(span);
        }

        v.cur_ctxt = ModuleId::builtin();

        node.visit_with(&mut v);

        v.to
    }
}

impl<C> Visit<Vec<&'_ RModuleItem>> for ImportFinder<'_, C>
where
    C: Comments,
{
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

impl<C> Visit<RCallExpr> for ImportFinder<'_, C>
where
    C: Comments,
{
    /// Extracts require('foo')
    fn visit(&mut self, expr: &RCallExpr) {
        expr.visit_children_with(self);

        let span = expr.span();

        match &expr.callee {
            RCallee::Expr(box RExpr::Ident(i)) if i.sym == js_word!("require") => {
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
            RCallee::Import(import) => {
                let src = expr.args.first().and_then(|v| match *v.expr {
                    RExpr::Lit(RLit::Str(RStr { ref value, .. })) => Some(value.clone()),
                    _ => None,
                });

                if let Some(src) = src {
                    self.to.push((self.cur_ctxt, DepInfo { span, src }));
                }
            }
            _ => {}
        }
    }
}

impl<C> Visit<RImportDecl> for ImportFinder<'_, C>
where
    C: Comments,
{
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

impl<C> Visit<RNamedExport> for ImportFinder<'_, C>
where
    C: Comments,
{
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

impl<C> Visit<RExportAll> for ImportFinder<'_, C>
where
    C: Comments,
{
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

impl<C> Visit<RTsExternalModuleRef> for ImportFinder<'_, C>
where
    C: Comments,
{
    fn visit(&mut self, r: &RTsExternalModuleRef) {
        self.to.push((
            self.cur_ctxt,
            DepInfo {
                span: r.span,
                src: r.expr.value.clone(),
            },
        ));
    }
}

impl<C> Visit<RTsImportType> for ImportFinder<'_, C>
where
    C: Comments,
{
    fn visit(&mut self, import: &RTsImportType) {
        let span = import.span();

        self.to.push((
            self.cur_ctxt,
            DepInfo {
                span,
                src: import.arg.value.clone(),
            },
        ));
    }
}
