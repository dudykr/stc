use super::{util::ResultExt, Analyzer};
use crate::mode::Storage;
use crate::DepInfo;
use crate::{
    errors::Error, loader::ModuleInfo, validator, validator::Validate, Specifier, ValidationResult,
};
use fxhash::FxHashMap;
use rayon::prelude::*;
use stc_checker_macros::extra_validator;
use stc_types::ModuleId;
use stc_types::{Id, Type};
use std::mem::take;
use std::sync::Arc;
use swc_atoms::js_word;
use swc_common::Span;
use swc_common::{Spanned, DUMMY_SP};
use swc_ecma_ast::*;
use swc_ecma_visit::Node;

impl Analyzer<'_, '_> {
    pub(super) fn find_imported_var(&self, id: &Id) -> ValidationResult<Option<Box<Type>>> {
        if let Some(ModuleInfo { module_id, data }) = self.imports_by_id.get(&id) {
            if let Some(dep) = data.vars.get(&id).cloned() {
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
    pub(super) fn load_normal_imports(&mut self, items: &Vec<ModuleItem>) {
        if self.is_builtin {
            return;
        }
        // We first load non-circular imports.
        let mut imports = ImportFinder::find_imports(&self.storage, &*items);

        let loader = self.loader;
        let mut normal_imports = vec![];
        for (ctxt, import) in imports {
            let base = self.storage.path(ctxt);
            if loader.is_in_same_circular_group(&base, &import.src) {
                continue;
            }

            normal_imports.push((ctxt, base, import));
        }

        let import_results = normal_imports
            .into_par_iter()
            .map(|(ctxt, base, import)| {
                let res = loader.load_non_circular_dep(base, &import);
                (ctxt, import, res)
            })
            .panic_fuse()
            .collect::<Vec<_>>();

        for (ctxt, import, res) in import_results {
            let span = import.span;

            match res {
                Ok(info) => {
                    self.insert_import_info(ctxt, info)
                        .report(&mut self.storage);
                }
                Err(err) => self.storage.report(err),
            }
        }
    }
}

impl Analyzer<'_, '_> {
    fn handle_import(&mut self, span: Span, ctxt: ModuleId, target: ModuleId, orig: Id, id: Id) {
        let mut did_work = false;

        if let Some(data) = self.imports.get(&(ctxt, target)) {
            for (i, ty) in &data.vars {
                if orig == *i {
                    did_work = true;
                    self.storage.store_private_var(ctxt, id.clone(), ty.clone());
                }
            }

            for (i, types) in &data.types {
                if orig == *i {
                    for ty in types {
                        did_work = true;
                        self.storage
                            .store_private_type(ctxt, id.clone(), ty.clone());
                    }
                }
            }
        }

        if !did_work {
            self.storage.report(Error::ImportFailed { span, orig, id });
        }
    }
}

#[validator]
impl Analyzer<'_, '_> {
    fn validate(&mut self, node: &mut ImportDecl) {
        let ctxt = self.ctx.module_id;
        let base = self.storage.path(ctxt);
        let target = self.loader.module_id(&base, &node.src.value);

        for specifier in &node.specifiers {
            match specifier {
                ImportSpecifier::Named(named) => {
                    //
                    match &named.imported {
                        Some(imported) => {
                            self.handle_import(
                                named.span,
                                ctxt,
                                target,
                                Id::from(imported),
                                Id::from(&named.local),
                            );
                        }
                        None => {
                            self.handle_import(
                                named.span,
                                ctxt,
                                target,
                                Id::from(&named.local),
                                Id::from(&named.local),
                            );
                        }
                    }
                }
                ImportSpecifier::Default(default) => {
                    self.handle_import(
                        default.span,
                        ctxt,
                        target,
                        Id::word(js_word!("default")),
                        Id::from(&default.local),
                    );
                }
                ImportSpecifier::Namespace(ns) => {}
            }
        }

        Ok(())
    }
}

pub(super) struct ImportFinder<'a> {
    storage: &'a Storage<'a>,
    cur_ctxt: ModuleId,
    to: Vec<(ModuleId, DepInfo)>,
}

impl<'a> ImportFinder<'a> {
    pub fn find_imports<T>(storage: &'a Storage<'a>, node: &T) -> Vec<(ModuleId, DepInfo)>
    where
        T: for<'any> swc_ecma_visit::VisitWith<ImportFinder<'any>>,
    {
        let mut v = Self {
            storage,
            to: Default::default(),
            cur_ctxt: ModuleId::builtin(),
        };

        node.visit_with(&Invalid { span: DUMMY_SP }, &mut v);

        v.to
    }
}

impl swc_ecma_visit::Visit for ImportFinder<'_> {
    fn visit_module_items(&mut self, items: &[ModuleItem], _: &dyn Node) {
        use swc_ecma_visit::VisitWith;

        for (index, item) in items.iter().enumerate() {
            let ctxt = self.storage.module_id(index);
            self.cur_ctxt = ctxt;

            item.visit_with(&Invalid { span: DUMMY_SP }, self);
        }
    }

    /// Extracts require('foo')
    fn visit_call_expr(&mut self, expr: &CallExpr, _: &dyn Node) {
        let span = expr.span();

        match expr.callee {
            ExprOrSuper::Expr(box Expr::Ident(ref i)) if i.sym == js_word!("require") => {
                let src = expr
                    .args
                    .iter()
                    .map(|v| match *v.expr {
                        Expr::Lit(Lit::Str(Str { ref value, .. })) => value.clone(),
                        _ => unimplemented!("error reporting for dynamic require"),
                    })
                    .next()
                    .unwrap();
                self.to.push((self.cur_ctxt, DepInfo { span, src }));
            }
            _ => return,
        }
    }

    fn visit_import_decl(&mut self, import: &ImportDecl, _: &dyn Node) {
        let span = import.span();

        self.to.push((
            self.cur_ctxt,
            DepInfo {
                span,
                src: import.src.value.clone(),
            },
        ));
    }

    fn visit_named_export(&mut self, export: &NamedExport, _: &dyn Node) {
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

    fn visit_export_all(&mut self, export: &ExportAll, _: &dyn Node) {
        self.to.push((
            self.cur_ctxt,
            DepInfo {
                span: export.span,
                src: export.src.value.clone(),
            },
        ));
    }
}
