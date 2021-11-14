use swc_atoms::JsWord;
use swc_common::DUMMY_SP;
use swc_ecma_ast::*;
use swc_ecma_visit::{Node, Visit, VisitWith};

pub(crate) fn find_modules_and_deps(m: &Module) -> (Vec<JsWord>, Vec<JsWord>) {
    let mut v = DepFinder::default();

    m.visit_with(&Invalid { span: DUMMY_SP }, &mut v);

    (v.declared_modules, v.files)
}

#[derive(Default)]
struct DepFinder {
    declared_modules: Vec<JsWord>,
    files: Vec<JsWord>,
}

impl Visit for DepFinder {
    fn visit_export_all(&mut self, export: &ExportAll, _: &dyn Node) {
        self.files.push(export.src.value.clone());
    }

    fn visit_import_decl(&mut self, import: &ImportDecl, _: &dyn Node) {
        self.files.push(import.src.value.clone());
    }

    fn visit_named_export(&mut self, export: &NamedExport, _: &dyn Node) {
        if let Some(src) = &export.src {
            self.files.push(src.value.clone());
        }
    }

    fn visit_ts_external_module_ref(&mut self, import: &TsExternalModuleRef, _: &dyn Node) {
        self.files.push(import.expr.value.clone());
    }

    fn visit_ts_module_decl(&mut self, n: &TsModuleDecl, _: &dyn Node) {
        n.visit_children_with(self);

        match &n.id {
            TsModuleName::Str(s) => {
                self.declared_modules.push(s.value.clone());
            }
            _ => {}
        }
    }
}
