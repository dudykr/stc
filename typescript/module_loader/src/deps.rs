use swc_atoms::JsWord;
use swc_common::DUMMY_SP;
use swc_ecma_ast::*;
use swc_ecma_visit::{Node, Visit, VisitWith};

pub(crate) fn find_deps(m: &Module) -> Vec<JsWord> {
    let mut v = DepFinder::default();

    m.visit_with(&Invalid { span: DUMMY_SP }, &mut v);

    v.files
}

#[derive(Default)]
struct DepFinder {
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
}
