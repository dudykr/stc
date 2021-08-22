use rnode::{Visit, VisitWith};
use stc_ts_ast_rnode::{RExportAll, RImportDecl, RModule, RNamedExport};
use swc_atoms::JsWord;

pub(crate) fn find_deps(m: &RModule) -> Vec<JsWord> {
    let mut v = DepFinder::default();

    m.visit_with(&mut v);

    v.files
}

#[derive(Default)]
struct DepFinder {
    files: Vec<JsWord>,
}

impl Visit<RImportDecl> for DepFinder {
    fn visit(&mut self, import: &RImportDecl) {
        self.files.push(import.src.value.clone());
    }
}

impl Visit<RNamedExport> for DepFinder {
    fn visit(&mut self, export: &RNamedExport) {
        if let Some(src) = &export.src {
            self.files.push(src.value.clone());
        }
    }
}

impl Visit<RExportAll> for DepFinder {
    fn visit(&mut self, export: &RExportAll) {
        self.files.push(export.src.value.clone());
    }
}
