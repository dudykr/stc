use stc_ts_utils::imports::find_imports_in_comments;
use swc_atoms::JsWord;
use swc_common::{comments::Comments, Span, Spanned};
use swc_ecma_ast::*;
use swc_ecma_visit::{Visit, VisitWith};

pub(crate) fn find_modules_and_deps<C>(comments: &C, m: &Module) -> (Vec<JsWord>, Vec<JsWord>)
where
    C: Comments,
{
    let mut v = DepFinder {
        comments,
        declared_modules: Default::default(),
        deps: Default::default(),
    };

    m.visit_with(&mut v);

    (v.declared_modules, v.deps)
}

struct DepFinder<C>
where
    C: Comments,
{
    comments: C,
    declared_modules: Vec<JsWord>,
    deps: Vec<JsWord>,
}

impl<C> DepFinder<C>
where
    C: Comments,
{
    fn check_comments(&mut self, span: Span) {
        let deps = find_imports_in_comments(&self.comments, span);

        self.deps.extend(deps.into_iter().map(|i| i.to_path()));
    }
}

impl<C> Visit for DepFinder<C>
where
    C: Comments,
{
    fn visit_module_item(&mut self, i: &ModuleItem) {
        i.visit_children_with(self);

        self.check_comments(i.span())
    }

    fn visit_module(&mut self, m: &Module) {
        m.visit_children_with(self);

        self.check_comments(m.span)
    }

    fn visit_export_all(&mut self, export: &ExportAll) {
        self.deps.push(export.src.value.clone());
    }

    fn visit_import_decl(&mut self, import: &ImportDecl) {
        self.deps.push(import.src.value.clone());
    }

    fn visit_named_export(&mut self, export: &NamedExport) {
        if let Some(src) = &export.src {
            self.deps.push(src.value.clone());
        }
    }

    fn visit_ts_external_module_ref(&mut self, import: &TsExternalModuleRef) {
        self.deps.push(import.expr.value.clone());
    }

    fn visit_ts_module_decl(&mut self, n: &TsModuleDecl) {
        n.visit_children_with(self);

        match &n.id {
            TsModuleName::Str(s) => {
                self.declared_modules.push(s.value.clone());
            }
            _ => {}
        }
    }
}
