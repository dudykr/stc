use swc_atoms::JsWord;
use swc_common::{
    comments::{CommentKind, Comments},
    Spanned, DUMMY_SP,
};
use swc_ecma_ast::*;
use swc_ecma_visit::{Node, Visit, VisitWith};

pub(crate) fn find_modules_and_deps<C>(comments: &C, m: &Module) -> (Vec<JsWord>, Vec<JsWord>)
where
    C: Comments,
{
    let mut v = DepFinder {
        comments,
        declared_modules: Default::default(),
        deps: Default::default(),
    };

    m.visit_with(&Invalid { span: DUMMY_SP }, &mut v);

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

impl<C> Visit for DepFinder<C>
where
    C: Comments,
{
    fn visit_module_item(&mut self, i: &ModuleItem, _: &dyn Node) {
        i.visit_children_with(self);

        let mut deps = vec![];

        self.comments.with_leading(i.span().lo, |comments| {
            for c in comments {
                if c.kind != CommentKind::Line {
                    continue;
                }
                if let Some(cmt_text) = c
                    .text
                    .strip_prefix("/")
                    .map(|s| s.trim())
                    .and_then(|s| s.strip_prefix("<reference"))
                    .and_then(|s| s.strip_suffix("\" />"))
                    .map(|s| s.trim())
                    .and_then(|s| s.strip_prefix("types=\""))
                {
                    deps.push(cmt_text.into());
                }
            }
        });

        self.deps.extend(deps);
    }

    fn visit_export_all(&mut self, export: &ExportAll, _: &dyn Node) {
        self.deps.push(export.src.value.clone());
    }

    fn visit_import_decl(&mut self, import: &ImportDecl, _: &dyn Node) {
        self.deps.push(import.src.value.clone());
    }

    fn visit_named_export(&mut self, export: &NamedExport, _: &dyn Node) {
        if let Some(src) = &export.src {
            self.deps.push(src.value.clone());
        }
    }

    fn visit_ts_external_module_ref(&mut self, import: &TsExternalModuleRef, _: &dyn Node) {
        self.deps.push(import.expr.value.clone());
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
