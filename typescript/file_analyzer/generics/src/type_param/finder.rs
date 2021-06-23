use fxhash::FxHashSet;
use rnode::{Visit, VisitWith};
use stc_ts_types::{Id, TypeParam, TypeParamDecl};

#[derive(Debug, Default)]
pub struct TypeParamDeclFinder {
    pub params: FxHashSet<Id>,
}

impl Visit<TypeParamDecl> for TypeParamDeclFinder {
    #[inline]
    fn visit(&mut self, decl: &TypeParamDecl) {
        decl.visit_children_with(self);

        self.params.extend(decl.params.iter().map(|v| v.name.clone()));
    }
}

#[derive(Debug, Default)]
pub struct TypeParamUsageFinder {
    pub params: Vec<TypeParam>,
}

/// Noop as declaration is not usage.
impl Visit<TypeParamDecl> for TypeParamUsageFinder {
    #[inline]
    fn visit(&mut self, _: &TypeParamDecl) {}
}

impl Visit<TypeParam> for TypeParamUsageFinder {
    fn visit(&mut self, node: &TypeParam) {
        for p in &self.params {
            if node.name == p.name {
                return;
            }
        }

        // slog::info!(self.logger, "Found type parameter({})", node.name);

        self.params.push(node.clone());
    }
}
