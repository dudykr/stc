use fxhash::FxHashSet;
use rnode::{Visit, VisitWith};
use stc_ts_types::{Id, TypeParam, TypeParamDecl};
use stc_utils::cache::ALLOW_DEEP_CLONE;

#[derive(Debug, Default)]
pub struct TypeParamDeclFinder {
    pub params: FxHashSet<Id>,
}

impl Visit<TypeParamDecl> for TypeParamDeclFinder {
    #[inline]
    fn visit(&mut self, decl: &TypeParamDecl) {
        decl.visit_children_with(self);

        self.params
            .extend(decl.params.iter().map(|v| v.name.clone()));
    }
}

#[derive(Debug, Default)]
pub struct TypeParamNameUsageFinder {
    pub params: Vec<Id>,
}

/// Noop as declaration is not usage.
impl Visit<TypeParamDecl> for TypeParamNameUsageFinder {
    #[inline]
    fn visit(&mut self, _: &TypeParamDecl) {}
}

impl Visit<TypeParam> for TypeParamNameUsageFinder {
    fn visit(&mut self, node: &TypeParam) {
        for p in &self.params {
            if node.name == *p {
                return;
            }
        }

        // info!( "Found type parameter({})", node.name);

        self.params.push(node.name.clone());
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

        // info!( "Found type parameter({})", node.name);

        self.params.push(ALLOW_DEEP_CLONE.set(&(), || node.clone()));
    }
}
