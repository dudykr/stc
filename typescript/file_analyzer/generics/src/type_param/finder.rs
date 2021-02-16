use rnode::Visit;
use stc_ts_types::TypeParam;
use stc_ts_types::TypeParamDecl;

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
