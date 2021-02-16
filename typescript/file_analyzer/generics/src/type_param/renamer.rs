use fxhash::FxHashMap;
use rnode::Fold;
use rnode::FoldWith;
use rnode::Visit;
use stc_ts_types::Id;
use stc_ts_types::Type;
use stc_ts_types::TypeParam;
use stc_ts_types::TypeParamDecl;

#[derive(Debug)]
pub struct TypeParamRenamer {
    pub inferred: FxHashMap<Id, Box<Type>>,
}

impl Fold<Type> for TypeParamRenamer {
    fn fold(&mut self, mut ty: Type) -> Type {
        ty = ty.fold_children_with(self);

        match ty {
            Type::Param(ref param) => {
                if let Some(mapped) = self.inferred.get(&param.name) {
                    match mapped.normalize() {
                        Type::Param(..) => return ty,
                        _ => {}
                    }
                    return *mapped.clone();
                }
            }
            _ => {}
        }

        ty
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
