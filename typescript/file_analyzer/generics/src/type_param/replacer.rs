use fxhash::FxHashMap;
use rnode::Fold;
use rnode::FoldWith;
use stc_ts_types::Id;
use stc_ts_types::Type;

#[derive(Debug)]
pub struct TypeParamReplacer {
    pub inferred: FxHashMap<Id, Box<Type>>,
    /// `true` means we should replace type parameters with other type
    /// parameter.
    pub include_type_params: bool,
}

impl Fold<Type> for TypeParamReplacer {
    fn fold(&mut self, mut ty: Type) -> Type {
        ty = ty.fold_children_with(self);

        match ty {
            Type::Param(ref param) => {
                if let Some(mapped) = self.inferred.get(&param.name) {
                    match mapped.normalize() {
                        Type::Param(..) if !self.include_type_params => return ty,
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
