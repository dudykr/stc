use fxhash::FxHashMap;
use fxhash::FxHashSet;
use rnode::Fold;
use rnode::FoldWith;
use stc_ts_types::Function;
use stc_ts_types::Id;
use stc_ts_types::MethodSignature;
use stc_ts_types::Type;
use stc_ts_types::TypeParamDecl;

#[derive(Debug)]
pub struct TypeParamRenamer {
    pub inferred: FxHashMap<Id, Type>,
    /// Declared type parameters. Only type parameters in this set will be
    /// replaced.
    ///
    /// This is filled by visitor itself.
    pub declared: Option<FxHashSet<Id>>,
}

impl Fold<TypeParamDecl> for TypeParamRenamer {
    fn fold(&mut self, decl: TypeParamDecl) -> TypeParamDecl {
        if self.declared.is_none() {
            self.declared = Some(Default::default())
        }

        self.declared
            .as_mut()
            .unwrap()
            .extend(decl.params.iter().map(|v| v.name.clone()));
        decl.fold_children_with(self)
    }
}

impl Fold<MethodSignature> for TypeParamRenamer {
    fn fold(&mut self, m: MethodSignature) -> MethodSignature {
        let key = m.key.fold_with(self);
        let type_params = m.type_params.fold_with(self);
        let params = m.params.fold_with(self);
        let ret_ty = m.ret_ty.fold_with(self);

        MethodSignature {
            accessibility: m.accessibility,
            span: m.span,
            readonly: m.readonly,
            key,
            optional: m.optional,
            params,
            ret_ty,
            type_params,
        }
    }
}

impl Fold<Function> for TypeParamRenamer {
    fn fold(&mut self, f: Function) -> Function {
        let type_params = f.type_params.fold_with(self);
        let params = f.params.fold_with(self);
        let ret_ty = f.ret_ty.fold_with(self);

        Function {
            span: f.span,
            type_params,
            params,
            ret_ty,
        }
    }
}

impl Fold<Type> for TypeParamRenamer {
    fn fold(&mut self, mut ty: Type) -> Type {
        ty = ty.fold_children_with(self);

        match ty {
            Type::Param(ref param) => {
                if let Some(declared) = &self.declared {
                    if !declared.contains(&param.name) {
                        return ty;
                    }
                }

                if let Some(mapped) = self.inferred.get(&param.name) {
                    if self.declared.is_none() && mapped.is_type_param() {
                        return ty;
                    }
                    return mapped.clone();
                }
            }
            _ => {}
        }

        ty
    }
}
