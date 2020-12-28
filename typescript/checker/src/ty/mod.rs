use self::{generalize::TupleToArray, type_facts::TypeFactsHandler};
use crate::util::type_ext::TypeVecExt;
use crate::{type_facts::TypeFacts, util::is_str_lit_or_union};
use retain_mut::RetainMut;
use rnode::Fold;
use rnode::FoldWith;
use stc_ast_rnode::RBool;
use stc_ast_rnode::RNumber;
use stc_ast_rnode::RStr;
use stc_ast_rnode::RTsKeywordType;
use stc_ast_rnode::RTsLit;
use stc_ast_rnode::RTsLitType;
pub(crate) use stc_types::*;
use swc_ecma_ast::{Bool, Number, Str, TsKeywordType, TsKeywordTypeKind, TsLit, TsLitType};

mod generalize;
mod type_facts;

pub(crate) struct LitGeneralizer;

impl Fold<Ref> for LitGeneralizer {
    fn fold(&mut self, mut r: Ref) -> Ref {
        r.type_name = r.type_name.fold_with(self);

        r
    }
}

impl Fold<Union> for LitGeneralizer {
    fn fold(&mut self, mut union: Union) -> Union {
        union = union.fold_children_with(self);

        union.types.dedup_type();

        union
    }
}

impl Fold<Tuple> for LitGeneralizer {
    fn fold(&mut self, mut tuple: Tuple) -> Tuple {
        tuple = tuple.fold_children_with(self);

        let has_rest = tuple
            .elems
            .iter()
            .map(|element| &element.ty)
            .any(|ty| match &**ty {
                Type::Rest(..) => true,
                _ => false,
            });

        if has_rest {
            // Handle rest
            let mut rest_ty = None;

            // Remove types after `...boolean[]`
            tuple.elems.retain_mut(|element| {
                match &*element.ty {
                    Type::Rest(RestType {
                        ty: box Type::Array(Array { elem_type, .. }),
                        ..
                    }) => {
                        rest_ty = Some(elem_type.clone());
                    }
                    _ => {
                        if let Some(rest_ty) = &rest_ty {
                            return false;
                        }
                    }
                }

                true
            });
        }

        tuple
    }
}

impl Fold<Type> for LitGeneralizer {
    fn fold(&mut self, mut ty: Type) -> Type {
        match &ty {
            Type::IndexedAccessType(IndexedAccessType { index_type, .. })
                if is_str_lit_or_union(&index_type) =>
            {
                return ty;
            }
            _ => {}
        }

        ty = ty.fold_children_with(self);

        match ty {
            Type::Lit(RTsLitType { span, ref lit, .. }) => {
                return Type::Keyword(RTsKeywordType {
                    span,
                    kind: match *lit {
                        RTsLit::Bool(RBool { .. }) => TsKeywordTypeKind::TsBooleanKeyword,
                        RTsLit::Number(RNumber { .. }) => TsKeywordTypeKind::TsNumberKeyword,
                        RTsLit::Str(RStr { .. }) => TsKeywordTypeKind::TsStringKeyword,
                        RTsLit::Tpl(..) => TsKeywordTypeKind::TsStringKeyword,
                        RTsLit::BigInt(..) => TsKeywordTypeKind::TsBigIntKeyword,
                    },
                })
            }
            _ => ty,
        }
    }
}

impl Fold<Function> for LitGeneralizer {
    fn fold(&mut self, node: Function) -> Function {
        node
    }
}

pub trait TypeExt: Into<Type> {
    fn generalize_lit(self) -> Box<Type> {
        box self.into().fold_with(&mut LitGeneralizer)
    }

    fn generalize_tuple(self) -> Box<Type> {
        box self.into().fold_with(&mut TupleToArray)
    }

    fn apply_type_facts(self, facts: TypeFacts) -> Box<Type> {
        (box self.into()).fold_with(&mut TypeFactsHandler { facts })
    }
}

impl<T> TypeExt for T where T: Into<Type> {}
