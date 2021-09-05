use self::generalize::TupleToArray;
use crate::{util::type_ext::TypeVecExt, Marks};
use retain_mut::RetainMut;
use rnode::{Fold, FoldWith, Visit, VisitWith};
use stc_ts_ast_rnode::{RBool, RNumber, RStr, RTsLit};
use stc_ts_type_ops::{is_str_lit_or_union, Fix};
pub(crate) use stc_ts_types::*;
use swc_ecma_ast::TsKeywordTypeKind;
use tracing::instrument;

mod generalize;
pub mod type_facts;

pub(crate) struct LitGeneralizer {
    pub marks: Marks,
}

impl Fold<Ref> for LitGeneralizer {
    fn fold(&mut self, mut r: Ref) -> Ref {
        r.type_name = r.type_name.fold_with(self);

        r
    }
}

impl Fold<Union> for LitGeneralizer {
    fn fold(&mut self, mut u: Union) -> Union {
        u = u.fold_children_with(self);

        u.types.dedup_type();

        u
    }
}

impl Fold<Tuple> for LitGeneralizer {
    fn fold(&mut self, mut tuple: Tuple) -> Tuple {
        tuple = tuple.fold_children_with(self);

        let has_rest = tuple.elems.iter().map(|element| &element.ty).any(|ty| match &**ty {
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
        {
            let mut checker = LitChecker {
                marks: self.marks,
                found: false,
            };
            ty.visit_with(&mut checker);

            if !checker.found {
                return ty;
            }
        }

        ty.normalize_mut();

        match &ty {
            Type::IndexedAccessType(IndexedAccessType { index_type, .. }) if is_str_lit_or_union(&index_type) => {
                return ty;
            }
            _ => {}
        }

        ty = ty.fold_children_with(self);

        match ty {
            Type::Lit(LitType {
                span,
                ref lit,
                metadata,
                ..
            }) => {
                if metadata.common.prevent_generalization {
                    return ty;
                }

                return Type::Keyword(KeywordType {
                    span,
                    kind: match *lit {
                        RTsLit::Bool(RBool { .. }) => TsKeywordTypeKind::TsBooleanKeyword,
                        RTsLit::Number(RNumber { .. }) => TsKeywordTypeKind::TsNumberKeyword,
                        RTsLit::Str(RStr { .. }) => TsKeywordTypeKind::TsStringKeyword,
                        RTsLit::Tpl(..) => TsKeywordTypeKind::TsStringKeyword,
                        RTsLit::BigInt(..) => TsKeywordTypeKind::TsBigIntKeyword,
                    },
                    metadata: KeywordTypeMetadata {
                        common: metadata.common,
                        ..Default::default()
                    },
                });
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

impl Fold<Interface> for LitGeneralizer {
    fn fold(&mut self, node: Interface) -> Interface {
        node
    }
}

impl Fold<Class> for LitGeneralizer {
    fn fold(&mut self, node: Class) -> Class {
        node
    }
}

impl Fold<ClassProperty> for LitGeneralizer {
    fn fold(&mut self, node: ClassProperty) -> ClassProperty {
        if node.readonly {
            return node;
        }
        node.fold_children_with(self)
    }
}

impl Fold<TypeLit> for LitGeneralizer {
    fn fold(&mut self, node: TypeLit) -> TypeLit {
        if node.metadata.specified {
            return node;
        }
        node.fold_children_with(self)
    }
}

struct LitChecker {
    marks: Marks,
    found: bool,
}

impl Visit<Type> for LitChecker {
    fn visit(&mut self, ty: &Type) {
        match ty {
            Type::Lit(LitType {
                span,
                ref lit,
                metadata,
                ..
            }) => {
                if metadata.common.prevent_generalization {
                    return;
                }

                self.found = true;
                return;
            }
            _ => {}
        }

        ty.visit_children_with(self);
    }
}

pub trait TypeExt: Into<Type> {
    #[instrument(skip(self, marks))]
    fn generalize_lit(self, marks: Marks) -> Type {
        self.into().fold_with(&mut LitGeneralizer { marks }).fixed()
    }

    #[instrument(skip(self))]
    fn generalize_tuple(self) -> Type {
        self.into().fold_with(&mut TupleToArray)
    }
}

impl<T> TypeExt for T where T: Into<Type> {}
