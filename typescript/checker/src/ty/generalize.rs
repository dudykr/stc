use crate::ty::{self, Array, Type};
use stc_types::{eq::TypeEq, FoldWith};
use swc_common::Spanned;

pub(super) struct TupleToArray;

impl ty::Fold for TupleToArray {
    fn fold_type(&mut self, ty: Type) -> Type {
        let ty: Type = ty.fold_children_with(self);
        let span = ty.span();

        match ty {
            Type::Tuple(tuple) => {
                let mut types: Vec<Box<Type>> = vec![];

                for element in tuple.elems {
                    if types.iter().any(|item| item.type_eq(&element.ty)) {
                        continue;
                    }

                    types.push(element.ty);
                }

                let elem_type = Type::union(types);
                return Type::Array(Array { span, elem_type });
            }

            _ => ty,
        }
    }
}
