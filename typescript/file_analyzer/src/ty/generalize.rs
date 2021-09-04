use crate::ty::{Array, Type};
use rnode::{Fold, FoldWith};
use stc_ts_types::ArrayMetadata;
use swc_common::{Spanned, TypeEq};

pub(super) struct TupleToArray;

impl Fold<Type> for TupleToArray {
    fn fold(&mut self, ty: Type) -> Type {
        let ty = ty.fold_children_with(self);
        let span = ty.span();

        match ty {
            Type::Tuple(tuple) => {
                let mut types: Vec<Type> = vec![];

                for element in tuple.elems {
                    if types.iter().any(|item| item.type_eq(&element.ty)) {
                        continue;
                    }

                    types.push(*element.ty);
                }

                let elem_type = box Type::union(types);
                return Type::Array(Array {
                    span,
                    elem_type,
                    metadata: ArrayMetadata {
                        common: tuple.metadata.common,
                        ..Default::default()
                    },
                });
            }

            _ => ty,
        }
    }
}
