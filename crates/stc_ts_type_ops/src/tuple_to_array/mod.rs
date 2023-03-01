use rnode::{Fold, FoldWith};
use stc_ts_types::{Array, ArrayMetadata, CowType, Type};
use swc_common::TypeEq;

pub use self::metadata::prevent_tuple_to_array;

mod metadata;

pub struct TupleToArray;

impl Fold<Type> for TupleToArray {
    fn fold(&mut self, ty: Type) -> Type {
        let ty = ty.fold_children_with(self);

        match ty {
            Type::Tuple(tuple) => {
                let span = tuple.span;

                let mut types: Vec<CowType> = vec![];

                for element in tuple.elems {
                    if types.iter().any(|item| item.type_eq(&element.ty)) {
                        continue;
                    }

                    types.push(element.ty);
                }

                let elem_type = Type::new_union(span, types).into();
                Type::Array(Array {
                    span,
                    elem_type,
                    metadata: ArrayMetadata {
                        common: tuple.metadata.common,
                        ..Default::default()
                    },
                    tracker: Default::default(),
                })
            }

            _ => ty,
        }
    }
}
