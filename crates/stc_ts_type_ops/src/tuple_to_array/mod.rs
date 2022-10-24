use rnode::{Fold, FoldWith};
use stc_ts_types::{Array, ArrayMetadata, Type};
use swc_common::TypeEq;

pub use self::metadata::prevent_tuple_to_array;

mod metadata;

pub struct TupleToArray;

impl Fold<Type> for TupleToArray {
    fn fold(&mut self, mut ty: Type) -> Type {
        // TODO(kdy1): PERF
        ty.normalize_mut();
        let ty = ty.fold_children_with(self);

        match ty {
            Type::Tuple(tuple) => {
                let span = tuple.span;

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
