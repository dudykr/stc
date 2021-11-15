use rnode::{VisitMut, VisitMutWith};
use stc_ts_types::{Array, ArrayMetadata, Type};
use stc_ts_utils::MapWithMut;
use stc_utils::ext::TypeVecExt;
use swc_common::Spanned;

pub struct TupleNormalizer;

impl VisitMut<Type> for TupleNormalizer {
    fn visit_mut(&mut self, ty: &mut Type) {
        // TODO(kdy1): PERF
        ty.visit_mut_children_with(self);

        match &*ty {
            Type::Tuple(tuple) => {
                if tuple.metadata.common.prevent_tuple_to_array {
                    return;
                }

                let common_metadata = tuple.metadata.common;

                if tuple.elems.is_empty() {
                    return;
                }

                let span = ty.span();
                let mut types = ty
                    .take()
                    .expect_tuple()
                    .elems
                    .into_iter()
                    .map(|elem| elem.ty)
                    .collect::<Vec<_>>();
                types.dedup_type();

                let has_other = types.iter().any(|ty| !ty.is_null_or_undefined());
                if has_other {
                    types.retain(|ty| !ty.is_null_or_undefined())
                }

                *ty = Type::Array(Array {
                    span,
                    elem_type: Type::union(types),
                    metadata: ArrayMetadata {
                        common: common_metadata,
                        ..Default::default()
                    },
                });
            }
            _ => {}
        }
    }
}
