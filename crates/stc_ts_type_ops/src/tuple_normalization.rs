use stc_ts_types::{replace::replace_type, Array, ArrayMetadata, Type};
use stc_ts_utils::MapWithMut;
use stc_utils::ext::TypeVecExt;
use swc_common::Spanned;

pub fn normalize_tuples(ty: &mut Type) {
    replace_type(
        ty,
        |ty| {
            if let Type::Tuple(tuple) = ty.normalize() {
                if tuple.metadata.prevent_tuple_to_array {
                    return false;
                }

                if tuple.elems.is_empty() {
                    return false;
                }

                return true;
            }

            false
        },
        |ty| {
            if let Type::Tuple(tuple) = ty.normalize() {
                let common_metadata = tuple.metadata.common;

                let span = ty.span();
                let mut types = ty.take().expect_tuple().elems.into_iter().map(|elem| *elem.ty).collect::<Vec<_>>();
                types.dedup_type();

                let has_other = types.iter().any(|ty| !ty.is_null_or_undefined());
                if has_other {
                    types.retain(|ty| !ty.is_null_or_undefined())
                }

                return Some(Type::Array(Array {
                    span,
                    elem_type: box Type::new_union(span, types),
                    metadata: ArrayMetadata {
                        common: common_metadata,
                        ..Default::default()
                    },
                    tracker: Default::default(),
                }));
            }

            None
        },
    )
}
