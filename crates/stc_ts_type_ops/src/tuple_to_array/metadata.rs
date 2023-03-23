use stc_ts_types::{replace::replace_type, Type};
use stc_ts_utils::MapWithMut;
use stc_utils::dev_span;

/// TODO(kdy1): Optimize by visiting only tuple types.
pub fn prevent_tuple_to_array(ty: &mut Type) {
    let _tracing = dev_span!("prevent_tuple_to_array");

    replace_type(
        ty,
        |ty| ty.is_tuple(),
        |ty| {
            let mut ty = ty.take();
            if let Some(tuple) = ty.as_tuple_mut() {
                tuple.metadata.prevent_tuple_to_array = true;
            }
            Some(ty)
        },
    )
}
