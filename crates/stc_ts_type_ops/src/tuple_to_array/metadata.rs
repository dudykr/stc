use stc_ts_types::{replace::replace_type, Type};
use stc_ts_utils::MapWithMut;
use tracing::instrument;

/// TODO(kdy1): Optimize by visiting only tuple types.
#[instrument(skip_all)]
pub fn prevent_tuple_to_array(ty: &mut Type) {
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
