use stc_ts_types::{replace::replace_type, LitType, Type};
use stc_ts_utils::MapWithMut;
use tracing::instrument;

#[instrument(skip(ty))]
pub fn prevent_generalize(ty: &mut Type) {
    replace_type(
        ty,
        |ty| {
            if let Type::Lit(LitType { metadata, .. }) = ty.normalize() {
                if metadata.common.prevent_generalization {
                    return false;
                }

                return true;
            }

            false
        },
        |ty| {
            let mut ty = ty.take();
            ty.metadata_mut().prevent_generalization = true;
            Some(ty)
        },
    )
}
