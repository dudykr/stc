use stc_ts_errors::debug::dump_type_as_string;
use stc_ts_types::{replace::replace_type, LitType, Type};
use stc_ts_utils::MapWithMut;
use stc_utils::dev_span;
use tracing::debug;

pub fn prevent_generalize(ty: &mut Type) {
    let _tracing = dev_span!("prevent_generalize");

    debug!("Prevent generalize: {}", dump_type_as_string(ty));

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
