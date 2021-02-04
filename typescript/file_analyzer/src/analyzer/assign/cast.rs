use crate::analyzer::Analyzer;
use stc_ts_types::Type;

impl Analyzer<'_, '_> {
    pub(super) fn can_be_casted_to_number_in_rhs(&mut self, ty: &Type) -> bool {
        let ty = ty.normalize();

        match ty {
            Type::Enum(e) => !e.has_str,
            _ => false,
        }
    }
}
