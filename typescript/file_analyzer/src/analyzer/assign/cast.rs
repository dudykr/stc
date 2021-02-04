use crate::analyzer::Analyzer;
use stc_ts_types::Type;

impl Analyzer<'_, '_> {
    /// Returns true if the type can be casted to number if it's in the rvalue
    /// position.
    pub(crate) fn can_be_casted_to_number_in_rhs(&mut self, ty: &Type) -> bool {
        let ty = ty.normalize();

        match ty {
            Type::Enum(e) => !e.has_str,
            Type::Union(ty) => ty.types.iter().all(|ty| self.can_be_casted_to_number_in_rhs(&ty)),
            _ => false,
        }
    }
}
