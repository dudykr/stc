use crate::analyzer::Analyzer;
use crate::ValidationResult;
use stc_ts_types::Type;

impl Analyzer<'_, '_> {
    pub(super) fn expand_return_type_of_fn(&mut self, ret_ty: &mut Type) -> ValidationResult<()> {}
}
