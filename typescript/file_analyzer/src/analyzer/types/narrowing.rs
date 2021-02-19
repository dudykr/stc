use stc_ts_types::Type;

use crate::analyzer::Analyzer;
use crate::ValidationResult;

impl Analyzer<'_, '_> {
    pub(crate) fn narrowed_type_of_assignment(&mut self, declared: Type, actual: &Type) -> ValidationResult {}
}
