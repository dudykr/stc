use stc_ts_types::Interface;

use crate::analyzer::Analyzer;
use crate::ValidationResult;

impl Analyzer<'_, '_> {
    pub(super) fn infer_type_interface_and_interface(
        &mut self,
        param: &Interface,
        arg: &Interface,
    ) -> ValidationResult<()> {
    }
}
