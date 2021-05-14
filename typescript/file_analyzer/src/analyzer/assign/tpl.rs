use super::AssignOpts;
use crate::analyzer::Analyzer;
use crate::ValidationResult;
use stc_ts_types::TplType;
use stc_ts_types::Type;

impl Analyzer<'_, '_> {
    pub(crate) fn assign_to_tpl(&mut self, l: &TplType, r: &Type, opts: AssignOpts) -> ValidationResult<()> {
        Ok(())
    }
}
