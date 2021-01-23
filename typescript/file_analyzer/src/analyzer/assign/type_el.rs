use super::AssignOpts;
use crate::analyzer::Analyzer;
use stc_ts_types::ClassMember;
use stc_ts_types::TypeElement;

impl Analyzer<'_, '_> {
    pub(super) fn assign_class_members_to_type_element(
        &mut self,
        opts: AssignOpts,
        el: &TypeElement,
        rhs_members: &[ClassMember],
    ) -> ValidationResult<()> {
        Ok(())
    }
}
