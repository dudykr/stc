use crate::analyzer::Analyzer;

impl Analyzer<'_, '_> {
    pub(super) fn assign_class_members_to_type_element(
        &mut self,
        opts: AssignOpts,
        el: &TypeElement,
        rhs_members: &[stc_ts_types::ClassMember],
    ) -> ValidationResult<()> {
        Ok(())
    }
}
