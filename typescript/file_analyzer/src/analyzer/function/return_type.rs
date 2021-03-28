use crate::analyzer::Analyzer;
use crate::ValidationResult;
use rnode::VisitMut;
use rnode::VisitMutWith;
use stc_ts_types::Type;

impl Analyzer<'_, '_> {
    pub(super) fn expand_return_type_of_fn(&mut self, ret_ty: &mut Type) -> ValidationResult<()> {
        Ok(())
    }
}

struct FnReturnTypeHandler<'a, 'b, 'c> {
    analyzer: &'a mut Analyzer<'b, 'c>,
}

impl VisitMut<Type> for FnReturnTypeHandler<'_, '_, '_> {
    fn visit_mut(&mut self, ty: &mut Type) {
        ty.visit_mut_children_with(self);
    }
}
