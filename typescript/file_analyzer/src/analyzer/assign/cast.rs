use crate::analyzer::Analyzer;
use stc_ts_types::Type;
use std::borrow::Cow;
use swc_common::Span;

impl Analyzer<'_, '_> {
    /// Returns true if the type can be casted to number if it's in the rvalue
    /// position.
    pub(crate) fn can_be_casted_to_number_in_rhs(&mut self, span: Span, ty: &Type) -> bool {
        let ty = ty.normalize();

        if ty.is_num() {
            return true;
        }

        match ty {
            Type::Ref(..) => {
                if let Some(expanded) = self.expand_top_ref(span, Cow::Borrowed(ty)).ok().map(Cow::into_owned) {
                    return self.can_be_casted_to_number_in_rhs(span, &expanded);
                }

                false
            }
            Type::Enum(e) => !e.has_str,
            Type::Union(ty) => ty.types.iter().all(|ty| self.can_be_casted_to_number_in_rhs(span, &ty)),
            _ => false,
        }
    }
}
