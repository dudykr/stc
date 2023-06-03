use std::borrow::Cow;

use stc_ts_types::Type;
use swc_common::Span;
use swc_ecma_ast::TsKeywordTypeKind;

use crate::analyzer::{types::NormalizeTypeOpts, Analyzer};

impl Analyzer<'_, '_> {
    /// Returns true if the type can be casted to number if it's in the rvalue
    /// position.
    pub(crate) fn can_be_casted_to_number_in_rhs(&self, span: Span, ty: &Type) -> bool {
        let ty = match self.normalize(
            Some(span),
            Cow::Borrowed(ty),
            NormalizeTypeOpts {
                preserve_global_this: true,
                preserve_union: true,
                ..Default::default()
            },
        ) {
            Ok(v) => v.into_owned(),
            _ => return false,
        };

        if ty.is_num() {
            return true;
        }

        // TODO(kdy1): Maybe make this check optional, but only if tsc reports error for
        // number + bigint
        if ty.is_kwd(TsKeywordTypeKind::TsBigIntKeyword) {
            return true;
        }

        match ty.normalize() {
            Type::EnumVariant(e) => {
                // TODO(kdy1): Check if value is string
                true
            }
            Type::Enum(e) => !e.has_str,
            Type::Union(ty) => ty.types.iter().all(|ty| self.can_be_casted_to_number_in_rhs(span, ty)),
            Type::Intersection(ty) => ty.types.iter().any(|ty| self.can_be_casted_to_number_in_rhs(span, ty)),
            _ => false,
        }
    }
}
