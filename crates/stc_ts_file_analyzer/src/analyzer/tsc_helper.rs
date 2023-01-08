use stc_ts_types::{Intrinsic, IntrinsicKind, Type};
use swc_common::{Span, TypeEq};
use swc_ecma_ast::TsKeywordTypeKind;

use super::{
    relation::{IsRelatedOpts, Relation},
    Analyzer,
};
use crate::VResult;

/// These methods are ported from `tsc`.
impl Analyzer<'_, '_> {
    /// `isTypeCloselyMatchedBy` of `tsc`.
    pub(crate) fn is_type_closely_matched_by(&mut self, source: &Type, target: &Type) -> bool {
        match (source.normalize(), target.normalize()) {
            (Type::Ref(source), Type::Ref(target)) => source.type_name.type_eq(&target.type_name),
            _ => false,
        }
    }

    /// `isTypeOrBaseIdenticalTo` of `tsc`.
    pub(crate) fn is_type_or_base_identical_to(&mut self, source: &Type, target: &Type) -> bool {
        self.is_type_identical_to(source, target)
            || (target.is_kwd(TsKeywordTypeKind::TsStringKeyword) && source.is_str_lit())
            || (target.is_kwd(TsKeywordTypeKind::TsNumberKeyword) && source.is_num_lit())
    }

    /// `isTypeIdenticalTo` of `tsc`.
    pub(crate) fn is_type_identical_to(&mut self, source: &Type, target: &Type) -> bool {
        self.is_related_to(
            source,
            target,
            IsRelatedOpts {
                kind: Relation::Identical,
                ..Default::default()
            },
        )
    }

    /// Ported from `isTypeAssignableTo` of `tsc`.
    pub(crate) fn is_type_assignable_to(&mut self, span: Span, source: &Type, target: &Type) -> bool {
        self.assign(span, &mut Default::default(), target, source).is_ok()
    }

    /// Ported from `isValidNumberString` of `tsc`.
    pub(crate) fn is_valid_num_str(&mut self, s: &str, round_trip_only: bool) -> bool {
        if s == "" {
            return false;
        }

        if let Ok(v) = s.parse::<f64>() {
            v.is_finite() && (!round_trip_only || v.to_string() == s)
        } else {
            false
        }
    }

    /// Ported from `isValidBigIntString` of `tsc`.
    pub(crate) fn is_valid_big_int_str(&mut self, s: &str, round_trip_only: bool) -> bool {
        if s == "" {
            return false;
        }
    }

    /// Ported from `isMemberOfStringMapping` of `tsc`.
    pub(crate) fn is_member_of_string_mapping(&mut self, span: Span, source: &Type, target: &Type) -> VResult<bool> {
        if target.is_any() || target.is_kwd(TsKeywordTypeKind::TsStringKeyword) {
            return Ok(true);
        }

        if target.is_tpl() {
            return Ok(self.is_type_assignable_to(span, source, target));
        }

        match target.normalize() {
            Type::Intrinsic(Intrinsic {
                kind: IntrinsicKind::Capitalize | IntrinsicKind::Uncapitalize | IntrinsicKind::Uppercase | IntrinsicKind::Lowercase,
                ..
            }) => {
                // TODO: Port https://github.com/microsoft/TypeScript/blob/eb5419fc8d980859b98553586dfb5f40d811a745/src/compiler/checker.ts#L22574-L22589
            }
            _ => {}
        }

        Ok(false)
    }
}
