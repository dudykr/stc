use std::borrow::Cow;

use num_bigint::BigInt;
use stc_ts_types::{IntrinsicKind, StringMapping, TplElem, TplType, Type};
use swc_atoms::Atom;
use swc_common::{Span, Spanned, TypeEq, DUMMY_SP};
use swc_ecma_ast::TsKeywordTypeKind;

use super::{relation::Relation, Analyzer};
use crate::VResult;

/// These methods are ported from `tsc`.
impl Analyzer<'_, '_> {
    /// `isTypeCloselyMatchedBy` of `tsc`.
    pub(crate) fn is_type_closely_matched_by(&self, source: &Type, target: &Type) -> bool {
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
    pub(crate) fn is_type_identical_to(&self, source: &Type, target: &Type) -> bool {
        self.is_type_related_to(source, target, Relation::Identity)
    }

    /// Ported from `isTypeAssignableTo` of `tsc`.
    pub(crate) fn is_type_assignable_to(&self, span: Span, source: &Type, target: &Type) -> bool {
        self.assign(span, &mut Default::default(), target, source).is_ok()
    }

    /// Ported from `isValidNumberString` of `tsc`.
    pub(crate) fn is_valid_num_str(&self, s: &str, round_trip_only: bool) -> bool {
        if s.is_empty() {
            return false;
        }

        let v = if s.starts_with("0x") || s.starts_with("0X") {
            usize::from_str_radix(&s[2..], 16).ok().map(|v| v as f64)
        } else if s.starts_with("0o") || s.starts_with("0O") {
            usize::from_str_radix(&s[2..], 8).ok().map(|v| v as f64)
        } else if s.starts_with("0b") || s.starts_with("0B") {
            usize::from_str_radix(&s[2..], 2).ok().map(|v| v as f64)
        } else {
            s.parse::<f64>().ok()
        };

        if let Some(v) = v {
            v.is_finite() && (!round_trip_only || v.to_string() == s)
        } else {
            false
        }
    }

    /// Ported from `isValidBigIntString` of `tsc`.
    pub(crate) fn is_valid_big_int_str(&self, s: &str, round_trip_only: bool) -> bool {
        if s.is_empty() {
            return false;
        }

        let v = if s.starts_with("0x") || s.starts_with("0X") {
            BigInt::parse_bytes(s[2..].as_bytes(), 16)
        } else if s.starts_with("0o") || s.starts_with("0O") {
            BigInt::parse_bytes(s[2..].as_bytes(), 8)
        } else if s.starts_with("0b") || s.starts_with("0B") {
            BigInt::parse_bytes(s[2..].as_bytes(), 2)
        } else {
            let unsigned_literal = s.strip_prefix('-').unwrap_or(s);

            // numeric string starting with more than one 0 are incorrect
            if unsigned_literal.starts_with("00") {
                None
            }
            // BigInt strings only accepts numbers
            // 1000n or 1_000n are both considered invalid
            else if unsigned_literal.chars().all(|c| c.is_ascii_digit()) {
                s.parse::<BigInt>().ok()
            } else {
                None
            }
        };
        if let Some(v) = v {
            !round_trip_only || v.to_string() == s
        } else {
            false
        }
    }

    /// Ported from `isMemberOfStringMapping` of `tsc`.
    pub(crate) fn is_member_of_string_mapping(&self, span: Span, source: &Type, target: &Type) -> VResult<bool> {
        if target.is_any() || target.is_kwd(TsKeywordTypeKind::TsStringKeyword) {
            return Ok(true);
        }

        if target.is_tpl() {
            return Ok(self.is_type_assignable_to(span, source, target));
        }

        if let Type::StringMapping(StringMapping {
            kind: IntrinsicKind::Capitalize | IntrinsicKind::Uncapitalize | IntrinsicKind::Uppercase | IntrinsicKind::Lowercase,
            ..
        }) = target.normalize()
        {
            // TODO: Port https://github.com/microsoft/TypeScript/blob/eb5419fc8d980859b98553586dfb5f40d811a745/src/compiler/checker.ts#L22574-L22589
            return Ok(true);
        }

        Ok(false)
    }

    pub(crate) fn get_string_like_type_for_type<'a>(&self, ty: &'a Type) -> Cow<'a, Type> {
        if ty.is_any() || ty.is_str() || ty.is_string_mapping() || ty.is_tpl() {
            Cow::Borrowed(ty)
        } else {
            Cow::Owned(Type::Tpl(TplType {
                span: ty.span(),
                quasis: vec![
                    TplElem {
                        span: DUMMY_SP,
                        value: Atom::default(),
                    },
                    TplElem {
                        span: DUMMY_SP,
                        value: Atom::default(),
                    },
                ],
                types: vec![ty.clone()],
                metadata: Default::default(),
                tracker: Default::default(),
            }))
        }
    }
}
