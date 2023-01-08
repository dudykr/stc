#![allow(clippy::if_same_then_else)]

use stc_ts_ast_rnode::RTsLit;
use stc_ts_errors::ErrorKind;
use stc_ts_types::{Intrinsic, IntrinsicKind, LitType, TplType, Type};
use swc_common::{Span, TypeEq};
use swc_ecma_ast::TsKeywordTypeKind;

use crate::{
    analyzer::{
        assign::{AssignData, AssignOpts},
        Analyzer,
    },
    VResult,
};

impl Analyzer<'_, '_> {
    /// # Implementation notes
    ///
    /// We split string based on the literals.
    ///
    ///
    /// `:${string}:::${string}:` = ":1:sss:s:s:s:s::s:s:"
    ///
    /// For the code above, we try to find `:`, `:::`, `:`, while preserving
    /// orders.
    ///
    /// After splitting, we can check if each element is assignable.
    pub(crate) fn assign_to_tpl(&mut self, data: &mut AssignData, l: &TplType, r_ty: &Type, opts: AssignOpts) -> VResult<()> {
        let span = opts.span;
        let r_ty = r_ty.normalize();

        let types = self.infer_types_from_tpl_lit_type(span, r_ty, l)?;

        let types = match types {
            Some(types) => types,
            None => return Err(ErrorKind::SimpleAssignFailed { span, cause: None }.context("tried to infer")),
        };

        for (i, ty) in types.iter().enumerate() {
            if !self.is_valid_type_for_tpl_lit_placeholder(span, ty, &l.types[i])? {
                return Err(ErrorKind::SimpleAssignFailed { span, cause: None }.context("verified types"));
            }
        }

        Ok(())
    }

    /// Ported from `isValidTypeForTemplateLiteralPlaceholder` of `tsc`
    pub(crate) fn is_valid_type_for_tpl_lit_placeholder(&mut self, span: Span, source: &Type, target: &Type) -> VResult<bool> {
        if source.type_eq(target) || target.is_any() || target.is_kwd(TsKeywordTypeKind::TsStringKeyword) {
            return Ok(true);
        }

        match source.normalize() {
            Type::Lit(LitType {
                lit: RTsLit::Str(value), ..
            }) => {
                if target.is_kwd(TsKeywordTypeKind::TsNumberKeyword) && self.is_valid_num_str(&value.value, false)
                    || target.is_kwd(TsKeywordTypeKind::TsBigIntKeyword) && self.is_valid_big_int_str(&value.value, false)
                {
                    return Ok(true);
                }

                // TODO: Return true if

                // target.flags & (TypeFlags.BooleanLiteral |
                // TypeFlags.Nullable) && value === (target as
                // IntrinsicType).intrinsicName

                return match target.normalize() {
                    Type::Intrinsic(Intrinsic {
                        kind: IntrinsicKind::Capitalize | IntrinsicKind::Uncapitalize | IntrinsicKind::Uppercase | IntrinsicKind::Lowercase,
                        ..
                    }) => self.is_member_of_string_mapping(span, source, target),
                    _ => Ok(false),
                };
            }

            Type::Tpl(source) => {
                if source.quasis.len() == 2 && source.quasis[0].value == "" && source.quasis[1].value == "" {
                    return Ok(self.is_type_assignable_to(span, &source.types[0], target));
                } else {
                    return Ok(false);
                }
            }

            _ => {}
        }

        Ok(self.is_type_assignable_to(span, source, target))
    }
}
