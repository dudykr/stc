#![allow(clippy::if_same_then_else)]

use stc_ts_ast_rnode::RTsLit;
use stc_ts_errors::{debug::force_dump_type_as_string, ErrorKind};
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

        let inference = self.infer_types_from_tpl_lit_type(span, r_ty, l)?;

        let inference = match inference {
            Some(inference) => inference,
            None => return Err(ErrorKind::SimpleAssignFailed { span, cause: None }.context("tried to infer")),
        };

        for (i, ty) in inference.iter().enumerate() {
            if !self.is_valid_type_for_tpl_lit_placeholder(span, ty, &l.types[i])? {
                return Err(ErrorKind::SimpleAssignFailed { span, cause: None }.context(format!(
                    "verified types:{}\n{}",
                    force_dump_type_as_string(ty),
                    force_dump_type_as_string(&l.types[i])
                )));
            }
        }

        Ok(())
    }

    /// Ported from `isValidTypeForTemplateLiteralPlaceholder` of `tsc`
    pub(crate) fn is_valid_type_for_tpl_lit_placeholder(&mut self, span: Span, source: &Type, target: &Type) -> VResult<bool> {
        #[cfg(debug_assertions)]
        let _tracing = {
            let source = force_dump_type_as_string(source);
            let target = force_dump_type_as_string(target);
            tracing::span!(
                tracing::Level::ERROR,
                "is_valid_type_for_tpl_lit_placeholder",
                source = tracing::field::display(&source),
                target = tracing::field::display(&target),
            )
            .entered()
        };

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

                if target.is_bool_lit() || target.is_null_or_undefined() {
                    // TODO: Return true if
                    // value === (target as IntrinsicType).intrinsicName
                    return Ok(true);
                }

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
                    // TODO(kdy1): We should uncomment line below. But I don't know why tsc is not
                    // creating a string-like type for source

                    return Ok(self.is_type_assignable_to(span, &source.types[0], target));

                    // let source =
                    // self.get_string_like_type_for_type(&source.types[0]);
                    // return Ok(self.is_type_assignable_to(span, &source,
                    // target));
                } else {
                    return Ok(false);
                }
            }

            _ => {}
        }

        Ok(self.is_type_assignable_to(span, source, target))
    }

    /// Ported from `templateLiteralTypesDefinitelyUnrelated` of `tsc`.
    pub(crate) fn tpl_lit_type_definitely_unrelated(&mut self, span: Span, source: &TplType, target: &TplType) -> VResult<bool> {
        // Two template literal types with differences in their starting or ending text
        // spans are definitely unrelated.

        let source_start = &source.quasis[0].value;
        let target_start = &target.quasis[0].value;
        let source_end = &source.quasis[source.quasis.len() - 1].value;
        let target_end = &target.quasis[target.quasis.len() - 1].value;
        let start_len = source_start.len().min(target_start.len());
        let end_len = source_end.len().min(target_end.len());

        Ok(source_start[0..start_len] != target_start[0..start_len]
            || source_end[(source_end.len() - end_len)..] != target_end[(target_end.len() - end_len)..])
    }
}
