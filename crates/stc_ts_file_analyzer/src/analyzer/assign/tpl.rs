#![allow(clippy::if_same_then_else)]

use std::borrow::Cow;

use stc_ts_ast_rnode::RTsLit;
use stc_ts_errors::{debug::force_dump_type_as_string, ErrorKind};
use stc_ts_types::{IntrinsicKind, LitType, StringMapping, TplType, Type};
use stc_utils::dev_span;
use swc_common::{Span, Spanned, TypeEq};
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
            if !self.is_valid_type_for_tpl_lit_placeholder(span, ty, &l.types[i], data)? {
                return Err(ErrorKind::SimpleAssignFailed { span, cause: None }.context(format!(
                    "verified types:\nsource = {}\ntarget = {}",
                    force_dump_type_as_string(ty),
                    force_dump_type_as_string(&l.types[i])
                )));
            }
        }

        Ok(())
    }

    /// Ported from `isValidTypeForTemplateLiteralPlaceholder` of `tsc`
    pub(crate) fn is_valid_type_for_tpl_lit_placeholder(
        &mut self,
        span: Span,
        source: &Type,
        target: &Type,
        data: &mut AssignData,
    ) -> VResult<bool> {
        let _tracing = dev_span!(
            "is_valid_type_for_tpl_lit_placeholder",
            source = tracing::field::display(&force_dump_type_as_string(source)),
            target = tracing::field::display(&force_dump_type_as_string(target)),
        );

        if source.type_eq(target) || target.is_any() || target.is_kwd(TsKeywordTypeKind::TsStringKeyword) {
            return Ok(true);
        }

        match source.normalize() {
            Type::Lit(LitType {
                lit: RTsLit::Str(value), ..
            }) => {
                // TODO: Validate literal value correctly
                if target.is_num() && self.is_valid_num_str(&value.value, false)
                    || target.is_bigint() && self.is_valid_big_int_str(&value.value, false)
                {
                    return Ok(true);
                }

                if target.is_bool() {
                    let value = &*value.value;

                    match value {
                        "true" | "false" => {
                            return Ok(true);
                        }
                        "TRUE" | "FALSE" if !data.dejavu.is_empty() => {
                            if let Type::Instance(ty) = &data.dejavu[0].0.normalize() {
                                let ty = &ty.ty;
                                let t = self.expand_top_ref(ty.span(), Cow::Owned(ty.normalize().clone()), Default::default());

                                if let Ok(ty) = t {
                                    if let Type::StringMapping(StringMapping {
                                        kind: IntrinsicKind::Uppercase,
                                        ..
                                    }) = ty.normalize()
                                    {
                                        return Ok(true);
                                    }
                                }
                            }
                        }
                        "True" | "False" if !data.dejavu.is_empty() => {
                            if let Type::Instance(ty) = &data.dejavu[0].0.normalize() {
                                let ty = &ty.ty;
                                let t = self.expand_top_ref(ty.span(), Cow::Owned(ty.normalize().clone()), Default::default());

                                if let Ok(ty) = t {
                                    if let Type::StringMapping(StringMapping {
                                        kind: IntrinsicKind::Capitalize,
                                        ..
                                    }) = ty.normalize()
                                    {
                                        return Ok(true);
                                    }
                                }
                            }
                        }
                        _ => {}
                    };
                }

                // TODO: Check for `source`
                match &*value.value {
                    "true" | "false" | "null" | "undefined" => return Ok(true),
                    _ => {}
                }

                if let Type::StringMapping(StringMapping {
                    kind: IntrinsicKind::Capitalize | IntrinsicKind::Uncapitalize | IntrinsicKind::Uppercase | IntrinsicKind::Lowercase,
                    ..
                }) = target.normalize()
                {
                    return self.is_member_of_string_mapping(span, source, target);
                }

                // TODO(kdy1): Return `Ok(false)` instead
            }

            Type::Tpl(source) => {
                if source.quasis.len() == 2 && source.quasis[0].value == "" && source.quasis[1].value == "" {
                    // TODO(kdy1): Return `Ok(self.is_type_assignable_to(span, &source.types[0],
                    // target))` instead
                    if self.is_type_assignable_to(span, &source.types[0], target) {
                        return Ok(true);
                    }
                } else {
                    return Ok(false);
                }
            }

            Type::StringMapping(str_map) => {
                if let Type::Tpl(tpl) = &str_map.type_args.params[0].normalize() {
                    if self.is_type_assignable_to(span, &tpl.types[0], target) {
                        return Ok(true);
                    }
                }

                return Ok(false);
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
