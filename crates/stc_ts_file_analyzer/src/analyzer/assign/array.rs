use std::{borrow::Cow, cmp::min};

use stc_ts_ast_rnode::RNumber;
use stc_ts_errors::{debug::force_dump_type_as_string, DebugExt, ErrorKind};
use stc_ts_types::{Array, IdCtx, Key, KeywordType, RestType, Tuple, Type};
use stc_utils::{cache::Freeze, dev_span};
use swc_ecma_ast::TsKeywordTypeKind;

use super::{AssignData, AssignOpts};
use crate::{
    analyzer::{
        assign::get_tuple_subtract_count,
        expr::{AccessPropertyOpts, TypeOfMode},
        Analyzer,
    },
    VResult,
};

impl Analyzer<'_, '_> {
    /// Returns `Ok(Some())` if the assignment is handled.
    pub(super) fn assign_to_tuple(
        &mut self,
        data: &mut AssignData,
        l: &Tuple,
        l_type: &Type,
        rhs: &Type,
        opts: AssignOpts,
    ) -> VResult<Option<()>> {
        let span = opts.span;

        macro_rules! fail {
            () => {{
                fail!(vec![])
            }};

            ($errors:expr) => {{
                return Err(ErrorKind::AssignFailed {
                    span,
                    left: Box::new(l_type.clone()),
                    right: Box::new(rhs.clone()),
                    right_ident: opts.right_ident_span,
                    cause: $errors,
                }
                .context(format!(
                    "LHS (final): {}\nRHS (final): {}",
                    force_dump_type_as_string(l_type),
                    force_dump_type_as_string(rhs)
                )));
            }};
        }

        if l.elems.is_empty() {
            match rhs {
                Type::Array(..) | Type::Tuple(..) => return Ok(Some(())),
                _ => {}
            }
        }

        match *rhs.normalize() {
            Type::Tuple(Tuple { elems: ref rhs_elems, .. }) => {
                if rhs_elems.is_empty() {
                    fail!()
                }

                if !opts.ignore_tuple_length_difference && l.elems.len() < rhs_elems.len() {
                    if l.elems.iter().any(|elem| elem.ty.is_rest()) {
                        // Type::Rest eats many elements
                    } else {
                        return Err(ErrorKind::AssignFailedBecauseTupleLengthDiffers { span }.into());
                    }
                }

                if !opts.ignore_tuple_length_difference && l.elems.len() > rhs_elems.len() {
                    let is_len_fine = rhs_elems.iter().any(|elem| elem.ty.is_rest())
                        || l.elems.iter().skip(rhs_elems.len()).all(|l| {
                            matches!(
                                l.ty.normalize_instance(),
                                Type::Keyword(KeywordType {
                                    kind: TsKeywordTypeKind::TsAnyKeyword,
                                    ..
                                }) | Type::Optional(..)
                            )
                        });

                    if !is_len_fine {
                        return Err(ErrorKind::AssignFailedBecauseTupleLengthDiffers { span }.into());
                    }
                }

                let len = l.elems.len().max(rhs_elems.len());

                let r_max = rhs_elems.len().saturating_sub(get_tuple_subtract_count(&l.elems));

                let mut errors = vec![];

                for index in 0..len {
                    let li = index;
                    let ri = min(index, r_max);

                    let _tracing = dev_span!("assign_tuple_to_tuple", li = li, ri = ri);

                    let l_elem_type = self.access_property(
                        span,
                        l_type,
                        &Key::Num(RNumber {
                            span,
                            value: li as _,
                            raw: None,
                        }),
                        TypeOfMode::RValue,
                        IdCtx::Type,
                        AccessPropertyOpts {
                            do_not_validate_type_of_computed_prop: true,
                            disallow_indexing_array_with_string: true,
                            disallow_creating_indexed_type_from_ty_els: true,
                            disallow_indexing_class_with_computed: true,
                            use_undefined_for_tuple_index_error: true,
                            ..Default::default()
                        },
                    )?;

                    let r_elem_type = self.access_property(
                        span,
                        rhs,
                        &Key::Num(RNumber {
                            span,
                            value: ri as _,
                            raw: None,
                        }),
                        TypeOfMode::RValue,
                        IdCtx::Type,
                        AccessPropertyOpts {
                            do_not_validate_type_of_computed_prop: true,
                            disallow_indexing_array_with_string: true,
                            disallow_creating_indexed_type_from_ty_els: true,
                            disallow_indexing_class_with_computed: true,
                            use_undefined_for_tuple_index_error: true,
                            ..Default::default()
                        },
                    )?;

                    errors.extend(
                        self.assign_inner(
                            data,
                            &l_elem_type,
                            &r_elem_type,
                            AssignOpts {
                                allow_unknown_rhs: Some(true),
                                ..opts
                            },
                        )
                        .with_context(|| format!("tried to assign {}th tuple element\nli = {},ri = {}", index, li, ri))
                        .err(),
                    );
                }

                if !errors.is_empty() {
                    // We should use single error for tuple with rest in some cases.
                    if (!opts.do_not_use_single_error_for_tuple_with_rest && l.elems.iter().any(|elem| elem.ty.is_rest()))
                        || rhs.metadata().resolved_from_var
                    {
                        fail!(errors);
                    }

                    return Err(ErrorKind::TupleAssignError { span, errors }.into());
                }

                return Ok(Some(()));
            }
            Type::Array(Array {
                elem_type: ref rhs_elem_type,
                ..
            }) => {
                if l.elems.len() != 1 {
                    fail!();
                }

                match l.elems[0].ty.normalize() {
                    Type::Rest(RestType { ty: l_ty, .. }) => {
                        self.assign_inner(
                            data,
                            l_ty,
                            rhs_elem_type,
                            AssignOpts {
                                allow_unknown_rhs: Some(true),
                                ..opts
                            },
                        )?;
                    }
                    _ => {
                        fail!();
                    }
                }
            }

            Type::Interface(..) | Type::TypeLit(..) => {
                if let Some(tuple) = self.convert_type_to_type_lit(span, Cow::Borrowed(l_type), Default::default())? {
                    return self
                        .assign_to_type_elements(
                            data,
                            tuple.span,
                            &tuple.members,
                            rhs,
                            tuple.metadata,
                            AssignOpts {
                                allow_unknown_rhs: Some(false),
                                allow_missing_fields: false,
                                ..opts
                            },
                        )
                        .convert_err(|err| match &err {
                            ErrorKind::Errors { span, errors }
                                if errors
                                    .iter()
                                    .all(|err| matches!(&**err, ErrorKind::UnknownPropertyInObjectLiteralAssignment { .. })) =>
                            {
                                ErrorKind::SimpleAssignFailed {
                                    span: *span,
                                    cause: Some(Box::new(err.context("union errors because we are assigning to tuple"))),
                                }
                            }
                            _ => err,
                        })
                        .context("tried to assign to a type literal created from a tuple")
                        .map(Some);
                }

                fail!()
            }
            Type::Index(..) | Type::Lit(..) | Type::Keyword(..) | Type::Class(..) | Type::ClassDef(..) if !opts.allow_iterable_on_rhs => {
                fail!()
            }

            _ => {
                // Try to assign by converting rhs to an iterable.
                if opts.allow_iterable_on_rhs {
                    let r = self
                        .get_iterator(span, Cow::Borrowed(rhs), Default::default())
                        .context("tried to convert a type to an iterator to assign to a tuple")?;
                    //
                    for (i, elem) in l.elems.iter().enumerate() {
                        let r_ty = self
                            .get_element_from_iterator(span, Cow::Borrowed(&r), i)
                            .context("tried to get an element of type to assign to a tuple element")?
                            .freezed();

                        self.assign_with_opts(
                            data,
                            &elem.ty,
                            &r_ty,
                            AssignOpts {
                                allow_iterable_on_rhs: false,
                                ..opts
                            },
                        )?;
                    }

                    return Ok(Some(()));
                }
            }
        }

        Ok(None)
    }
}
