use std::{borrow::Cow, collections::HashMap};

use stc_ts_ast_rnode::{RBool, RIdent, RStr, RTsEntityName, RTsEnumMemberId, RTsLit};
use stc_ts_errors::{
    debug::{dump_type_as_string, force_dump_type_as_string},
    DebugExt, ErrorKind,
};
use stc_ts_types::{
    Array, Conditional, EnumVariant, Index, Instance, Interface, Intersection, IntrinsicKind, Key, KeywordType, LitType, Mapped,
    PropertySignature, QueryExpr, QueryType, Readonly, Ref, StringMapping, ThisType, Tuple, TupleElement, Type, TypeElement, TypeLit,
    TypeParam,
};
use stc_utils::{cache::Freeze, dev_span, ext::SpanExt, stack};
use swc_atoms::js_word;
use swc_common::{EqIgnoreSpan, Span, Spanned, TypeEq, DUMMY_SP};
use swc_ecma_ast::{TruePlusMinus::*, *};
use tracing::{debug, error, info};

use super::pat::PatMode;
use crate::{
    analyzer::{types::NormalizeTypeOpts, util::is_lit_eq_ignore_span, Analyzer},
    ty::TypeExt,
    util::is_str_or_union,
    VResult,
};

mod array;
mod builtin;
mod cast;
mod class;
mod function;
#[cfg(test)]
mod tests;
mod tpl;
mod type_el;
mod unions;

/// Context used for `=` assignments.
#[derive(Debug, Clone, Copy, Default)]
pub(crate) struct AssignOpts {
    /// This field should be overriden by caller.
    pub span: Span,
    pub right_ident_span: Option<Span>,

    // Span of X in expr `X = Y`
    // This is used for better error display
    pub left_ident_span: Option<Span>,

    /// # Values
    ///
    /// - `Some(false)`: `inexact` and `specified` of [TypeLitMetadata] are
    ///   ignored.
    /// - `Some(true)`: extra properties are allowed.
    /// - `None`: It depends on `inexact` and `specified` of [TypeLitMetadata]
    ///
    /// # Usages
    ///
    /// - `Some(false)` is Used for `extends` check.
    pub allow_unknown_rhs: Option<bool>,

    pub allow_missing_fields: bool,

    /// Allow assigning `unknown` type to other types. This should be `true` for
    /// parameters because the following is valid.
    ///
    ///
    /// ```ts
    ///   declare var a: {
    ///     (a:[2]): void
    ///   }
    ///
    ///   declare var b: {
    ///     (a:[unknown]): void
    ///   }
    ///
    ///   a = b;
    /// ```
    pub allow_unknown_type: bool,

    /// Allow assignment to [Type::Param].
    pub allow_assignment_to_param: bool,

    pub allow_assignment_of_param: bool,
    pub skip_call_and_constructor_elem: bool,

    pub for_overload: bool,

    pub disallow_assignment_to_unknown: bool,
    /// This is `true` for variable overloads, too. This will be fixed in
    /// future.
    pub for_castablity: bool,

    /// If this is `false`, assignment of literals or some other strange type to
    /// empty class will success.
    pub disallow_special_assignment_to_empty_class: bool,

    /// If true, assignment of a class to another class without inheritance
    /// relation will fail, even if the class is empty.
    pub disallow_different_classes: bool,

    /// If true, `assign` will try to assign by converting rhs to an iterable.
    pub allow_iterable_on_rhs: bool,

    /// If `true`, assignment will success if rhs is `void`.
    /// [None] means `false`.
    ///
    /// This is [Option] because it's required.
    pub allow_assignment_of_void: Option<bool>,

    /// If `true`, assignment will success if lhs is `void`.
    pub allow_assignment_to_void: bool,

    pub allow_assignment_of_array_to_optional_type_lit: bool,

    pub use_missing_fields_for_class: bool,

    pub allow_assignment_to_param_constraint: bool,

    /// The code below is valid.
    ///
    /// ```ts
    /// declare var p: Promise<Promise<string>>
    ///
    /// async function foo(): Promise<string> {
    ///     return p
    /// }
    /// ```
    pub may_unwrap_promise: bool,

    /// contextualTYpeWithUnionTypeMembers says
    ///
    /// > When used as a contextual type, a union type U has those members that
    /// > are present in any of its constituent types, with types that are
    /// > unions of the respective members in the constituent types.
    ///
    /// And
    ///
    /// ```ts
    /// var i1Ori2: I1<number> | I2<number> = { // Like i1 and i2 both
    ///     commonPropertyType: "hello",
    ///     commonMethodType: a=> a,
    ///     commonMethodWithTypeParameter: a => a,
    ///     methodOnlyInI1: a => a,
    ///     propertyOnlyInI1: "Hello",
    ///     methodOnlyInI2: a => a,
    ///     propertyOnlyInI2: "Hello",
    /// };
    /// ```
    ///
    /// is valid but
    ///
    ///
    /// ```ts
    /// function f13(x: { a: null; b: string } | { a: string, c: number }) {
    ///     x = { a: null, b: "foo", c: 4};  // Error
    /// }
    /// ```
    ///
    /// and
    ///
    /// ```ts
    /// var test: { a: null; b: string } | { a: string, c: number } = { a: null, b: "foo", c: 4}
    /// ```
    /// are not.
    pub allow_unknown_rhs_if_expanded: bool,

    pub infer_type_params_of_left: bool,

    pub is_assigning_to_class_members: bool,

    // Method definitions are bivariant (method shorthand)
    pub is_params_of_method_definition: bool,

    pub treat_array_as_interfaces: bool,

    pub do_not_convert_enum_to_string_nor_number: bool,

    pub ignore_enum_variant_name: bool,
    pub ignore_tuple_length_difference: bool,

    /// Used to prevent recursion
    pub do_not_normalize_intersection_on_rhs: bool,

    /// Use `TS2322` on missing properties.
    pub report_assign_failure_for_missing_properties: Option<bool>,

    pub do_not_use_single_error_for_tuple_with_rest: bool,
}

#[derive(Default)]
pub struct AssignData {
    dejavu: Vec<(Type, Type)>,
}

impl Analyzer<'_, '_> {
    /// Denies `null` and `undefined`. This method does not check for elements
    /// of union.
    pub(crate) fn deny_null_or_undefined(&mut self, span: Span, ty: &Type) -> VResult<()> {
        if ty.is_kwd(TsKeywordTypeKind::TsUndefinedKeyword) {
            return Err(ErrorKind::ObjectIsPossiblyUndefined { span }.into());
        }

        if ty.is_kwd(TsKeywordTypeKind::TsNullKeyword) {
            return Err(ErrorKind::ObjectIsPossiblyNull { span }.into());
        }

        Ok(())
    }

    /// Used to validate assignments like `a += b`.
    pub(crate) fn assign_with_operator(&mut self, span: Span, op: AssignOp, lhs: &Type, rhs: &Type) -> VResult<()> {
        debug_assert_ne!(op, op!("="));

        let l = self.normalize(
            Some(span),
            Cow::Borrowed(lhs),
            NormalizeTypeOpts {
                preserve_global_this: true,
                ..Default::default()
            },
        )?;
        let r = self.normalize(
            Some(span),
            Cow::Borrowed(rhs),
            NormalizeTypeOpts {
                preserve_global_this: true,
                ..Default::default()
            },
        )?;

        let lhs = l.normalize();
        let rhs = r.normalize();

        if op == op!("+=") {
            if lhs.is_enum_variant() {
                if rhs.is_type_lit() || rhs.is_bool() || rhs.is_symbol_like() {
                    return Err(ErrorKind::OperatorCannotBeAppliedToTypes { span }.into());
                }
            }
        }

        match op {
            op!("*=")
            | op!("**=")
            | op!("/=")
            | op!("%=")
            | op!("-=")
            | op!("&=")
            | op!("|=")
            | op!("^=")
            | op!("<<=")
            | op!(">>=")
            | op!(">>>=") => {
                if lhs.is_symbol_like() {
                    return Err(ErrorKind::WrongTypeForLhsOfNumericOperation { span }.into());
                }
            }
            _ => {}
        }

        let mut skip_check_null_or_undefined_of_rhs = false;
        match op {
            op!("*=") | op!("**=") | op!("/=") | op!("%=") | op!("-=") => {
                if let Type::Keyword(KeywordType {
                    kind: TsKeywordTypeKind::TsUndefinedKeyword | TsKeywordTypeKind::TsNullKeyword,
                    ..
                }) = rhs
                {
                    if op == op!("**=") {
                        skip_check_null_or_undefined_of_rhs = true;
                    }
                    if op != op!("**=") && !self.rule().strict_null_checks && (l.is_num() || l.is_enum_variant()) {
                        skip_check_null_or_undefined_of_rhs = true;
                    } else {
                        self.storage
                            .report(ErrorKind::UndefinedOrNullIsNotValidOperand { span: rhs.span() }.into());
                    }
                } else {
                    self.deny_null_or_undefined(rhs.span(), rhs)
                        .context("tried to check operands of a numeric assignment")?;
                }

                match lhs {
                    Type::TypeLit(..) => return Err(ErrorKind::WrongTypeForLhsOfNumericOperation { span }.into()),
                    ty if ty.is_bool() || ty.is_str() || ty.is_tpl() || ty.is_kwd(TsKeywordTypeKind::TsVoidKeyword) => {
                        return Err(ErrorKind::WrongTypeForLhsOfNumericOperation { span }.into());
                    }
                    _ => {}
                }

                match rhs {
                    Type::TypeLit(..) => return Err(ErrorKind::WrongTypeForRhsOfNumericOperation { span, ty: box rhs.clone() }.into()),
                    ty if ty.is_bool() || ty.is_str() || ty.is_tpl() || ty.is_kwd(TsKeywordTypeKind::TsVoidKeyword) => {
                        return Err(ErrorKind::WrongTypeForRhsOfNumericOperation { span, ty: box rhs.clone() }.into())
                    }
                    _ => {}
                }

                let r_castable = self.can_be_casted_to_number_in_rhs(rhs.span(), rhs);
                if r_castable {
                    if l.is_num() {
                        return Ok(());
                    }

                    if let Type::Enum(l) = lhs {
                        //
                        if !l.has_str {
                            return Ok(());
                        }
                    }
                }
            }

            _ => {}
        }

        // Trivial
        if lhs.is_any() || rhs.is_any() {
            return Ok(());
        }

        // Addition to a string converts rhs into string.
        if op == op!("+=") {
            if lhs.is_str() || lhs.is_tpl() {
                return Ok(());
            }
        }

        if lhs.is_num() || lhs.is_enum_variant() {
            // TODO(kdy1): Check if actual value is number.

            if rhs.is_num() {
                return Ok(());
            }

            if rhs.is_enum_variant() {
                // TODO(kdy1): Check if actual value is number.
                return Ok(());
            }

            if rhs.is_kwd(TsKeywordTypeKind::TsUndefinedKeyword)
                || rhs.is_kwd(TsKeywordTypeKind::TsNullKeyword)
                || rhs.is_kwd(TsKeywordTypeKind::TsVoidKeyword)
            {
                if skip_check_null_or_undefined_of_rhs {
                    return Ok(());
                }
                return Err(ErrorKind::AssignOpCannotBeApplied { span, op }.into());
            }
        }

        if let Type::TypeLit(lhs) = lhs {
            if lhs.members.is_empty() {
                if rhs.is_str() {
                    return Ok(());
                }
            }
        }

        match op {
            op!("&&=") | op!("||=") => {
                if l.type_eq(&r) {
                    return Ok(());
                }
            }
            op!("??=") => {
                if rhs.is_bool() {
                    return Ok(());
                }
            }
            _ => {}
        }

        if let op!("&&=") = op {
            if rhs.is_bool() {
                return Ok(());
            }

            if self.can_be_casted_to_number_in_rhs(span, &l) && self.can_be_casted_to_number_in_rhs(span, &r) {
                return Ok(());
            }
        }

        match op {
            op!("+=") => {
                if rhs.is_str() {
                    if l.is_bool() || l.is_num() || l.is_enum_variant() || l.is_type_lit() || l.is_kwd(TsKeywordTypeKind::TsVoidKeyword) {
                        return Err(ErrorKind::InvalidOpAssign {
                            span,
                            op,
                            lhs: box l.into_owned().clone(),
                            rhs: box r.into_owned().clone(),
                        }
                        .into());
                    }

                    return Ok(());
                }
            }

            op!("??=") | op!("||=") | op!("&&=") => {
                return self
                    .assign_with_opts(
                        &mut Default::default(),
                        lhs,
                        rhs,
                        AssignOpts {
                            span,
                            ..Default::default()
                        },
                    )
                    .convert_err(|err| ErrorKind::InvalidOpAssign {
                        span,
                        op,
                        lhs: box l.into_owned().clone(),
                        rhs: box r.into_owned().clone(),
                    });
            }
            _ => {}
        }

        if skip_check_null_or_undefined_of_rhs {
            return Ok(());
        }

        Err(ErrorKind::AssignOpCannotBeApplied { span, op }.into())
    }

    /// Assign `right` to `left`. You can just use default for [AssignData].
    pub(crate) fn assign(&mut self, span: Span, data: &mut AssignData, left: &Type, right: &Type) -> VResult<()> {
        self.assign_with_opts(
            data,
            left,
            right,
            AssignOpts {
                span,
                ..Default::default()
            },
        )
    }

    /// Assign `right` to `left`. You can just use default for [AssignData].
    pub(crate) fn assign_with_opts(&mut self, data: &mut AssignData, left: &Type, right: &Type, opts: AssignOpts) -> VResult<()> {
        if self.config.is_builtin {
            return Ok(());
        }

        left.assert_valid();
        right.assert_valid();

        let _stack = stack::track(opts.span)?;

        // if cfg!(debug_assertions) && span.is_dummy() {
        //     print_backtrace();
        //     debug_assert!(!span.is_dummy());
        // }

        // self.verify_before_assign("lhs", left);
        // self.verify_before_assign("rhs", right);
        let res = self.assign_inner(data, left, right, opts);

        match res.as_ref().map_err(|e| &**e) {
            Err(ErrorKind::Errors { errors, .. }) if errors.is_empty() => return Ok(()),
            _ => {}
        }

        res.convert_err(|err| match err {
            ErrorKind::AssignFailed { .. }
            | ErrorKind::Errors { .. }
            | ErrorKind::Unimplemented { .. }
            | ErrorKind::TupleAssignError { .. }
            | ErrorKind::ObjectAssignFailed { .. } => err,
            _ => ErrorKind::AssignFailed {
                span: opts.span,
                left: box left.clone(),
                right: box right.clone(),
                right_ident: opts.right_ident_span,
                cause: vec![err.into()],
            },
        })
    }

    fn normalize_for_assign<'a>(&mut self, span: Span, ty: &'a Type, opts: AssignOpts) -> VResult<Cow<'a, Type>> {
        ty.assert_valid();

        let ty = ty.normalize();

        if let Type::Instance(Instance { ty, .. }) = ty {
            // Normalize further
            if ty.is_ref_type() {
                let normalized = self.normalize_for_assign(span, ty, opts)?;

                if normalized.is_keyword() {
                    return Ok(normalized);
                }
            }

            if ty.is_mapped() {
                let ty = self.normalize_for_assign(span, ty, opts)?;

                return Ok(ty);
            }
        }

        match ty {
            Type::EnumVariant(e @ EnumVariant { name: Some(..), .. }) => {
                if opts.ignore_enum_variant_name {
                    return Ok(Cow::Owned(Type::EnumVariant(EnumVariant { name: None, ..e.clone() })));
                }
            }
            Type::Conditional(..)
            | Type::IndexedAccessType(..)
            | Type::Alias(..)
            | Type::Instance(..)
            | Type::StringMapping(..)
            | Type::Enum(..)
            | Type::Import(..)
            | Type::Tuple(..)
            | Type::Union(..)
            | Type::Index(..)
            | Type::Tpl(..)
            | Type::Query(..) => {
                let ty = self
                    .normalize(
                        Some(span),
                        Cow::Borrowed(ty),
                        NormalizeTypeOpts {
                            merge_union_elements: true,
                            preserve_global_this: true,
                            in_type_or_type_param: true,
                            ..Default::default()
                        },
                    )?
                    .into_owned();

                return Ok(Cow::Owned(ty));
            }
            _ => {}
        }

        Ok(Cow::Borrowed(ty))
    }

    fn assign_inner(&mut self, data: &mut AssignData, left: &Type, right: &Type, opts: AssignOpts) -> VResult<()> {
        left.assert_valid();
        right.assert_valid();

        let l = dump_type_as_string(left);
        let r = dump_type_as_string(right);

        if data
            .dejavu
            .iter()
            .any(|(prev_l, prev_r)| prev_l.type_eq(left) && prev_r.type_eq(right))
        {
            if cfg!(debug_assertions) {
                info!("[assign/dejavu] {} = {}\n{:?} ", l, r, opts);
            }
            return Ok(());
        }
        let _stack = stack::track(opts.span)?;

        data.dejavu.push((left.clone(), right.clone()));

        let res = self.assign_without_wrapping(data, left, right, opts).with_context(|| {
            //
            let l = force_dump_type_as_string(left);
            let r = force_dump_type_as_string(right);

            if l == r && !l.contains("symbol") && format!("{:?}", left) == format!("{:?}", right) {
                unreachable!("Assignment of identical type failed\n{}\n{:?}", l, left);
            }

            let l_final = self.normalize_for_assign(opts.span, left, opts);
            let r_final = self.normalize_for_assign(opts.span, right, opts);

            let l_final = l_final.map(|v| force_dump_type_as_string(&v)).unwrap_or_default();
            let r_final = r_final.map(|v| force_dump_type_as_string(&v)).unwrap_or_default();

            format!(
                "\nlhs = {}\nrhs = {}\nlhs (normalized) = {}\nrhs (normalized) = {}",
                l, r, l_final, r_final
            )
        });

        let dejavu = data.dejavu.pop();
        debug_assert!(dejavu.is_some());

        debug!("[assign ({:?})] {} = {}\n{:?} ", res.is_ok(), l, r, opts);

        res
    }

    /// Assigns, but does not wrap error with [Error::AssignFailed].
    fn assign_without_wrapping(&mut self, data: &mut AssignData, to: &Type, rhs: &Type, opts: AssignOpts) -> VResult<()> {
        let span = opts.span;

        if !self.config.is_builtin && span.is_dummy() {
            unreachable!("cannot assign with dummy span")
        }

        let _tracing = if cfg!(debug_assertions) {
            let lhs = dump_type_as_string(to);
            let rhs = dump_type_as_string(rhs);

            Some(dev_span!("assign", lhs = &*lhs, rhs = &*rhs))
        } else {
            None
        };

        // It's valid to assign any to everything.
        if rhs.is_any() {
            return Ok(());
        }

        if opts.allow_unknown_type && rhs.is_unknown() {
            return Ok(());
        }

        if opts.allow_assignment_to_void && to.is_kwd(TsKeywordTypeKind::TsVoidKeyword) {
            return Ok(());
        }

        if to.type_eq(rhs) {
            return Ok(());
        }

        // debug_assert!(!span.is_dummy(), "\n\t{:?}\n<-\n\t{:?}", to, rhs);
        let mut to = self.normalize_for_assign(span, to, opts).context("tried to normalize lhs")?;
        to.freeze();
        let mut rhs = self.normalize_for_assign(span, rhs, opts).context("tried to normalize rhs")?;
        rhs.freeze();

        let to = to.normalize();
        let rhs = rhs.normalize();

        macro_rules! fail {
            () => {{
                return Err(ErrorKind::AssignFailed {
                    span,
                    left: box to.clone(),
                    right: box rhs.clone(),
                    right_ident: opts.right_ident_span,
                    cause: vec![],
                }
                .context(format!(
                    "LHS (final): {}\nRHS (final): {}",
                    force_dump_type_as_string(to),
                    force_dump_type_as_string(rhs)
                )));
            }};
        }

        macro_rules! handle_enum_in_rhs {
            ($e:expr) => {{
                let e = $e;

                // Allow
                //      let e: E = E.a
                //
                // and
                //
                //      let e1: E = E.a
                //      let e2: E = e1
                match *to {
                    Type::Enum(ref left_enum) => {
                        if left_enum.id.sym() == e.id.sym() {
                            return Ok(());
                        }
                        fail!()
                    }
                    _ => {}
                }

                if opts.do_not_convert_enum_to_string_nor_number {
                    fail!()
                }

                if !e.has_str && !e.has_num {
                    return self
                        .assign_inner(
                            data,
                            to,
                            &Type::Keyword(KeywordType {
                                span,
                                kind: TsKeywordTypeKind::TsNumberKeyword,
                                metadata: Default::default(),
                                tracker: Default::default(),
                            }),
                            opts,
                        )
                        .context("tried to assign enum as `number`");
                }

                if !e.has_num {
                    return self
                        .assign_inner(
                            data,
                            to,
                            &Type::Keyword(KeywordType {
                                span,
                                kind: TsKeywordTypeKind::TsStringKeyword,
                                metadata: Default::default(),
                                tracker: Default::default(),
                            }),
                            opts,
                        )
                        .context("tried to assign enum as `string`");
                }

                if !e.has_str {
                    return self
                        .assign_inner(
                            data,
                            to,
                            &Type::Keyword(KeywordType {
                                span,
                                kind: TsKeywordTypeKind::TsNumberKeyword,
                                metadata: Default::default(),
                                tracker: Default::default(),
                            }),
                            opts,
                        )
                        .context("tried to assign enum as `number`");
                }

                return self
                    .assign_inner(
                        data,
                        to,
                        &Type::new_union(
                            span,
                            vec![
                                Type::Keyword(KeywordType {
                                    span,
                                    kind: TsKeywordTypeKind::TsNumberKeyword,
                                    metadata: Default::default(),
                                    tracker: Default::default(),
                                }),
                                Type::Keyword(KeywordType {
                                    span,
                                    kind: TsKeywordTypeKind::TsStringKeyword,
                                    metadata: Default::default(),
                                    tracker: Default::default(),
                                }),
                            ],
                        ),
                        opts,
                    )
                    .context("tried to assign enum as `number | string`");
            }};
        }

        if to.type_eq(rhs) {
            return Ok(());
        }

        if to.is_global_this() || rhs.is_global_this() {
            return Err(ErrorKind::SimpleAssignFailed {
                span: opts.span,
                cause: None,
            }
            .context("global this"));
        }

        if let Some(res) = self.assign_to_builtin(data, to, rhs, opts) {
            return res;
        }

        if rhs.is_kwd(TsKeywordTypeKind::TsNeverKeyword) {
            return Ok(());
        }
        if opts.disallow_assignment_to_unknown && to.is_kwd(TsKeywordTypeKind::TsUnknownKeyword) {
            fail!()
        }

        if to.is_kwd(TsKeywordTypeKind::TsNeverKeyword) {
            match rhs.normalize() {
                Type::Param(TypeParam { constraint: Some(ty), .. }) if ty.is_never() => return Ok(()),
                Type::Intersection(Intersection { types, .. }) => {
                    let result_ty = self.normalize_intersection_types(span, types, Default::default())?;
                    if let Some(rhs) = result_ty {
                        if rhs.is_never() {
                            return Ok(());
                        }
                        fail!()
                    }
                }
                _ => fail!(),
            };
        }

        let opts = AssignOpts {
            disallow_assignment_to_unknown: false,
            ..opts
        };

        match (to, rhs) {
            (Type::Rest(lr), r) => {
                if r.is_unknown() {
                    return Err(ErrorKind::AssignFailed {
                        span,
                        left: box to.clone(),
                        right: box rhs.clone(),
                        right_ident: opts.right_ident_span,
                        cause: vec![],
                    }
                    .into());
                }

                if let Type::Array(la) = lr.ty.normalize() {
                    return self.assign_with_opts(data, &la.elem_type, r, opts);
                }
            }

            (l, Type::Rest(rr)) => {
                if let Type::Array(ra) = rr.ty.normalize() {
                    return self.assign_with_opts(data, l, &ra.elem_type, opts);
                }
            }

            (Type::Tuple(..) | Type::Array(..), Type::Function(..) | Type::Constructor(..)) => {
                fail!()
            }
            (Type::TypeLit(TypeLit { members, .. }), Type::TypeLit(..)) => {
                if members.is_empty() && !opts.for_overload {
                    return Ok(());
                }
            }
            _ => {}
        }

        if to.is_any() {
            return Ok(());
        }

        if opts.allow_assignment_of_param {
            if rhs.is_type_param() {
                return Ok(());
            }
        }

        if rhs.is_enum_type() {
            let rhs = self
                .normalize(
                    Some(span),
                    Cow::Borrowed(rhs),
                    NormalizeTypeOpts {
                        expand_enum_def: true,
                        ..Default::default()
                    },
                )?
                .freezed()
                .into_owned()
                .freezed();
            if self.assign_inner(data, to, &rhs, opts).is_ok() {
                return Ok(());
            }
        }

        match to {
            Type::Ref(Ref {
                type_name: RTsEntityName::Ident(RIdent {
                    sym: js_word!("Symbol"), ..
                }),
                ..
            }) => {
                if rhs.is_kwd(TsKeywordTypeKind::TsSymbolKeyword) {
                    return Ok(());
                }
            }

            // Str contains `kind`, and it's not handled properly by type_eq.
            Type::Lit(LitType { lit: RTsLit::Str(to), .. }) => match rhs {
                Type::Lit(LitType { lit: RTsLit::Str(rhs), .. }) => {
                    if to.value == rhs.value {
                        return Ok(());
                    } else {
                        fail!()
                    }
                }
                _ => {
                    if opts.for_castablity {
                        if rhs.is_kwd(TsKeywordTypeKind::TsStringKeyword) {
                            return Ok(());
                        }
                    }
                }
            },

            Type::Ref(left) => {
                if let Type::Ref(right) = rhs {
                    // We need this as type may recurse, and thus cannot be handled by expander.
                    if left.type_name.type_eq(&right.type_name) && left.type_args.type_eq(&right.type_args) {
                        return Ok(());
                    }
                }

                let new_lhs = self.expand_top_ref(span, Cow::Borrowed(to), Default::default())?.freezed();
                // self.replace(&mut new_lhs, &[(to, &Type::any(span))]);

                return self
                    .assign_inner(
                        data,
                        &new_lhs,
                        rhs,
                        AssignOpts {
                            allow_unknown_rhs: if opts.allow_unknown_rhs_if_expanded {
                                Some(true)
                            } else {
                                opts.allow_unknown_rhs
                            },
                            allow_unknown_rhs_if_expanded: false,
                            ..opts
                        },
                    )
                    .context("tried to assign a type created from a reference");
            }

            _ => {}
        }

        if to.is_mapped() {
            let to = self
                .normalize(
                    Some(opts.span),
                    Cow::Borrowed(rhs),
                    NormalizeTypeOpts {
                        preserve_typeof: true,
                        preserve_global_this: true,
                        preserve_intersection: true,
                        preserve_union: true,
                        ..Default::default()
                    },
                )?
                .freezed();

            if let Ok(()) = self.assign_with_opts(data, &to, rhs, opts) {
                return Ok(());
            }
        }

        if rhs.is_mapped() {
            let rhs = self
                .normalize(
                    Some(opts.span),
                    Cow::Borrowed(rhs),
                    NormalizeTypeOpts {
                        preserve_typeof: true,
                        preserve_global_this: true,
                        preserve_intersection: true,
                        preserve_union: true,
                        ..Default::default()
                    },
                )?
                .freezed();

            if let Ok(()) = self.assign_with_opts(data, to, &rhs, opts) {
                return Ok(());
            }
        }

        if to.is_str_lit() || to.is_num_lit() || to.is_bool_lit() {
            if rhs.is_type_lit() {
                fail!()
            }
        }

        // never -> never is ok, but T -> never is not.
        if to.is_kwd(TsKeywordTypeKind::TsNeverKeyword) {
            if !rhs.is_kwd(TsKeywordTypeKind::TsNeverKeyword) {
                fail!()
            }
        }

        // Allow v = null and v = undefined if strict null check is false
        if !self.rule().strict_null_checks {
            match rhs {
                Type::Keyword(KeywordType {
                    kind: TsKeywordTypeKind::TsNullKeyword,
                    ..
                })
                | Type::Keyword(KeywordType {
                    kind: TsKeywordTypeKind::TsUndefinedKeyword,
                    ..
                }) => return Ok(()),
                _ => {}
            }
        }

        if match rhs.normalize_instance() {
            Type::Lit(..) => true,
            Type::Interface(i) => matches!(&**i.name.sym(), "Boolean" | "String" | "Number" | "BigInt"),
            Type::EnumVariant(..) => true,
            _ => false,
        } {
            // Handle special cases.
            // Assigning boolean to Boolean is ok, but assigning Boolean to boolean is an
            // error.
            let special_cases = &[
                (TsKeywordTypeKind::TsBooleanKeyword, "Boolean"),
                (TsKeywordTypeKind::TsStringKeyword, "String"),
                (TsKeywordTypeKind::TsNumberKeyword, "Number"),
                (TsKeywordTypeKind::TsBigIntKeyword, "BigInt"),
            ];

            let rhs = rhs.clone().generalize_lit();

            for (kwd, interface) in special_cases {
                match to {
                    Type::Keyword(k) if k.kind == *kwd => match rhs {
                        Type::Instance(Instance {
                            ty: box Type::Interface(ref i),
                            ..
                        })
                        | Type::Interface(ref i) => {
                            if i.name.as_str() == *interface {
                                return Err(ErrorKind::AssignedWrapperToPrimitive { span }.into());
                            }
                        }
                        _ => {}
                    },
                    Type::Instance(Instance {
                        ty: box Type::Interface(ref i),
                        ..
                    })
                    | Type::Interface(ref i)
                        if i.name.as_str() == *interface =>
                    {
                        match rhs {
                            Type::Keyword(ref k) if k.kind == *kwd => return Ok(()),
                            Type::EnumVariant(ref e) => {
                                let e = e.def.normalize();
                                if (e.has_num && !e.has_str && *interface == "Number")
                                    || (e.has_str && !e.has_num && *interface == "String")
                                {
                                    return Ok(());
                                }
                            }
                            _ => {}
                        }
                    }
                    _ => {}
                }
            }
        }

        if let (Type::Conditional(lc), Type::Conditional(rc)) = (to, rhs) {
            if lc.extends_type.type_eq(&rc.extends_type) {
                if let Ok(..) = self.assign_with_opts(data, &rc.check_type, &lc.check_type, opts) {
                    self.assign_with_opts(data, &lc.true_type, &rc.true_type, opts)
                        .context("tried to assign the true type of a conditional type to it of similar conditional type")?;

                    self.assign_with_opts(data, &lc.false_type, &rc.false_type, opts)
                        .context("tried to assign the true type of a conditional type to it of similar conditional type")?;

                    return Ok(());
                }
            }

            if lc.extends_type.type_eq(&rc.extends_type) {
                let variance = self.variance(lc)?;

                match variance {
                    Variance::Covariant => {
                        return self
                            .assign_with_opts(data, &lc.check_type, &rc.check_type, opts)
                            .context("tried assignment of covariant types")
                    }
                    Variance::Contravariant => {
                        return self
                            .assign_with_opts(data, &rc.check_type, &lc.check_type, opts)
                            .context("tried assignment of contravariant types")
                    }
                    Variance::Invariant => {
                        fail!()
                    }
                }
            }
        }

        match (to, rhs) {
            (_, Type::Conditional(rc)) => {
                let new_true_ty = self.overwrite_conditional(span, rc);

                self.assign_with_opts(data, to, &new_true_ty, opts)
                    .context("tried to assign the true type of a conditional type to lhs")?;
                self.assign_with_opts(data, to, &rc.false_type, opts)
                    .context("tried to assign the false type of a conditional type to lhs")?;

                return Ok(());
            }

            (Type::Conditional(lc), _) => {
                self.assign_with_opts(data, &lc.true_type, rhs, opts)
                    .context("tried to assign to the true type")?;
                self.assign_with_opts(data, &lc.false_type, rhs, opts)
                    .context("tried to assign to the false type")?;

                return Ok(());
            }

            _ => {}
        }

        match to {
            Type::Keyword(KeywordType {
                kind: TsKeywordTypeKind::TsUndefinedKeyword,
                ..
            }) => fail!(),

            Type::Keyword(KeywordType {
                kind: TsKeywordTypeKind::TsVoidKeyword,
                ..
            }) => {
                if rhs.is_kwd(TsKeywordTypeKind::TsUndefinedKeyword) || rhs.is_any() {
                    return Ok(());
                }
                if let Type::Query(QueryType { expr, .. }) = rhs.clone() {
                    if let QueryExpr::TsEntityName(RTsEntityName::Ident(RIdent { sym, .. })) = *expr {
                        if sym == js_word!("undefined") {
                            return Ok(());
                        }
                    }
                }
                fail!()
            }
            // Anything is assignable to unknown
            Type::Keyword(KeywordType {
                kind: TsKeywordTypeKind::TsUnknownKeyword,
                ..
            }) => return Ok(()),

            // Everything is assignable to Object
            Type::Interface(ref i) if i.name.as_str() == "Object" => return Ok(()),

            Type::Module(to) => {
                // TODO(kdy1): Use unique id for module type.
                if let Type::Module(rhs) = rhs {
                    if to.name.eq_ignore_span(&rhs.name) {
                        return Ok(());
                    }
                }
                // TODO(kdy1): Validate this correctly
                //
                // Note that this task is a 100% parity issue, as no one do this in real world.
                return Ok(());
            }
            Type::Enum(..) => fail!(),

            Type::EnumVariant(EnumVariant { name: None, def, .. }) => match rhs.normalize() {
                Type::Lit(LitType {
                    lit: RTsLit::Number(r_num),
                    ..
                }) => {
                    if opts.do_not_convert_enum_to_string_nor_number {
                        fail!()
                    }

                    for m in def.members.iter() {
                        if let Type::Lit(LitType {
                            lit: RTsLit::Number(l_num),
                            ..
                        }) = m.val.normalize()
                        {
                            if l_num.value == r_num.value {
                                return Ok(());
                            }
                        }
                    }

                    fail!()
                }
                Type::Keyword(KeywordType {
                    kind: TsKeywordTypeKind::TsNumberKeyword,
                    ..
                }) => {
                    if opts.do_not_convert_enum_to_string_nor_number {
                        fail!()
                    }

                    // TODO: Check for values of member
                    if def.has_num {
                        return Ok(());
                    }
                    fail!()
                }

                Type::Lit(LitType {
                    lit: RTsLit::Str(r_str), ..
                }) => {
                    if opts.do_not_convert_enum_to_string_nor_number {
                        fail!()
                    }

                    for m in def.members.iter() {
                        if let Type::Lit(LitType {
                            lit: RTsLit::Str(l_str), ..
                        }) = m.val.normalize()
                        {
                            if l_str.value == r_str.value {
                                return Ok(());
                            }
                        }
                    }

                    fail!()
                }
                Type::Keyword(KeywordType {
                    kind: TsKeywordTypeKind::TsStringKeyword,
                    ..
                }) => {
                    if opts.do_not_convert_enum_to_string_nor_number {
                        fail!()
                    }

                    // TODO: Check for values of member
                    if def.has_str {
                        return Ok(());
                    }

                    fail!()
                }

                Type::EnumVariant(rhs) => {
                    if rhs.def.id == def.id {
                        return Ok(());
                    }
                    fail!()
                }

                Type::Lit(..)
                | Type::TypeLit(..)
                | Type::Keyword(KeywordType {
                    kind: TsKeywordTypeKind::TsVoidKeyword,
                    ..
                })
                | Type::Keyword(KeywordType {
                    kind: TsKeywordTypeKind::TsBooleanKeyword,
                    ..
                }) => fail!(),

                _ => {}
            },
            Type::EnumVariant(ref e @ EnumVariant { name: Some(name), .. }) => {
                // Single-variant enums seem to be treated like a number.
                // but if enum isn't has num, not assignable
                //
                // See typeArgumentInferenceWithObjectLiteral.ts
                match rhs.normalize() {
                    Type::EnumVariant(en) => {
                        if !&en.def.id.type_eq(&e.def.id) {
                            fail!()
                        }

                        if let Some(en_name) = &en.name {
                            if en_name.type_eq(name) {
                                return Ok(());
                            }
                        }

                        fail!()
                    }
                    Type::Lit(LitType {
                        lit: RTsLit::Number(r_num),
                        ..
                    }) => {
                        if opts.do_not_convert_enum_to_string_nor_number {
                            fail!()
                        }

                        if let Some(v) = e.def.members.iter().find(|m| match m.id {
                            RTsEnumMemberId::Ident(RIdent { ref sym, .. }) | RTsEnumMemberId::Str(RStr { value: ref sym, .. }) => {
                                sym == name
                            }
                        }) {
                            if let Type::Lit(LitType {
                                lit: RTsLit::Number(l_num),
                                ..
                            }) = v.val.normalize()
                            {
                                if l_num.value == r_num.value {
                                    return Ok(());
                                }
                            }
                        }
                        fail!()
                    }
                    Type::Lit(LitType {
                        lit: RTsLit::Str(r_str), ..
                    }) => {
                        if opts.do_not_convert_enum_to_string_nor_number {
                            fail!()
                        }

                        if let Some(v) = e.def.members.iter().find(|m| match m.id {
                            RTsEnumMemberId::Ident(RIdent { ref sym, .. }) | RTsEnumMemberId::Str(RStr { value: ref sym, .. }) => {
                                sym == name
                            }
                        }) {
                            if let Type::Lit(LitType {
                                lit: RTsLit::Str(l_str), ..
                            }) = &*v.val
                            {
                                if l_str.value == r_str.value {
                                    return Ok(());
                                }
                            }
                        }

                        fail!()
                    }
                    _ => fail!(),
                }
            }

            Type::Intersection(ref li) => {
                let mut errors = vec![];

                // This is required to handle intersections of function-like types.
                if let Some(l_type_lit) = self.convert_type_to_type_lit(span, Cow::Borrowed(to), Default::default())? {
                    if self
                        .assign_to_type_elements(
                            data,
                            li.span,
                            &l_type_lit.members,
                            rhs,
                            l_type_lit.metadata,
                            AssignOpts {
                                is_assigning_to_class_members: true,
                                allow_unknown_rhs: Some(false),
                                ..opts
                            },
                        )
                        .is_ok()
                    {
                        return Ok(());
                    }
                }

                for ty in &li.types {
                    match self
                        .assign_with_opts(
                            data,
                            ty,
                            rhs,
                            AssignOpts {
                                allow_unknown_rhs: Some(true),
                                allow_assignment_to_param_constraint: true,
                                ..opts
                            },
                        )
                        .context("tried to assign to an element of an intersection type")
                        .convert_err(|err| ErrorKind::SimpleAssignFailed {
                            span: err.span(),
                            cause: Some(box err.into()),
                        }) {
                        Ok(..) => {}
                        Err(err) => errors.push(err),
                    }
                }

                let left_contains_object = li.types.iter().any(|ty| ty.is_kwd(TsKeywordTypeKind::TsObjectKeyword));
                let rhs_requires_unknown_property_check = !matches!(rhs.normalize(), Type::Keyword(..));

                if !left_contains_object && rhs_requires_unknown_property_check && !opts.allow_unknown_rhs.unwrap_or_default() {
                    let lhs = self.convert_type_to_type_lit(span, Cow::Borrowed(to), Default::default())?;

                    if let Some(lhs) = lhs {
                        self.assign_to_type_elements(data, lhs.span, &lhs.members, rhs, lhs.metadata, AssignOpts { ..opts })
                            .with_context(|| {
                                format!(
                                    "tried to check if unknown rhs exists while assigning to an intersection type:\nLHS: {}",
                                    dump_type_as_string(&Type::TypeLit(lhs.into_owned()))
                                )
                            })
                            .convert_err(|err| ErrorKind::SimpleAssignFailed {
                                span: err.span(),
                                cause: Some(box err.into()),
                            })?;

                        errors.retain(|err| !matches!(&**err, ErrorKind::UnknownPropertyInObjectLiteralAssignment { .. }));
                    }
                }

                if errors.is_empty() {
                    return Ok(());
                }

                return Err(ErrorKind::Errors { span, errors }.into());
            }

            Type::Class(l) => match rhs {
                Type::Interface(..)
                | Type::Ref(..)
                | Type::TypeLit(..)
                | Type::Lit(..)
                | Type::Keyword(..)
                | Type::Class(..)
                | Type::Predicate(..) => {
                    return self
                        .assign_to_class(data, l, rhs, opts)
                        .context("tried to assign a type to an instance of a class")
                }
                Type::Array(..) | Type::ClassDef(..) => {
                    fail!()
                }
                _ => {}
            },
            Type::ClassDef(l) => {
                return self
                    .assign_to_class_def(data, l, rhs, opts)
                    .context("tried to assign a type to a class definition")
            }

            Type::Lit(ref lhs) => match rhs.normalize() {
                Type::Lit(rhs) => {
                    if is_lit_eq_ignore_span(lhs, rhs) {
                        return Ok(());
                    } else {
                        return Err(ErrorKind::AssignFailed {
                            span: opts.left_ident_span.unwrap_or(span),
                            left: box to.clone(),
                            right_ident: opts.right_ident_span,
                            right: box rhs.clone().into(),
                            cause: vec![],
                        }
                        .into());
                    }
                }
                Type::Ref(..) | Type::Query(..) | Type::Param(..) => {
                    // We should expand ref. We expand it with the match
                    // expression below.
                }
                Type::EnumVariant(e) => {
                    if opts.do_not_convert_enum_to_string_nor_number {
                        fail!()
                    }

                    // Single-variant enums seem to be treated like a number.
                    //
                    // See typeArgumentInferenceWithObjectLiteral.ts

                    let rhs = self.expand_enum_variant(rhs.clone())?;

                    return self
                        .assign_inner(data, to, &rhs, opts)
                        .context("tried to assign an enum variant to a literal");
                }
                _ => {
                    if let RTsLit::Str(lhs) = &lhs.lit {
                        if let Type::Tpl(rhs) = rhs {
                            if rhs.types.is_empty() {
                                if *lhs.value == *rhs.quasis[0].value {
                                    return Ok(());
                                }
                            }
                        }
                    }
                    fail!()
                }
            },

            Type::Readonly(Readonly { ty, .. }) => {
                return self
                    .assign_with_opts(data, ty, rhs, opts)
                    .context("tried to assign a type to an operand of readonly type")
            }

            _ => {}
        }

        if opts.allow_assignment_of_void.unwrap_or_default() {
            if rhs.is_kwd(TsKeywordTypeKind::TsVoidKeyword) {
                return Ok(());
            }
        }

        match rhs {
            Type::Ref(..) => {
                let mut new_rhs = self.expand_top_ref(span, Cow::Borrowed(rhs), Default::default())?;
                new_rhs.freeze();
                // self.replace(&mut new_rhs, &[(rhs, &Type::any(span))]);
                return self
                    .assign_inner(data, to, &new_rhs, opts)
                    .context("tried to assign a type expanded from a reference to another type");
            }

            Type::Infer(..) => fail!(),

            // When strict null check is disabled, we can assign null / undefined to many things.
            Type::Keyword(KeywordType {
                kind: TsKeywordTypeKind::TsUndefinedKeyword,
                ..
            })
            | Type::Keyword(KeywordType {
                kind: TsKeywordTypeKind::TsNullKeyword,
                ..
            }) => {
                // Deny assigning null to class. (not instance)

                match *to.normalize() {
                    Type::Class(..) | Type::Function(..) => fail!(),
                    _ => {}
                }

                if !self.rule().strict_null_checks {
                    return Ok(());
                }
            }

            Type::Intersection(Intersection { types, .. }) => {
                if !opts.do_not_normalize_intersection_on_rhs {
                    // Filter out `never` types
                    if let Some(new) = self.normalize_intersection_types(span, types, NormalizeTypeOpts { ..Default::default() })? {
                        return self
                            .assign_inner(
                                &mut AssignData::default(),
                                to,
                                &new,
                                AssignOpts {
                                    do_not_normalize_intersection_on_rhs: true,
                                    ..opts
                                },
                            )
                            .context("tried to assign a normalized intersection type to another type");
                    }
                }
                if let Some(new) = self.normalize_intersection_types(span, types, NormalizeTypeOpts { ..Default::default() })? {
                    if new.is_never() && to.is_never() {
                        return Ok(());
                    }
                }

                let results = types
                    .iter()
                    .map(|rhs| {
                        self.assign_inner(
                            data,
                            to,
                            rhs,
                            AssignOpts {
                                allow_assignment_to_param_constraint: true,
                                ..opts
                            },
                        )
                        .context("tried to assign an element of an intersection type to another type")
                    })
                    .collect::<Vec<_>>();
                if results.iter().any(Result::is_ok) {
                    return Ok(());
                }

                if let Ok(Some(rhs)) = self.convert_type_to_type_lit(opts.span, Cow::Borrowed(rhs), Default::default()) {
                    if self.assign_inner(data, to, &Type::TypeLit(rhs.into_owned()), opts).is_ok() {
                        return Ok(());
                    }
                }

                let use_single_error = types.iter().all(|ty| ty.is_interface());
                let errors = results.into_iter().map(Result::unwrap_err).collect();

                if use_single_error {
                    return Err(ErrorKind::AssignFailed {
                        span,
                        left: box to.clone(),
                        right_ident: None,
                        right: box rhs.clone(),
                        cause: errors,
                    }
                    .into());
                }

                return Err(ErrorKind::Errors { span, errors }.into());
            }

            Type::Union(r) => {
                if self.should_use_special_union_assignment(span, rhs)? {
                    // TODO(kdy1): We should assign rhs as full.
                    //
                    //
                    // lhs = (undefined | {
                    //     (x: number) : number;
                    //     (s: string) : string;
                    // });
                    // rhs = (undefined | (x: number) => number | (s: string) => string);
                    //
                    // The assignment above is valid, but it only works if we create a type literal
                    // with two call signatures using two functions in rhs.

                    r.types
                        .iter()
                        .try_for_each(|rhs| {
                            if cfg!(debug_assertions) {
                                // Assertion for deep clones.
                                let _ = to.clone();
                                let _ = rhs.clone();
                            }

                            self.assign_with_opts(
                                data,
                                to,
                                rhs,
                                AssignOpts {
                                    allow_unknown_rhs: Some(true),
                                    report_assign_failure_for_missing_properties: opts
                                        .report_assign_failure_for_missing_properties
                                        .or(Some(true)),
                                    ..opts
                                },
                            )
                        })
                        .context("tried to assign an union type to another one")?;

                    return Ok(());
                }

                let errors = r
                    .types
                    .iter()
                    .filter_map(|rhs| match self.assign_with_opts(data, to, rhs, opts) {
                        Ok(()) => None,
                        Err(err) => Some(err),
                    })
                    .collect::<Vec<_>>();
                if errors.is_empty() {
                    return Ok(());
                }
                return Err(ErrorKind::Errors { span, errors }.context("tried to assign a union to other type"));
            }

            Type::Keyword(KeywordType {
                kind: TsKeywordTypeKind::TsAnyKeyword,
                ..
            }) => return Ok(()),

            Type::Param(TypeParam {
                ref name, ref constraint, ..
            }) => {
                if let Type::Param(TypeParam { name: ref l_name, .. }) = to {
                    if opts.allow_assignment_to_param {
                        if let Some(ref c) = *constraint {
                            return self
                                .assign_inner(data, to, c, AssignOpts { ..opts })
                                .context("tried to assign a type parameter to another type parameter");
                        }

                        return Ok(());
                    }

                    if name == l_name {
                        return Ok(());
                    }
                    match constraint.as_deref() {
                        Some(constraint) if constraint.is_type_param() => {}
                        _ => {
                            fail!()
                        }
                    }
                }
            }

            Type::Predicate(..) => {
                if let Type::Keyword(KeywordType {
                    kind: TsKeywordTypeKind::TsBooleanKeyword,
                    ..
                }) = rhs
                {
                    return Ok(());
                }
            }

            _ => {}
        }

        match to {
            Type::Mapped(to) => return self.assign_to_mapped(data, to, rhs, opts),
            Type::Param(TypeParam {
                constraint: Some(ref c), ..
            }) => {
                if opts.allow_assignment_to_param_constraint {
                    return self.assign_inner(
                        data,
                        c,
                        rhs,
                        AssignOpts {
                            allow_assignment_to_param: true,
                            ..opts
                        },
                    );
                }

                if let Some(true) = c.as_union_type().map(|ty| ty.types.iter().any(|ty| ty.type_eq(rhs))) {
                    return Ok(());
                }

                if !rhs.is_type_param() {
                    fail!()
                }
            }

            Type::Param(..) => {
                // We handled equality above.
                //
                // This is optional so we can change behavior while selecting method to call.
                // While selecting method, we may need to assign to a type parameter.

                if opts.allow_assignment_to_param {
                    return Ok(());
                } else {
                    match rhs {
                        Type::Mapped(m) => {
                            // Try assign mapped type takes `T` as an arg to type param `T` has no
                            // constraint.
                            // Error will occur if mapped type including `?` or `+?`
                            // modifiers.
                            //
                            // ex) type Partial<T> = { [P in keyof T]?: T[P] | undefined; }
                            // ```ts
                            // function error<T>(x: T, y: Partial<T>) {
                            //     x = y; // error TS2322
                            // }
                            // ```
                            let add_optional = matches!(m.optional, Some(True) | Some(Plus));
                            if let Some(constraint @ Type::Index(Index { ty, .. })) =
                                m.type_param.constraint.as_deref().map(|ty| ty.normalize())
                            {
                                if to.type_eq(ty) && !add_optional {
                                    return Ok(());
                                } else {
                                    fail!()
                                }
                            }
                        }
                        Type::Param(..) => {}
                        _ => fail!(),
                    };
                }
            }

            Type::Array(Array {
                elem_type: ref lhs_elem_type,
                ..
            }) => match rhs {
                Type::Array(Array {
                    elem_type: ref rhs_elem_type,
                    ..
                }) => {
                    return self.assign_inner(data, lhs_elem_type, rhs_elem_type, opts);
                }

                Type::Tuple(Tuple { ref elems, .. }) => {
                    let mut errors = vec![];
                    for el in elems {
                        errors.extend(self.assign_inner(data, lhs_elem_type, &el.ty, opts).err());
                    }
                    if !errors.is_empty() {
                        Err(ErrorKind::Errors { span, errors })?;
                    }

                    return Ok(());
                }

                Type::Param(param) => {
                    let ty = &param.constraint;

                    if let Some(ty) = ty {
                        let ty = ty.normalize();

                        if let Type::Array(Array {
                            elem_type: ref rhs_elem_type,
                            ..
                        }) = ty
                        {
                            return self.assign_inner(data, lhs_elem_type, rhs_elem_type, opts);
                        } else {
                            return self.assign_without_wrapping(data, to, ty, opts);
                        }
                    }
                }

                _ => {
                    if let Some(res) = self.try_assign_using_parent(data, to, rhs, opts) {
                        return res;
                    }

                    let r = self.convert_type_to_type_lit(span, Cow::Borrowed(rhs), Default::default())?;
                    if let Some(r) = r {
                        for m in &r.members {
                            if let TypeElement::Index(m) = m {
                                if let Type::Keyword(KeywordType {
                                    span,
                                    kind: TsKeywordTypeKind::TsNumberKeyword,
                                    ..
                                }) = m.params[0].ty.normalize()
                                {
                                    if let Some(type_ann) = &m.type_ann {
                                        return self.assign_with_opts(data, lhs_elem_type, type_ann, opts);
                                    }
                                }
                            }
                        }
                    }

                    // Try to assign by converting rhs to an iterable.
                    if opts.allow_iterable_on_rhs {
                        let res: VResult<_> = try {
                            let r = self
                                .get_iterator(span, Cow::Borrowed(rhs), Default::default())
                                .context("tried to convert a type to an iterator to assign to a tuple")?
                                .freezed();
                            //
                            let rhs_el = self
                                .get_iterator_element_type(span, r, false, Default::default())
                                .context("tried to get the element type of an iterator assignment")?
                                .freezed();

                            self.assign_with_opts(
                                data,
                                lhs_elem_type,
                                &rhs_el,
                                AssignOpts {
                                    allow_iterable_on_rhs: false,
                                    ..opts
                                },
                            )?;
                        };

                        if res.is_ok() {
                            return Ok(());
                        }
                    }

                    if opts.treat_array_as_interfaces {
                        if rhs.is_type_lit() {
                            return Ok(());
                        }
                    }

                    fail!()
                }
            },

            // let a: string | number = 'string';
            Type::Union(lu) => {
                // true | false = boolean
                if rhs.is_kwd(TsKeywordTypeKind::TsBooleanKeyword) {
                    if lu.types.iter().any(|ty| {
                        matches!(
                            ty.normalize(),
                            Type::Lit(LitType {
                                lit: RTsLit::Bool(RBool { value: true, .. }),
                                ..
                            })
                        )
                    }) && lu.types.iter().any(|ty| {
                        matches!(
                            ty.normalize(),
                            Type::Lit(LitType {
                                lit: RTsLit::Bool(RBool { value: false, .. }),
                                ..
                            })
                        )
                    }) {
                        return Ok(());
                    }
                }

                if rhs.is_unknown() {
                    //  In TypeScript, type `{}` means "any non-nullish value".
                    //  So, `unknown` is assignable to `{} | null | undefined`.

                    let empty_member: Vec<TypeElement> = Vec::new();
                    if lu.types.iter().any(|ty| {
                        matches!(
                            ty.normalize(),
                            Type::Keyword(KeywordType {
                                kind: TsKeywordTypeKind::TsNullKeyword,
                                ..
                            })
                        )
                    }) && lu.types.iter().any(|ty| {
                        matches!(
                            ty.normalize(),
                            Type::Keyword(KeywordType {
                                kind: TsKeywordTypeKind::TsUndefinedKeyword,
                                ..
                            })
                        )
                    }) && lu
                        .types
                        .iter()
                        .any(|ty| matches!(ty.normalize(), Type::TypeLit(TypeLit { members: empty_member, .. })))
                    {
                        return Ok(());
                    }
                }

                if let Type::Tuple(..)
                | Type::TypeLit(..)
                | Type::Union(..)
                | Type::Alias(..)
                | Type::Interface(..)
                | Type::Intersection(..) = rhs
                {
                    if let Some(res) = self.assign_to_union(data, to, rhs, opts) {
                        return res.context("tried to assign using `assign_to_union`");
                    }
                }

                if let Type::StringMapping(StringMapping { type_args, .. }) = rhs {
                    if let Some(res) = type_args.params.iter().find_map(|param| {
                        if let Type::Param(TypeParam {
                            constraint: Some(constraint),
                            ..
                        }) = param
                        {
                            if let Type::Union(..) = constraint.normalize() {
                                if let Some(res) = self.assign_to_union(data, to, constraint, opts) {
                                    return Some(res.context("tried to assign intrinsic union using `assign_to_union`"));
                                }
                            }
                        }
                        None
                    }) {
                        return res;
                    }
                }

                let results = lu
                    .types
                    .iter()
                    .map(|to| {
                        self.assign_with_opts(
                            data,
                            to,
                            rhs,
                            AssignOpts {
                                allow_unknown_rhs_if_expanded: true,
                                ..opts
                            },
                        )
                        .context("tried to assign a type to a union")
                    })
                    .collect::<Vec<_>>();
                if results.iter().any(Result::is_ok) {
                    return Ok(());
                }
                let normalized = lu.types.iter().any(|ty| match ty.normalize() {
                    Type::TypeLit(ty) => ty.metadata.normalized,
                    _ => false,
                });
                let errors = results.into_iter().map(Result::unwrap_err).collect();
                let should_use_single_error = normalized
                    || lu.types.iter().all(|ty| {
                        ty.is_lit()
                            || ty.is_type_lit()
                            || ty.is_keyword()
                            || ty.is_enum_variant()
                            || ty.is_ref_type()
                            || ty.is_query()
                            || ty.is_fn_type()
                            || ty.is_tpl()
                            || ty.is_intersection()
                            || ty.is_type_param()
                            || ty.is_class()
                            || ty.is_class_def()
                    });

                if should_use_single_error {
                    return Err(ErrorKind::AssignFailed {
                        span,
                        cause: errors,
                        left: box to.clone(),
                        right: box rhs.clone(),
                        right_ident: opts.right_ident_span,
                    }
                    .into());
                } else {
                    return Err(ErrorKind::Errors { span, errors }.context("tried to assign a type to a union type"));
                }
            }

            Type::Intersection(Intersection { ref types, .. }) => {
                let vs = types.iter().map(|to| self.assign_inner(data, to, rhs, opts)).collect::<Vec<_>>();

                // TODO(kdy1): Multiple error
                for v in vs {
                    if let Err(error) = v {
                        return Err(ErrorKind::IntersectionError { span, error: box error }.into());
                    }
                }

                return Ok(());
            }

            // Handle same keyword type.
            Type::Keyword(KeywordType { kind, .. }) => {
                match *rhs {
                    Type::Keyword(KeywordType { kind: rhs_kind, .. }) => {
                        if rhs_kind == *kind {
                            return Ok(());
                        }

                        if rhs_kind == TsKeywordTypeKind::TsUndefinedKeyword && *kind == TsKeywordTypeKind::TsVoidKeyword {
                            return Ok(());
                        }

                        if *kind == TsKeywordTypeKind::TsUndefinedKeyword && rhs_kind == TsKeywordTypeKind::TsVoidKeyword {
                            return Ok(());
                        }

                        fail!()
                    }

                    Type::Symbol(..) => match kind {
                        TsKeywordTypeKind::TsSymbolKeyword => return Ok(()),
                        _ => {
                            fail!()
                        }
                    },

                    Type::Array(..) | Type::Tuple(..) | Type::Class(..) | Type::ClassDef(..) => {
                        if *kind != TsKeywordTypeKind::TsObjectKeyword {
                            fail!()
                        }
                    }

                    _ => {}
                }

                match kind {
                    TsKeywordTypeKind::TsStringKeyword => match rhs.normalize() {
                        Type::Lit(LitType { lit: RTsLit::Str(..), .. }) => return Ok(()),
                        Type::EnumVariant(EnumVariant { name: None, def, .. }) => {
                            if def.has_str && !def.has_num {
                                return Ok(());
                            }
                        }
                        Type::EnumVariant(EnumVariant { ref name, .. }) => {
                            // Allow assigning enum with numeric values to
                            // string.
                            if let Ok(Type::Lit(LitType { lit: RTsLit::Str(..), .. })) = self.expand_enum_variant(rhs.clone()) {
                                return Ok(());
                            }

                            fail!()
                        }
                        _ => {}
                    },

                    TsKeywordTypeKind::TsNumberKeyword => match *rhs {
                        Type::Lit(LitType {
                            lit: RTsLit::Number(..), ..
                        }) => return Ok(()),
                        Type::EnumVariant(EnumVariant { name: None, ref def, .. }) => {
                            if opts.do_not_convert_enum_to_string_nor_number {
                                fail!()
                            }

                            if def.has_num && !def.has_str {
                                return Ok(());
                            }
                        }
                        Type::EnumVariant(EnumVariant { ref name, .. }) => {
                            if opts.do_not_convert_enum_to_string_nor_number {
                                fail!()
                            }

                            // Allow assigning enum with numeric values to
                            // number.
                            if let Ok(Type::Lit(LitType {
                                lit: RTsLit::Number(..), ..
                            })) = self.expand_enum_variant(rhs.clone())
                            {
                                return Ok(());
                            }

                            fail!()
                        }
                        _ => {}
                    },

                    TsKeywordTypeKind::TsBooleanKeyword => {
                        if let Type::Lit(LitType { lit: RTsLit::Bool(..), .. }) = *rhs {
                            return Ok(());
                        }
                    }

                    TsKeywordTypeKind::TsBigIntKeyword => {
                        if let Type::Lit(LitType {
                            lit: RTsLit::BigInt(..), ..
                        }) = *rhs
                        {
                            return Ok(());
                        }
                    }

                    TsKeywordTypeKind::TsSymbolKeyword => {
                        //

                        if rhs.is_symbol_like() {
                            return Ok(());
                        }
                        fail!()
                    }

                    TsKeywordTypeKind::TsObjectKeyword => {
                        match *rhs {
                            Type::Keyword(KeywordType {
                                kind: TsKeywordTypeKind::TsNumberKeyword,
                                ..
                            })
                            | Type::Keyword(KeywordType {
                                kind: TsKeywordTypeKind::TsStringKeyword,
                                ..
                            })
                            | Type::Keyword(KeywordType {
                                kind: TsKeywordTypeKind::TsBooleanKeyword,
                                ..
                            })
                            | Type::Keyword(KeywordType {
                                kind: TsKeywordTypeKind::TsBigIntKeyword,
                                ..
                            })
                            | Type::Keyword(KeywordType {
                                kind: TsKeywordTypeKind::TsVoidKeyword,
                                ..
                            })
                            | Type::Keyword(KeywordType {
                                kind: TsKeywordTypeKind::TsNullKeyword,
                                ..
                            })
                            | Type::Keyword(KeywordType {
                                kind: TsKeywordTypeKind::TsUndefinedKeyword,
                                ..
                            })
                            | Type::Lit(..) => {
                                fail!()
                            }

                            // let a: object = {};
                            Type::Function(..)
                            | Type::Constructor(..)
                            | Type::Enum(..)
                            | Type::Interface(..)
                            | Type::Class(..)
                            | Type::TypeLit(..) => return Ok(()),

                            _ => {}
                        }
                    }
                    _ => {}
                }

                match kind {
                    TsKeywordTypeKind::TsStringKeyword
                    | TsKeywordTypeKind::TsBigIntKeyword
                    | TsKeywordTypeKind::TsNumberKeyword
                    | TsKeywordTypeKind::TsBooleanKeyword
                    | TsKeywordTypeKind::TsNullKeyword
                    | TsKeywordTypeKind::TsUndefinedKeyword => match rhs {
                        Type::Lit(..) | Type::Function(..) | Type::Constructor(..) | Type::Interface(..) => fail!(),
                        Type::TypeLit(..) => {
                            let left = self.normalize(
                                Some(span),
                                Cow::Borrowed(to),
                                NormalizeTypeOpts {
                                    normalize_keywords: true,
                                    ..Default::default()
                                },
                            )?;
                            return self
                                .assign_inner(
                                    data,
                                    &left,
                                    rhs,
                                    AssignOpts {
                                        allow_unknown_rhs: Some(false),
                                        allow_missing_fields: false,
                                        ..opts
                                    },
                                )
                                .convert_err(|err| ErrorKind::SimpleAssignFailed {
                                    span: err.span(),
                                    cause: Some(box err.into()),
                                })
                                .context("tried to assign a type literal to an expanded keyword");
                        }
                        _ => {}
                    },
                    _ => {}
                }
            }

            Type::EnumVariant(ref l @ EnumVariant { name: Some(..), .. }) => match *rhs {
                Type::EnumVariant(ref r) => {
                    if l.def.id == r.def.id && l.name == r.name {
                        return Ok(());
                    }
                }
                Type::Lit(..)
                | Type::TypeLit(..)
                | Type::Keyword(KeywordType {
                    kind: TsKeywordTypeKind::TsStringKeyword,
                    ..
                })
                | Type::Keyword(KeywordType {
                    kind: TsKeywordTypeKind::TsNumberKeyword,
                    ..
                })
                | Type::Keyword(KeywordType {
                    kind: TsKeywordTypeKind::TsBooleanKeyword,
                    ..
                }) => {
                    fail!()
                }
                _ => {}
            },

            // TODO(kdy1): Use data stored in the current scope.
            Type::This(ThisType { span, .. }) => return Ok(()),

            Type::Interface(Interface {
                name,
                ref body,
                ref extends,
                ..
            }) if !rhs.is_type_param() => {
                // TODO(kdy1): Optimize handling of unknown rhs

                if name == "Function" {
                    if let Type::Function(..) = rhs.normalize() {
                        return Ok(());
                    }
                }

                self.assign_to_type_elements(
                    data,
                    span,
                    body,
                    rhs,
                    Default::default(),
                    AssignOpts {
                        allow_unknown_rhs: Some(true),
                        allow_assignment_of_array_to_optional_type_lit: true,
                        ..opts
                    },
                )
                .context("tried to assign a type to an interface")?;

                let mut errors = vec![];
                for parent in extends {
                    let parent = self
                        .type_of_ts_entity_name(span, &parent.expr, parent.type_args.as_deref())?
                        .freezed();

                    // An interface can extend a class.
                    let parent = self.instantiate_class(span, &parent)?;

                    let res = self.assign_with_opts(
                        data,
                        &parent,
                        rhs,
                        AssignOpts {
                            allow_unknown_rhs: Some(true),
                            ..opts
                        },
                    );

                    errors.extend(res.err());
                }

                if !extends.is_empty() && errors.is_empty() {
                    return Ok(());
                }

                // TODO(kdy1): Prevent recursion and uncomment the code below.
                //
                // // We try assigning as builtin interfaces.
                // match rhs {
                //     Type::Keyword(KeywordType {
                //         kind: TsKeywordTypeKind::TsStringKeyword,
                //         ..
                //     })
                //     | Type::Lit(LitType {
                //         lit: RTsLit::Str(..), ..
                //     }) => {
                //         return self
                //             .assign_inner(
                //                 to,
                //                 &Type::Ref(Ref {
                //                     span,
                //                     ctxt: ModuleId::builtin(),
                //                     type_name:
                // RTsEntityName::Ident(RIdent::new("String".into(), span)),
                //                     type_args: None,
                //                 }),
                //                 opts,
                //             )
                //             .context("tried to assign by converting rhs to builtin interface
                // 'String'")     }
                //     _ => {}
                // }

                // Assignment failed. This check is required to distinguish an empty interface
                // from an interface with parents.
                //
                // TODO(kdy1): Use errors returned from parent assignment.
                if body.is_empty() && !extends.is_empty() {
                    return Err(ErrorKind::AssignFailed {
                        span,
                        left: box to.clone(),
                        right: box rhs.clone(),
                        right_ident: opts.right_ident_span,
                        cause: errors,
                    }
                    .into());
                }

                // We should check for unknown rhs, while allowing assignment to parent
                // interfaces.
                if !opts.allow_unknown_rhs.unwrap_or_default() && !opts.allow_unknown_rhs_if_expanded {
                    let lhs = self.convert_type_to_type_lit(span, Cow::Borrowed(to), Default::default())?;
                    if let Some(lhs) = lhs {
                        self.assign_to_type_elements(data, span, &lhs.members, rhs, Default::default(), opts)
                            .with_context(|| {
                                format!(
                                    "tried to assign a type to an interface to check if unknown rhs exists\nLHS: {}\nRHS: {}",
                                    dump_type_as_string(&Type::TypeLit(lhs.into_owned())),
                                    dump_type_as_string(rhs)
                                )
                            })?;
                    }
                }

                if !errors.is_empty() {
                    return Err(ErrorKind::AssignFailed {
                        span,
                        left: box to.clone(),
                        right: box rhs.clone(),
                        right_ident: opts.right_ident_span,
                        cause: errors,
                    }
                    .into());
                }

                return Ok(());
            }

            Type::TypeLit(TypeLit { ref members, metadata, .. }) if !rhs.is_type_param() => {
                return self
                    .assign_to_type_elements(
                        data,
                        span,
                        members,
                        rhs,
                        *metadata,
                        AssignOpts {
                            report_assign_failure_for_missing_properties: opts.report_assign_failure_for_missing_properties.or_else(|| {
                                match rhs.normalize() {
                                    Type::Interface(r) if !r.extends.is_empty() => Some(true),
                                    _ => None,
                                }
                            }),
                            ..opts
                        },
                    )
                    .context("tried to assign a type to type elements");
            }

            Type::Lit(LitType { ref lit, .. }) => match *rhs {
                Type::Lit(LitType { lit: ref r_lit, .. }) => {
                    if lit.eq_ignore_span(r_lit) {
                        return Ok(());
                    }

                    // Extra check to handle "has_escape"
                    match (lit, r_lit) {
                        (RTsLit::Str(l), RTsLit::Str(r)) if l.value == r.value => return Ok(()),
                        _ => {}
                    }

                    fail!()
                }

                Type::Ref(..) | Type::Param(..) | Type::Query(..) => {}

                // TODO(kdy1): allow
                // let a: true | false = bool
                _ => fail!(),
            },

            Type::Function(lf) => match rhs {
                Type::Function(..) | Type::TypeLit(..) | Type::Interface(..) => {
                    return self.assign_to_function(data, to, lf, rhs, opts).with_context(|| {
                        format!(
                            "tried to assign to a function type: {}",
                            dump_type_as_string(&Type::Function(lf.clone()))
                        )
                    })
                }
                Type::Keyword(KeywordType {
                    kind: TsKeywordTypeKind::TsVoidKeyword,
                    ..
                })
                | Type::Keyword(KeywordType {
                    kind: TsKeywordTypeKind::TsNumberKeyword,
                    ..
                })
                | Type::Keyword(KeywordType {
                    kind: TsKeywordTypeKind::TsStringKeyword,
                    ..
                })
                | Type::Keyword(KeywordType {
                    kind: TsKeywordTypeKind::TsBigIntKeyword,
                    ..
                })
                | Type::Keyword(KeywordType {
                    kind: TsKeywordTypeKind::TsBooleanKeyword,
                    ..
                })
                | Type::Constructor(..)
                | Type::Array(..)
                | Type::Tuple(..) => {
                    fail!()
                }
                _ => {}
            },

            Type::Tuple(l) => {
                if let Some(()) = self
                    .assign_to_tuple(data, l, to, rhs, opts)
                    .context("tried to assign to a tuple type")?
                {
                    return Ok(());
                }
            }

            Type::Constructor(ref lc) => {
                return self
                    .assign_to_constructor(data, to, lc, rhs, opts)
                    .context("tried to assign to a constructor type")
            }

            _ => {}
        }

        match rhs {
            Type::Enum(ref e) => match to {
                Type::Interface(..) | Type::TypeLit(..) => {
                    fail!()
                }
                _ => {
                    handle_enum_in_rhs!(e)
                }
            },

            // Handle unknown on rhs
            Type::Keyword(KeywordType {
                kind: TsKeywordTypeKind::TsUnknownKeyword,
                ..
            }) => {
                if to.is_kwd(TsKeywordTypeKind::TsAnyKeyword) || to.is_kwd(TsKeywordTypeKind::TsUndefinedKeyword) {
                    return Ok(());
                }

                fail!();
            }

            Type::EnumVariant(EnumVariant { ref def, .. }) => {
                match to {
                    Type::Interface(..) | Type::TypeLit(..) => {}
                    _ => {
                        handle_enum_in_rhs!(def)
                    }
                }

                fail!()
            }

            _ => {}
        }

        match to {
            // Handle symbol assignments
            Type::Unique(u) if u.ty.is_kwd(TsKeywordTypeKind::TsSymbolKeyword) => {
                if rhs.is_symbol() {
                    return Ok(());
                }
            }

            Type::Predicate(..) => {
                if rhs.is_kwd(TsKeywordTypeKind::TsBooleanKeyword) {
                    return Ok(());
                }
            }

            Type::Index(Index { ty, .. }) if ty.is_type_param() => {
                return self
                    .assign_with_opts(
                        data,
                        &Type::Keyword(KeywordType {
                            span: DUMMY_SP,
                            kind: TsKeywordTypeKind::TsStringKeyword,
                            metadata: Default::default(),
                            tracker: Default::default(),
                        }),
                        rhs,
                        opts,
                    )
                    .context("tried to assign a type to a `keyof TypeParam`")
            }

            _ => {}
        }

        if to.is_symbol() || to.is_kwd(TsKeywordTypeKind::TsNeverKeyword) || rhs.is_kwd(TsKeywordTypeKind::TsVoidKeyword) {
            fail!()
        }

        match (to, rhs) {
            (Type::Tpl(l), r) => {
                return self
                    .assign_to_tpl(data, l, r, opts)
                    .with_context(|| format!("tried to assign to a template type: {}", force_dump_type_as_string(to)))
            }
            (
                Type::Keyword(KeywordType {
                    kind: TsKeywordTypeKind::TsStringKeyword,
                    ..
                }),
                Type::Tpl(..),
            )
            | (
                Type::Predicate(..),
                Type::Keyword(KeywordType {
                    kind: TsKeywordTypeKind::TsBooleanKeyword,
                    ..
                }),
            )
            | (Type::Predicate(..), Type::Predicate(..)) => return Ok(()),

            (Type::StringMapping(l), r) => return self.assign_to_intrinsic(data, l, r, opts),

            (Type::Rest(l), Type::Rest(r)) => {
                return self
                    .assign_with_opts(data, &l.ty, &r.ty, opts)
                    .context("tried to assign to a rest type")
            }

            (Type::IndexedAccessType(..), _) | (_, Type::IndexedAccessType(..)) => {
                fail!()
            }

            (Type::Function(..) | Type::Unique(..), Type::Lit(..)) => {
                fail!()
            }

            (
                Type::Keyword(KeywordType {
                    kind: TsKeywordTypeKind::TsObjectKeyword,
                    ..
                }),
                Type::Array(..) | Type::Tuple(..),
            ) => return Ok(()),

            (
                Type::Keyword(KeywordType {
                    kind:
                        TsKeywordTypeKind::TsStringKeyword
                        | TsKeywordTypeKind::TsBigIntKeyword
                        | TsKeywordTypeKind::TsNumberKeyword
                        | TsKeywordTypeKind::TsBooleanKeyword,
                    ..
                }),
                Type::Array(..) | Type::Tuple(..) | Type::Unique(..),
            ) => fail!(),

            (
                Type::Optional(..),
                Type::Keyword(KeywordType {
                    kind: TsKeywordTypeKind::TsUndefinedKeyword,
                    ..
                }),
            ) => return Ok(()),

            (Type::Optional(l_opt), _) => {
                return self
                    .assign_inner(data, &l_opt.ty.clone().union_with_undefined(span), rhs, opts)
                    .context("tried to assign to an optional type")
            }

            (_, Type::Readonly(r_readonly)) => {
                return self
                    .assign_inner(data, to, &r_readonly.ty, opts)
                    .context("tried to assign a readonly type to another type")
            }

            (
                _,
                Type::Keyword(KeywordType {
                    kind: TsKeywordTypeKind::TsVoidKeyword,
                    ..
                }),
            ) => fail!(),

            (_, Type::Param(TypeParam { constraint, .. })) => {
                match *constraint {
                    Some(ref c) => {
                        return self.assign_inner(
                            data,
                            to,
                            c,
                            AssignOpts {
                                allow_unknown_rhs: Some(true),
                                ..opts
                            },
                        );
                    }
                    None => {
                        // unknownType1.ts says

                        // Type parameter with explicit 'unknown' constraint not assignable to '{}'

                        match to.normalize() {
                            Type::TypeLit(TypeLit { ref members, .. }) if members.is_empty() => {
                                if self.rule().strict_null_checks {
                                    fail!()
                                } else {
                                    return Ok(());
                                }
                            }
                            _ => {}
                        }
                    }
                }

                match to.normalize() {
                    Type::Union(..) => {}
                    Type::Mapped(m) => {
                        if let Err(err) = self.assign_to_mapped(data, m, rhs, opts) {
                            fail!()
                        }
                    }
                    Type::TypeLit(to) => {
                        // Don't ask why.
                        //
                        // See: subtypingWithOptionalProperties.ts
                        if !self.rule().strict_null_checks
                            && to.members.iter().all(|el| match el {
                                TypeElement::Property(p) => p.optional,
                                _ => false,
                            })
                        {
                            return Ok(());
                        } else {
                            fail!()
                        }
                    }

                    _ => {
                        fail!()
                    }
                }
            }

            _ => {}
        }

        // TODO(kdy1): Implement full type checker
        error!(
            "unimplemented: assign: \nLeft: {}\nRight: {}",
            force_dump_type_as_string(to),
            force_dump_type_as_string(rhs)
        );
        Ok(())
    }

    /// Should be called only if `to` is not expandable.
    pub(super) fn assign_to_intrinsic(&mut self, data: &mut AssignData, to: &StringMapping, r: &Type, opts: AssignOpts) -> VResult<()> {
        match r.normalize() {
            Type::Keyword(KeywordType {
                kind: TsKeywordTypeKind::TsAnyKeyword,
                ..
            })
            | Type::Keyword(KeywordType {
                kind: TsKeywordTypeKind::TsNeverKeyword,
                ..
            }) => {
                return Ok(());
            }
            Type::Keyword(KeywordType {
                kind: TsKeywordTypeKind::TsStringKeyword,
                ..
            }) => {
                if self.ctx.in_actual_type {
                    return Ok(());
                }

                // declare var x: Uppercase<string>
                if let PatMode::Decl = self.ctx.pat_mode {
                    if to.type_args.params[0].is_str() {
                        return Ok(());
                    }
                }

                if self.ctx.in_return_arg {
                    if to.type_args.params[0].is_str_like() {
                        return Ok(());
                    }
                }

                let span = opts.span.or_else(|| to.span());

                return Err(ErrorKind::AssignFailed {
                    span,
                    left: box Type::StringMapping(to.clone()),
                    right_ident: None,
                    right: box r.clone(),
                    cause: vec![],
                }
                .into());
            }

            Type::Lit(LitType {
                lit: RTsLit::Str(str_lit), ..
            }) => {
                let type_param = &to.type_args.params[0];

                if self.ctx.in_actual_type {
                    return Ok(());
                }

                match to.kind {
                    IntrinsicKind::Uppercase => {
                        if let Some(value) = &str_lit.raw {
                            if value.to_uppercase() != **value {
                                return Err(ErrorKind::AssignFailed {
                                    span: str_lit.span(),
                                    left: box Type::StringMapping(to.clone()),
                                    right_ident: None,
                                    right: box r.clone(),
                                    cause: vec![],
                                }
                                .into());
                            }
                        }
                    }
                    IntrinsicKind::Lowercase => {
                        if let Some(value) = &str_lit.raw {
                            if value.to_lowercase() != **value {
                                return Err(ErrorKind::AssignFailed {
                                    span: str_lit.span(),
                                    left: box Type::StringMapping(to.clone()),
                                    right_ident: None,
                                    right: box r.clone(),
                                    cause: vec![],
                                }
                                .into());
                            }
                        }
                    }
                    IntrinsicKind::Capitalize => {
                        if let Some(value) = &str_lit.raw {
                            let ch = value.chars().next();

                            if let Some(ch) = ch {
                                if !ch.is_uppercase() {
                                    return Err(ErrorKind::AssignFailed {
                                        span: str_lit.span(),
                                        left: box Type::StringMapping(to.clone()),
                                        right_ident: None,
                                        right: box r.clone(),
                                        cause: vec![],
                                    }
                                    .into());
                                }
                            }
                        }
                    }
                    IntrinsicKind::Uncapitalize => {
                        if let Some(value) = &str_lit.raw {
                            let ch = value.chars().next();

                            if let Some(ch) = ch {
                                if !ch.is_lowercase() {
                                    return Err(ErrorKind::AssignFailed {
                                        span: str_lit.span(),
                                        left: box Type::StringMapping(to.clone()),
                                        right_ident: None,
                                        right: box r.clone(),
                                        cause: vec![],
                                    }
                                    .into());
                                }
                            }
                        }
                    }
                }
            }
            Type::Lit(lit_type) => {
                let ty = match lit_type.lit {
                    RTsLit::Number(..) | RTsLit::BigInt(..) => TsKeywordTypeKind::TsNumberKeyword,
                    RTsLit::Bool(..) => TsKeywordTypeKind::TsBooleanKeyword,
                    _ => {
                        return Err(ErrorKind::AssignFailed {
                            span: r.span(),
                            left: box Type::StringMapping(to.clone()),
                            right_ident: None,
                            right: box r.clone(),
                            cause: vec![],
                        }
                        .into());
                    }
                };

                return Err(ErrorKind::NotSatisfyConstraint {
                    span: to.type_args.params[0].span(),
                    left: box Type::Keyword(KeywordType {
                        kind: TsKeywordTypeKind::TsStringKeyword,
                        span: to.span(),
                        metadata: Default::default(),
                        tracker: Default::default(),
                    }),
                    right: box Type::Keyword(KeywordType {
                        kind: ty,
                        span: r.span(),
                        metadata: Default::default(),
                        tracker: Default::default(),
                    }),
                }
                .into());
            }
            Type::Union(ty) => {
                // TODO: Maybe change when https://github.com/dudykr/stc/issues/795 is resolved
                // This handles cases where one of the union elements is any
                // ex: type A = Uppercase<any | 30> // no error
                if ty.types.iter().any(|v| v.is_any()) {
                    return Ok(());
                }

                if ty.types.iter().all(|v| v.is_str_like()) {
                    return Ok(());
                }

                return Err(ErrorKind::AssignFailed {
                    span: r.span(),
                    left: box Type::StringMapping(to.clone()),
                    right_ident: None,
                    right: box r.clone(),
                    cause: vec![],
                }
                .into());
            }
            Type::StringMapping(string) => {
                if self.ctx.in_actual_type {
                    return Ok(());
                }

                if !self.ctx.in_declare && to.kind != string.kind {
                    return Err(ErrorKind::AssignFailed {
                        span: opts.span,
                        left: box Type::StringMapping(to.clone()),
                        right_ident: None,
                        right: box r.clone(),
                        cause: vec![],
                    }
                    .into());
                }

                let mut last_ty = &to.type_args.params[0];
                let mut last_r_ty = &string.type_args.params[0];
                loop {
                    match (last_ty.normalize(), last_r_ty.normalize()) {
                        (Type::StringMapping(l_map), Type::StringMapping(r_map)) => {
                            last_ty = &l_map.type_args.params[0];
                            last_r_ty = &r_map.type_args.params[0];
                        }
                        (Type::StringMapping(l_map), _) => {
                            last_ty = &l_map.type_args.params[0];
                        }
                        (_, Type::StringMapping(r_map)) => {
                            last_r_ty = &r_map.type_args.params[0];
                        }
                        _ => {
                            break;
                        }
                    }
                }

                // if lhs_param extends rhs_param (or both params) -> error
                if let (Type::Param(l), Type::Param(rr)) = (last_ty, last_r_ty) {
                    if let Some(constraint) = &rr.constraint {
                        let Type::Param(param) = constraint.normalize() else {
                            return Err(ErrorKind::AssignFailed {
                                span: opts.span,
                                left: box Type::StringMapping(to.clone()),
                                right_ident: None,
                                right: box r.clone(),
                                cause: vec![],
                            }
                            .into());
                        };
                        if !param.type_eq(l) {
                            return Err(ErrorKind::AssignFailed {
                                span: opts.span,
                                left: box Type::StringMapping(to.clone()),
                                right_ident: None,
                                right: box r.clone(),
                                cause: vec![],
                            }
                            .into());
                        }
                    } else {
                        return Err(ErrorKind::AssignFailed {
                            span: opts.span,
                            left: box Type::StringMapping(to.clone()),
                            right_ident: None,
                            right: box r.clone(),
                            cause: vec![],
                        }
                        .into());
                    }

                    // if both params, but rhs_param extends lhs_param -> no error
                    if let Some(constraint) = &l.constraint {
                        if let Type::Param(param_l) = constraint.normalize() {
                            if let Some(box Type::Param(param_r)) = &rr.constraint {
                                if !param_l.type_eq(param_r) {
                                    return Err(ErrorKind::AssignFailed {
                                        span: opts.span,
                                        left: box Type::StringMapping(to.clone()),
                                        right_ident: None,
                                        right: box r.clone(),
                                        cause: vec![],
                                    }
                                    .into());
                                }
                            }
                        }
                    }
                }

                return Ok(());
            }
            Type::Tpl(tpl) => {
                // Make sure all template-literal types are valid
                // type X = { x: string }
                // type A = Uppercase<`hello${X}`> // should error
                for v in tpl.types.iter() {
                    match v.normalize() {
                        Type::Ref(ref_ty) => {
                            if let Ok(ty) = &self.expand_top_ref(ref_ty.span, Cow::Borrowed(v), Default::default()) {
                                if !(ty.is_any()
                                    || ty.is_num_like()
                                    || ty.is_bool_like()
                                    || ty.is_str_like()
                                    || ty.is_bigint_like()
                                    || ty.is_null()
                                    || ty.is_undefined())
                                {
                                    return Err(ErrorKind::AssignFailed {
                                        span: ref_ty.span(),
                                        left: box Type::StringMapping(to.clone()),
                                        right_ident: None,
                                        right: box r.clone(),
                                        cause: vec![],
                                    }
                                    .into());
                                }
                            }
                        }
                        Type::Param(param) => {
                            if let Some(constraint) = &param.constraint {
                                let v = &**constraint;

                                if !(v.is_any()
                                    || v.is_num_like()
                                    || v.is_bigint_like()
                                    || v.is_bool_like()
                                    || v.is_str_like()
                                    || v.is_null()
                                    || v.is_undefined())
                                {
                                    return Err(ErrorKind::AssignFailed {
                                        span: to.span(),
                                        left: box Type::StringMapping(to.clone()),
                                        right_ident: None,
                                        right: box r.clone(),
                                        cause: vec![],
                                    }
                                    .into());
                                }
                            }
                        }
                        Type::Union(ty) => {
                            if ty.types.iter().any(|v| v.is_any()) {
                                return Ok(());
                            }
                            // type A = Uppercase<`aB${string | number}`> - valid
                            // type A = Uppercase<`aB${string | { x: string }}`>; - invalid
                            let is_valid_union = ty.types.iter().all(|v| {
                                v.is_num_like()
                                    || v.is_bigint_like()
                                    || v.is_bool_like()
                                    || v.is_str_like()
                                    || v.is_null()
                                    || v.is_undefined()
                            });

                            if !is_valid_union {
                                return Err(ErrorKind::AssignFailed {
                                    span: ty.span(),
                                    left: box Type::StringMapping(to.clone()),
                                    right_ident: None,
                                    right: box r.clone(),
                                    cause: vec![],
                                }
                                .into());
                            }
                        }
                        _ => {
                            if !(v.is_any()
                                || v.is_num_like()
                                || v.is_bigint_like()
                                || v.is_bool_like()
                                || v.is_str_like()
                                || v.is_null()
                                || v.is_undefined())
                            {
                                return Err(ErrorKind::AssignFailed {
                                    span: to.span(),
                                    left: box Type::StringMapping(to.clone()),
                                    right_ident: None,
                                    right: box r.clone(),
                                    cause: vec![],
                                }
                                .into());
                            }
                        }
                    }
                }

                return Ok(());
            }

            Type::Param(param) => {
                if let Some(constraint) = &param.constraint {
                    if constraint.is_union_type() || constraint.is_type_param() {
                        return self.assign_to_intrinsic(data, to, constraint, opts);
                    }

                    if !constraint.is_str_like() && !constraint.is_never() {
                        let span = to.type_args.span().or_else(|| to.span());

                        return Err(ErrorKind::NotSatisfyConstraint {
                            // This ideally should be to.type_args.params[0].span()
                            // but that gives wrong span pos
                            span,
                            left: box Type::Keyword(KeywordType {
                                kind: TsKeywordTypeKind::TsStringKeyword,
                                span: to.span(),
                                metadata: Default::default(),
                                tracker: Default::default(),
                            }),
                            right: constraint.clone(),
                        }
                        .into());
                    }
                }
            }
            _ => {
                return Err(ErrorKind::AssignFailed {
                    span: to.span(),
                    left: box Type::StringMapping(to.clone()),
                    right_ident: None,
                    right: box r.clone(),
                    cause: vec![],
                }
                .into());
            }
        }

        Ok(())
    }

    fn extract_keys(&mut self, span: Span, ty: &Type) -> VResult<Type> {
        (|| -> VResult<_> {
            let ty = self.normalize(
                Some(span),
                Cow::Borrowed(ty),
                NormalizeTypeOpts {
                    normalize_keywords: true,
                    process_only_key: true,
                    ..Default::default()
                },
            )?;
            let ty = ty.normalize();

            if let Type::TypeLit(ty) = ty {
                //
                let mut keys = vec![];
                for member in &ty.members {
                    if let TypeElement::Property(PropertySignature {
                        span,
                        key: Key::Normal { sym: key, .. },
                        ..
                    }) = member
                    {
                        keys.push(Type::Lit(LitType {
                            span: *span,
                            lit: RTsLit::Str(RStr {
                                span: *span,
                                value: key.clone(),
                                raw: None,
                            }),
                            metadata: Default::default(),
                            tracker: Default::default(),
                        }));
                    }
                }

                return Ok(Type::new_union(span, keys));
            }

            if let Some(ty) = self
                .convert_type_to_type_lit(span, Cow::Borrowed(ty), Default::default())?
                .map(Cow::into_owned)
                .map(Type::TypeLit)
            {
                return self.extract_keys(span, &ty);
            }

            Err(ErrorKind::Unimplemented {
                span,
                msg: "Extract keys".to_string(),
            })?
        })()
        .context("tried to extract keys")
    }

    /// Handles `P in 'foo' | 'bar'`. Note that `'foo' | 'bar'` part should be
    /// passed as `keys`.
    ///
    ///
    /// Currently only literals and unions are supported for `keys`.
    fn assign_keys(&mut self, data: &mut AssignData, keys: &Type, rhs: &Type, opts: AssignOpts) -> VResult<()> {
        let keys = keys.normalize();
        let rhs = rhs.normalize();

        let mut rhs_keys = self.extract_keys(opts.span, rhs)?;
        rhs_keys.freeze();

        self.assign_with_opts(
            data,
            keys,
            &rhs_keys,
            AssignOpts {
                allow_unknown_rhs: Some(true),
                ..opts
            },
        )
        .context("tried to assign keys")
    }

    fn assign_to_mapped(&mut self, data: &mut AssignData, l: &Mapped, r: &Type, opts: AssignOpts) -> VResult<()> {
        let span = opts.span;
        let mut r = self
            .normalize(Some(span), Cow::Borrowed(r), NormalizeTypeOpts { ..Default::default() })
            .context("tried to normalize rhs of assignment (to a mapped type)")?;
        r.freeze();

        let res: VResult<_> = try {
            // Validate keys

            let l_ty = match &l.ty {
                Some(v) => v.normalize(),
                None => return Ok(()),
            };

            match r.normalize() {
                Type::Interface(..) | Type::Class(..) | Type::ClassDef(..) | Type::Intersection(..) => {
                    if let Some(r) = self
                        .convert_type_to_type_lit(span, Cow::Borrowed(&r), Default::default())?
                        .map(Cow::into_owned)
                        .map(Type::TypeLit)
                    {
                        self.assign_to_mapped(data, l, &r, opts)
                            .context("tried to assign a type to a mapped type by converting it to a type literal")?;
                        return Ok(());
                    }
                }

                Type::TypeLit(rt) => {
                    match &l.type_param.constraint {
                        Some(constraint) => self.assign_keys(data, constraint, &r, opts)?,
                        None => {}
                    }

                    //
                    for member in &rt.members {
                        match member {
                            TypeElement::Property(prop) => {
                                if let Some(prop_ty) = &prop.type_ann {
                                    self.assign_with_opts(data, l_ty, prop_ty, opts)?;
                                }
                            }
                            TypeElement::Index(ri) => {
                                if !ri.params.is_empty() && is_str_or_union(&ri.params[0].ty) {
                                    if let Some(lt) = &l.ty {
                                        if let Some(rt) = &ri.type_ann {
                                            return self
                                                .assign_inner(data, lt, rt, opts)
                                                .context("tried to assign an index signature to a mapped type");
                                        }
                                    }

                                    return Ok(());
                                }
                            }

                            _ => Err(ErrorKind::Unimplemented {
                                span: opts.span,
                                msg: format!("Assignment to mapped type: type element - {:?}", member),
                            })?,
                        }
                    }

                    return Ok(());
                }
                Type::Param(ty) => {
                    // Try assign type param `T` has no constraint to mapped
                    // type takes `T` as an arg.
                    // Error will occur if mapped type including `-?`
                    // modifiers.
                    //
                    // ex) type Required<T> = { [P in keyof T]-?: T[P]; }
                    // ```ts
                    // function error<T>(x: Required<T>, y: T) {
                    //     x = y; // error TS2322
                    // }
                    // ```
                    let remove_opt = matches!(l.optional, Some(Minus));
                    if let Some(constraint @ Type::Index(Index { ty, .. })) = l.type_param.constraint.as_deref().map(|ty| ty.normalize()) {
                        if r.type_eq(ty) && !remove_opt {
                            return Ok(());
                        }
                    }
                }
                Type::Mapped(r) => {
                    if l.type_eq(r) {
                        return Ok(());
                    }

                    // If constraint is identical, we replace type parameter of rhs and see if
                    // return type is identical.
                    //
                    if l.type_param.constraint.type_eq(&r.type_param.constraint) {
                        if l.ty.type_eq(&r.ty) {
                            return Ok(());
                        }

                        let mut map = HashMap::default();
                        map.insert(r.type_param.name.clone(), Type::Param(l.type_param.clone()).freezed());

                        let new_r_ty = self.expand_type_params(&map, r.ty.clone(), Default::default())?;

                        if l.ty.type_eq(&new_r_ty) {
                            return Ok(());
                        }

                        if let Some(l) = &l.ty {
                            if let Some(r) = &new_r_ty {
                                Err(ErrorKind::Unimplemented {
                                    span: opts.span,
                                    msg: format!("Assignment to mapped type\n{}\n{}", dump_type_as_string(l), dump_type_as_string(r),),
                                })?
                            }
                        }
                    }
                }
                _ => {}
            }

            Err(ErrorKind::Unimplemented {
                span: opts.span,
                msg: "Assignment to mapped type".to_string(),
            })?
        };

        res.with_context(|| format!("tried to assign {} to a mapped type", force_dump_type_as_string(&r)))
    }

    /// Returns true for `A | B | | C = A | B` and similar cases.
    ///
    /// Should be called iff lhs is a union type.
    fn should_use_special_union_assignment(&mut self, span: Span, r: &Type) -> VResult<bool> {
        match r.normalize() {
            Type::Union(..) => return Ok(true),
            Type::TypeLit(r) => {
                if r.members.iter().all(|el| matches!(el, TypeElement::Call(..))) {
                    return Ok(true);
                }

                if r.members.iter().all(|el| matches!(el, TypeElement::Constructor(..))) {
                    return Ok(true);
                }
            }
            _ => {}
        }

        Ok(false)
    }

    /// TODO(kdy1): I'm not sure about this.
    fn variance(&mut self, ty: &Conditional) -> VResult<Variance> {
        let can_be_covariant = self.is_covariant(&ty.check_type, &ty.true_type)? || self.is_covariant(&ty.check_type, &ty.false_type)?;

        let can_be_contravariant =
            self.is_contravariant(&ty.check_type, &ty.true_type)? || self.is_contravariant(&ty.check_type, &ty.false_type)?;

        match (can_be_covariant, can_be_contravariant) {
            (true, true) | (false, false) => Ok(Variance::Invariant),
            (true, false) => Ok(Variance::Covariant),
            (false, true) => Ok(Variance::Contravariant),
        }
    }

    fn is_covariant(&mut self, check_type: &Type, output_type: &Type) -> VResult<bool> {
        Ok(check_type.type_eq(output_type))
    }

    fn is_contravariant(&mut self, check_type: &Type, output_type: &Type) -> VResult<bool> {
        if let Type::Index(Index { ty, .. }) = output_type.normalize() {
            if output_type.type_eq(&**ty) {
                return Ok(true);
            }
        }

        Ok(false)
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub(crate) enum Variance {
    Covariant,
    Contravariant,
    Invariant,
}

//fn type_of_ts_fn_param<'a>(p: &TsFnParam) -> Type {
//    match p {
//        RTsFnParam::Ident(Ident { type_ann, .. })
//        | TsFnParam::Array(ArrayPat { type_ann, .. })
//        | TsFnParam::Object(ObjectPat { type_ann, .. })
//        | TsFnParam::Rest(RRestPat { type_ann, .. }) => type_ann
//            .clone()
//            .map(|ty| Type::from(ty))
//            .unwrap_or(Type::any(p.span())),
//    }
//}

pub(crate) fn get_tuple_subtract_count(t: &[TupleElement]) -> usize {
    let rest_pos = t.iter().position(|e| e.ty.is_rest());

    match rest_pos {
        Some(rest_pos) => {
            // If the rest is not the last, we should return the index of rest
            if t.iter().skip(rest_pos).any(|e| !e.ty.is_rest()) {
                t.len() - rest_pos
            } else {
                0
            }
        }
        None => {
            // No rest means we can iterate over whole tuple.
            0
        }
    }
}
