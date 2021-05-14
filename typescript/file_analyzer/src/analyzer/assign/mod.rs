use super::Analyzer;
use crate::analyzer::types::NormalizeTypeOpts;
use crate::{ty::TypeExt, ValidationResult};
use rnode::NodeId;
use stc_ts_ast_rnode::RBool;
use stc_ts_ast_rnode::RExpr;
use stc_ts_ast_rnode::RIdent;
use stc_ts_ast_rnode::RStr;
use stc_ts_ast_rnode::RTsEntityName;
use stc_ts_ast_rnode::RTsKeywordType;
use stc_ts_ast_rnode::RTsLit;
use stc_ts_ast_rnode::RTsLitType;
use stc_ts_ast_rnode::RTsThisType;
use stc_ts_errors::debug::dump_type_as_string;
use stc_ts_errors::debug::print_backtrace;
use stc_ts_errors::DebugExt;
use stc_ts_errors::Error;
use stc_ts_file_analyzer_macros::context;
use stc_ts_types::Key;
use stc_ts_types::Mapped;
use stc_ts_types::Operator;
use stc_ts_types::PropertySignature;
use stc_ts_types::Ref;
use stc_ts_types::{
    Array, EnumVariant, FnParam, Interface, Intersection, Tuple, Type, TypeElement, TypeLit, TypeParam, Union,
};
use stc_utils::stack;
use std::borrow::Cow;
use swc_atoms::js_word;
use swc_common::EqIgnoreSpan;
use swc_common::TypeEq;
use swc_common::DUMMY_SP;
use swc_common::{Span, Spanned};
use swc_ecma_ast::*;

mod builtin;
mod cast;
mod class;
mod function;
mod query;
mod tpl;
mod type_el;

/// Context used for `=` assignments.
#[derive(Debug, Clone, Copy, Default)]
pub(crate) struct AssignOpts {
    /// This field should be overrided by caller.
    pub span: Span,
    pub right_ident_span: Option<Span>,
    pub allow_unknown_rhs: bool,
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
    pub allow_assignment_to_param: bool,

    pub for_overload: bool,

    pub for_castablity: bool,

    /// If true, assignment of a class to another class without inheritance
    /// relation will fail, even if the class is empty.
    pub disallow_different_classes: bool,

    /// If true, `assign` will try to assign by converting rhs to an iterable.
    pub allow_iterable_on_rhs: bool,

    /// If `true`, assignment will success if rhs is `void`.
    pub allow_assignment_of_void: bool,

    pub allow_assignment_of_array_to_optional_type_lit: bool,

    pub use_missing_fields_for_class: bool,
}

#[derive(Default)]
pub struct AssignData {
    dejavu: Vec<(Type, Type)>,
}

impl Analyzer<'_, '_> {
    pub(crate) fn assign_with_op(&mut self, span: Span, op: AssignOp, lhs: &Type, rhs: &Type) -> ValidationResult<()> {
        debug_assert_ne!(op, op!("="));

        let l = self.expand_top_ref(span, Cow::Borrowed(lhs))?;
        let r = self.expand_top_ref(span, Cow::Borrowed(rhs))?;

        let lhs = l.normalize();
        let rhs = r.normalize();

        if op == op!("*=") {
            if rhs.is_kwd(TsKeywordTypeKind::TsUndefinedKeyword) {
                return Err(Error::ObjectIsPossiblyUndefined { span: rhs.span() });
            }
            if rhs.is_kwd(TsKeywordTypeKind::TsNullKeyword) {
                return Err(Error::ObjectIsPossiblyNull { span: rhs.span() });
            }

            let r_castable = self.can_be_casted_to_number_in_rhs(rhs.span(), &rhs);
            if r_castable {
                if l.is_num() {
                    return Ok(());
                }

                match lhs {
                    Type::Enum(l) => {
                        //
                        if !l.has_str {
                            return Ok(());
                        }
                    }
                    _ => {}
                }
            }
        }

        // Trivial
        if lhs.is_any() || rhs.is_any() {
            return Ok(());
        }

        // Addition to a string converts rhs into stirng.
        if lhs.is_str() {
            return Ok(());
        }

        if lhs.is_num() || lhs.is_enum_variant() || lhs.is_enum_type() {
            // TODO: Check if actual value is number.

            if rhs.is_num() {
                return Ok(());
            }

            if rhs.is_enum_variant() {
                // TODO: Check if actual value is numberx.
                return Ok(());
            }

            if rhs.is_kwd(TsKeywordTypeKind::TsUndefinedKeyword)
                || rhs.is_kwd(TsKeywordTypeKind::TsNullKeyword)
                || rhs.is_kwd(TsKeywordTypeKind::TsVoidKeyword)
            {
                return Err(Error::AssignOpCannotBeApplied { span, op });
            }
        }

        match lhs {
            Type::TypeLit(lhs) => {
                if lhs.members.is_empty() {
                    if rhs.is_str() {
                        return Ok(());
                    }
                }
            }
            _ => {}
        }

        match op {
            op!("+=") => {}

            op!("??=") | op!("&&=") => {
                if rhs.is_bool() {
                    return Ok(());
                }
            }
            _ => {}
        }

        Err(Error::InvalidOpAssign {
            span,
            op,
            lhs: box l.into_owned().clone(),
            rhs: box r.into_owned().clone(),
        })
    }

    /// TODO: Change argument order. (Span should come first).
    pub(crate) fn assign(
        &mut self,
        data: &mut AssignData,
        left: &Type,
        right: &Type,
        span: Span,
    ) -> ValidationResult<()> {
        self.assign_with_opts(
            data,
            AssignOpts {
                span,
                ..Default::default()
            },
            left,
            right,
        )
    }

    pub(crate) fn assign_with_opts(
        &mut self,
        data: &mut AssignData,
        opts: AssignOpts,
        left: &Type,
        right: &Type,
    ) -> ValidationResult<()> {
        if self.is_builtin {
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

        match res {
            Err(Error::Errors { errors, .. }) if errors.is_empty() => return Ok(()),
            _ => {}
        }

        res.map_err(|err| {
            err.convert(|err| match err {
                Error::AssignFailed { .. }
                | Error::Errors { .. }
                | Error::Unimplemented { .. }
                | Error::TupleAssignError { .. }
                | Error::ObjectAssignFailed { .. } => err,
                _ => Error::AssignFailed {
                    span: opts.span,
                    left: box left.clone(),
                    right: box right.clone(),
                    right_ident: opts.right_ident_span,
                    cause: vec![err],
                },
            })
        })
    }

    /// Verifies that `ty` is
    ///
    /// - Not a reference
    /// - Not a type parameter declared on child scope.
    fn verify_before_assign(&self, ctx: &'static str, ty: &Type) {
        ty.assert_valid();

        match ty.normalize() {
            Type::Ref(ref r) => {
                print_backtrace();
                panic!(
                    "A reference type (in {}) should be expanded before calling .assign()\n{:?}",
                    ctx, r,
                )
            }

            _ => {}
        }
    }

    fn normalize_for_assign<'a>(&mut self, span: Span, ty: &'a Type) -> ValidationResult<Cow<'a, Type>> {
        ty.assert_valid();

        let ty = ty.normalize();

        match ty {
            Type::Ref(Ref {
                span,
                type_name: RTsEntityName::Ident(type_name),
                type_args: None,
                ..
            }) => {
                // TODO: Check if ref points global.
                return Ok(Cow::Owned(Type::Keyword(RTsKeywordType {
                    span: *span,
                    kind: match type_name.sym {
                        js_word!("Boolean") => TsKeywordTypeKind::TsBooleanKeyword,
                        js_word!("Number") => TsKeywordTypeKind::TsNumberKeyword,
                        js_word!("String") => TsKeywordTypeKind::TsStringKeyword,
                        _ => return Ok(Cow::Borrowed(ty)),
                    },
                })));
            }
            Type::Conditional(..)
            | Type::Alias(..)
            | Type::Operator(Operator {
                op: TsTypeOperatorOp::KeyOf,
                ..
            }) => {
                let ty = self
                    .normalize(Some(span), Cow::Borrowed(ty), Default::default())
                    .context("tried to normalize a type for assignment")?
                    .into_owned();

                return Ok(Cow::Owned(ty));
            }
            _ => {}
        }

        Ok(Cow::Borrowed(ty))
    }

    fn assign_inner(
        &mut self,
        data: &mut AssignData,
        left: &Type,
        right: &Type,
        opts: AssignOpts,
    ) -> ValidationResult<()> {
        left.assert_valid();
        right.assert_valid();

        let l = dump_type_as_string(&self.cm, &left);
        let r = dump_type_as_string(&self.cm, &right);

        if data
            .dejavu
            .iter()
            .any(|(prev_l, prev_r)| prev_l.type_eq(left) && prev_r.type_eq(&right))
        {
            slog::info!(self.logger, "[assign/dejavu] {} = {}\n{:?} ", l, r, opts);
            return Ok(());
        }
        let _stack = stack::track(opts.span)?;

        data.dejavu.push((left.clone(), right.clone()));

        let res = self.assign_without_wrapping(data, left, right, opts).with_context(|| {
            //
            let l = dump_type_as_string(&self.cm, &left);
            let r = dump_type_as_string(&self.cm, &right);

            format!("\nlhs = {}\nrhs = {}", l, r)
        });

        let dejavu = data.dejavu.pop();
        debug_assert!(dejavu.is_some());

        slog::debug!(self.logger, "[assign ({:?})] {} = {}\n{:?} ", res.is_ok(), l, r, opts);

        res
    }

    /// Assigns, but does not wrap error with [Error::AssignFailed].
    fn assign_without_wrapping(
        &mut self,
        data: &mut AssignData,
        to: &Type,
        rhs: &Type,
        opts: AssignOpts,
    ) -> ValidationResult<()> {
        let span = opts.span;

        if !self.is_builtin && span.is_dummy() {
            panic!("cannot assign with dummy span")
        }

        // It's valid to assign any to everything.
        if rhs.is_any() {
            return Ok(());
        }

        if opts.allow_unknown_type && rhs.is_unknown() {
            return Ok(());
        }

        // debug_assert!(!span.is_dummy(), "\n\t{:?}\n<-\n\t{:?}", to, rhs);
        let to = self.normalize_for_assign(span, to)?;
        let rhs = self.normalize_for_assign(span, rhs)?;

        let to = to.normalize();
        let rhs = rhs.normalize();

        macro_rules! fail {
            () => {{
                return Err(Error::AssignFailed {
                    span,
                    left: box to.clone(),
                    right: box rhs.clone(),
                    right_ident: opts.right_ident_span,
                    cause: vec![],
                });
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
                        if left_enum.id.sym == *e.id.sym {
                            return Ok(());
                        }
                        fail!()
                    }
                    _ => {}
                }

                if !e.has_str && !e.has_num {
                    return self.assign_inner(
                        data,
                        to,
                        &Type::Keyword(RTsKeywordType {
                            span,
                            kind: TsKeywordTypeKind::TsNumberKeyword,
                        }),
                        opts,
                    );
                }

                if !e.has_num {
                    return self.assign_inner(
                        data,
                        to,
                        &Type::Keyword(RTsKeywordType {
                            span,
                            kind: TsKeywordTypeKind::TsStringKeyword,
                        }),
                        opts,
                    );
                }

                if !e.has_str {
                    return self.assign_inner(
                        data,
                        to,
                        &Type::Keyword(RTsKeywordType {
                            span,
                            kind: TsKeywordTypeKind::TsNumberKeyword,
                        }),
                        opts,
                    );
                }

                return self.assign_inner(
                    data,
                    to,
                    &Type::union(vec![
                        Type::Keyword(RTsKeywordType {
                            span,
                            kind: TsKeywordTypeKind::TsNumberKeyword,
                        }),
                        Type::Keyword(RTsKeywordType {
                            span,
                            kind: TsKeywordTypeKind::TsStringKeyword,
                        }),
                    ]),
                    opts,
                );
            }};
        }

        if to.type_eq(rhs) {
            return Ok(());
        }

        if let Some(res) = self.assign_to_builtins(data, opts, &to, &rhs) {
            return res;
        }

        if rhs.is_kwd(TsKeywordTypeKind::TsNeverKeyword) {
            return Ok(());
        }

        match rhs {
            Type::IndexedAccessType(rhs) => {
                let err = Error::NoSuchProperty {
                    span,
                    obj: Some(rhs.obj_type.clone()),
                    // TODO
                    prop: None,
                };
                return Err(Error::Errors {
                    span,
                    errors: vec![err],
                });
            }
            _ => {}
        }

        match to {
            Type::Ref(Ref {
                type_name:
                    RTsEntityName::Ident(RIdent {
                        sym: js_word!("Symbol"),
                        ..
                    }),
                ..
            }) => {
                if rhs.is_kwd(TsKeywordTypeKind::TsSymbolKeyword) {
                    return Ok(());
                }
            }

            // Str contains `kind`, and it's not handled properly by type_eq.
            Type::Lit(RTsLitType {
                lit: RTsLit::Str(to), ..
            }) => match rhs {
                Type::Lit(RTsLitType {
                    lit: RTsLit::Str(rhs), ..
                }) => {
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
                match rhs {
                    Type::Ref(right) => {
                        // We need this as type may recurse, and thus cannot be handled by expander.
                        if left.type_name.type_eq(&right.type_name) && left.type_args.type_eq(&right.type_args) {
                            return Ok(());
                        }
                    }
                    _ => {}
                }

                let mut new_lhs = self.expand_top_ref(span, Cow::Borrowed(to))?.into_owned();
                // self.replace(&mut new_lhs, &[(to, &Type::any(span))]);

                return self
                    .assign_inner(data, &new_lhs, rhs, opts)
                    .context("tried to assign a type created from a reference");
            }

            _ => {}
        }

        if to.is_str() || to.is_num() {
            if rhs.is_type_lit() {
                fail!()
            }
        }

        // Allow v = null and v = undefined if strict null check is false
        if !self.rule().strict_null_checks {
            match rhs {
                Type::Keyword(RTsKeywordType {
                    kind: TsKeywordTypeKind::TsNullKeyword,
                    ..
                })
                | Type::Keyword(RTsKeywordType {
                    kind: TsKeywordTypeKind::TsUndefinedKeyword,
                    ..
                }) => return Ok(()),
                _ => {}
            }
        }

        {
            // Handle special cases.
            // Assigning boolean to Boolean is ok, but assigning Boolean to boolean is an
            // error.
            let special_cases = &[
                (TsKeywordTypeKind::TsBooleanKeyword, "Boolean"),
                (TsKeywordTypeKind::TsStringKeyword, "String"),
                (TsKeywordTypeKind::TsNumberKeyword, "Number"),
            ];

            for (kwd, interface) in special_cases {
                let rhs = rhs.clone().generalize_lit();
                match to {
                    Type::Keyword(k) if k.kind == *kwd => match rhs {
                        Type::Interface(ref i) => {
                            if i.name.as_str() == *interface {
                                return Err(Error::AssignedWrapperToPrimitive { span });
                            }
                        }
                        _ => {}
                    },
                    Type::Interface(ref i) if i.name.as_str() == *interface => match rhs {
                        Type::Keyword(ref k) if k.kind == *kwd => return Ok(()),
                        _ => {}
                    },
                    _ => {}
                }
            }
        }

        match rhs {
            Type::Conditional(rhs) => {
                self.assign_with_opts(data, opts, to, &rhs.true_type)
                    .context("tried to assign the true type of a conditional type to lhs")?;
                self.assign_with_opts(data, opts, to, &rhs.false_type)
                    .context("tried to assign the false type of a conditional type to lhs")?;

                return Ok(());
            }
            _ => {}
        }

        match to {
            // let a: any = 'foo'
            Type::Keyword(RTsKeywordType {
                kind: TsKeywordTypeKind::TsAnyKeyword,
                ..
            }) => return Ok(()),

            Type::Keyword(RTsKeywordType {
                kind: TsKeywordTypeKind::TsUndefinedKeyword,
                ..
            }) => fail!(),

            // Anything is assignable to unknown
            Type::Keyword(RTsKeywordType {
                kind: TsKeywordTypeKind::TsUnknownKeyword,
                ..
            }) => return Ok(()),

            // Everything is assignable to Object
            Type::Interface(ref i) if i.name.as_str() == "Object" => return Ok(()),

            Type::Module(to) => {
                match rhs {
                    // TODO: Use unique id for module type.
                    Type::Module(rhs) => {
                        if to.name.eq_ignore_span(&rhs.name) {
                            return Ok(());
                        }
                    }
                    _ => {}
                }
                dbg!();
                return Err(Error::InvalidLValue { span: to.span() });
            }
            Type::Enum(ref e) => {
                match rhs.normalize() {
                    Type::Lit(RTsLitType {
                        lit: RTsLit::Number(..),
                        ..
                    })
                    | Type::Keyword(RTsKeywordType {
                        kind: TsKeywordTypeKind::TsNumberKeyword,
                        ..
                    }) => {
                        // validEnumAssignments.ts insists that this is valid.
                        return Ok(());
                    }
                    Type::EnumVariant(rhs) => {
                        if rhs.enum_name == e.id {
                            return Ok(());
                        }
                        fail!()
                    }
                    _ => {}
                }
            }
            Type::EnumVariant(ref e) => {
                dbg!();
                return Err(Error::InvalidLValue { span: e.span });
            }

            Type::Intersection(ref i) => {
                let mut errors = vec![];

                // TODO: Optimize unknown rhs handling

                for ty in &i.types {
                    match self
                        .assign_with_opts(
                            data,
                            AssignOpts {
                                allow_unknown_rhs: true,
                                ..opts
                            },
                            &ty,
                            rhs,
                        )
                        .context("tried to assign to an element of an intersection type")
                        .convert_err(|err| Error::SimpleAssignFailed { span: err.span() })
                    {
                        Ok(..) => {}
                        Err(err) => errors.push(err),
                    }
                }

                let left_contains_object = i.types.iter().any(|ty| ty.is_kwd(TsKeywordTypeKind::TsObjectKeyword));

                if !left_contains_object && !opts.allow_unknown_rhs {
                    let lhs = self.type_to_type_lit(span, to)?;
                    if let Some(lhs) = lhs {
                        self.assign_to_type_elements(data, opts, lhs.span, &lhs.members, &rhs, lhs.metadata)
                            .with_context(|| {
                                format!(
                                    "tried to check if unknown rhs exists while assigning to an intersection \
                                     type:\nLHS: {}",
                                    dump_type_as_string(&self.cm, &Type::TypeLit(lhs.into_owned()))
                                )
                            })
                            .convert_err(|err| Error::SimpleAssignFailed { span: err.span() })?;

                        errors.retain(|err| match err.actual() {
                            Error::UnknownPropertyInObjectLiteralAssignment { .. } => false,
                            _ => true,
                        });
                    }
                }

                if errors.is_empty() {
                    return Ok(());
                }

                return Err(Error::Errors { span, errors });
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
                        .assign_to_class(data, opts, l, rhs)
                        .context("tried to assign a type to an instance of a class")
                }
                _ => {}
            },
            Type::ClassDef(l) => {
                return self
                    .assign_to_class_def(data, opts, l, rhs)
                    .context("tried to assign a type to a class definition")
            }

            Type::Lit(ref lhs) => match rhs.normalize() {
                Type::Lit(rhs) if lhs.eq_ignore_span(&rhs) => return Ok(()),
                Type::Ref(..) => {
                    // We should expand ref. We expand it with the match
                    // expression below.
                }
                _ => fail!(),
            },

            Type::Query(ref to) => return self.assign_to_query_type(data, opts, to, &rhs),

            Type::Operator(Operator {
                op: TsTypeOperatorOp::ReadOnly,
                ty,
                ..
            }) => {
                return self
                    .assign_with_opts(data, opts, &ty, rhs)
                    .context("tried to assign a type to an operand of readonly type")
            }

            _ => {}
        }

        if opts.allow_assignment_of_void {
            if rhs.is_kwd(TsKeywordTypeKind::TsVoidKeyword) {
                return Ok(());
            }
        }

        match rhs {
            Type::Ref(..) => {
                let mut new_rhs = self.expand_top_ref(span, Cow::Borrowed(rhs))?.into_owned();
                // self.replace(&mut new_rhs, &[(rhs, &Type::any(span))]);
                return self
                    .assign_inner(data, to, &new_rhs, opts)
                    .context("tried to assign a type expanded from a reference to another type");
            }

            Type::Query(rhs) => {
                return self
                    .assign_from_query_type(data, opts, to, &rhs)
                    .context("tried to assign a query type to another type")
            }

            Type::Infer(..) => fail!(),

            // When strict null check is disabled, we can assign null / undefined to many things.
            Type::Keyword(RTsKeywordType {
                kind: TsKeywordTypeKind::TsUndefinedKeyword,
                ..
            })
            | Type::Keyword(RTsKeywordType {
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
                let errors = types
                    .iter()
                    .map(|rhs| self.assign_inner(data, to, rhs, opts))
                    .collect::<Vec<_>>();
                if errors.iter().any(Result::is_ok) {
                    return Ok(());
                }
                return Err(Error::Errors {
                    span,
                    errors: errors.into_iter().map(Result::unwrap_err).collect(),
                });
            }

            Type::Union(Union { ref types, .. }) => {
                if self.should_use_union_assignment(span, rhs)? {
                    types
                        .iter()
                        .map(|rhs| {
                            self.assign_with_opts(
                                data,
                                AssignOpts {
                                    allow_unknown_rhs: true,
                                    ..opts
                                },
                                to,
                                rhs,
                            )
                        })
                        .collect::<Result<_, _>>()
                        .context("tried to assign an union type to another one")?;

                    return Ok(());
                }

                let errors = types
                    .iter()
                    .filter_map(|rhs| match self.assign_with_opts(data, opts, to, rhs) {
                        Ok(()) => None,
                        Err(err) => Some(err),
                    })
                    .collect::<Vec<_>>();
                if errors.is_empty() {
                    return Ok(());
                }
                return Err(Error::Errors { span, errors }.context("tried to assign a union to other type"));
            }

            Type::Keyword(RTsKeywordType {
                kind: TsKeywordTypeKind::TsAnyKeyword,
                ..
            }) => return Ok(()),

            // Handle unknown on rhs
            Type::Keyword(RTsKeywordType {
                kind: TsKeywordTypeKind::TsUnknownKeyword,
                ..
            }) => {
                if to.is_kwd(TsKeywordTypeKind::TsAnyKeyword) || to.is_kwd(TsKeywordTypeKind::TsUndefinedKeyword) {
                    return Ok(());
                }

                fail!();
            }

            Type::Param(TypeParam {
                ref name,
                ref constraint,
                ..
            }) => {
                //
                match to {
                    Type::Param(TypeParam { name: ref l_name, .. }) => {
                        if name == l_name {
                            return Ok(());
                        }
                        match constraint.as_deref() {
                            Some(constraint) if constraint.normalize().is_type_param() => {}
                            _ => {
                                fail!()
                            }
                        }
                    }

                    _ => {}
                }

                match *constraint {
                    Some(ref c) => {
                        return self.assign_inner(
                            data,
                            to,
                            c,
                            AssignOpts {
                                allow_unknown_rhs: true,
                                ..opts
                            },
                        );
                    }
                    None => match to.normalize() {
                        Type::TypeLit(TypeLit { ref members, .. }) if members.is_empty() => return Ok(()),
                        _ => {}
                    },
                }

                fail!()
            }

            Type::Predicate(..) => match rhs {
                Type::Keyword(RTsKeywordType {
                    kind: TsKeywordTypeKind::TsBooleanKeyword,
                    ..
                }) => return Ok(()),
                _ => {}
            },

            _ => {}
        }

        match to {
            Type::Mapped(to) => return self.assign_to_mapped(data, opts, to, rhs),
            Type::Param(TypeParam {
                constraint: Some(ref c),
                ..
            }) => {
                return self.assign_inner(
                    data,
                    c,
                    rhs,
                    AssignOpts {
                        allow_assignment_to_param: true,
                        ..opts
                    },
                )
            }

            Type::Param(..) => {
                // We handled equality above.
                //
                // This is optional so we can change behavior while selecting method to call.
                // While selecting method, we may need to assign to a type parameter.

                if opts.allow_assignment_to_param {
                    return Ok(());
                } else {
                    fail!()
                }
            }

            Type::Array(Array { ref elem_type, .. }) => match rhs {
                Type::Array(Array {
                    elem_type: ref rhs_elem_type,
                    ..
                }) => {
                    return self.assign_inner(data, &elem_type, &rhs_elem_type, opts);
                }

                Type::Tuple(Tuple { ref elems, .. }) => {
                    let mut errors = vec![];
                    for el in elems {
                        errors.extend(self.assign_inner(data, elem_type, &el.ty, opts).err());
                    }
                    if !errors.is_empty() {
                        Err(Error::Errors { span, errors })?;
                    }

                    return Ok(());
                }

                _ => {
                    let r = self.type_to_type_lit(span, &rhs)?;
                    if let Some(r) = r {
                        for m in &r.members {
                            match m {
                                TypeElement::Index(m) => match m.params[0].ty.normalize() {
                                    Type::Keyword(RTsKeywordType {
                                        span,
                                        kind: TsKeywordTypeKind::TsNumberKeyword,
                                        ..
                                    }) => {
                                        if let Some(type_ann) = &m.type_ann {
                                            return self.assign_with_opts(data, opts, elem_type, type_ann);
                                        }
                                    }
                                    _ => {}
                                },
                                _ => {}
                            }
                        }
                    }

                    // Try to assign by converting rhs to an iterable.
                    if opts.allow_iterable_on_rhs {
                        let res: ValidationResult<_> = try {
                            let r = self
                                .get_iterator(span, Cow::Borrowed(&rhs), Default::default())
                                .context("tried to convert a type to an iterator to assign to a tuple")?;
                            //
                            let rhs_el = self
                                .get_iterator_element_type(span, r, false)
                                .context("tried to get the element type of an iterator assignment")?;

                            self.assign_with_opts(
                                data,
                                AssignOpts {
                                    allow_iterable_on_rhs: false,
                                    ..opts
                                },
                                elem_type,
                                &rhs_el,
                            )?;
                        };

                        match res {
                            Ok(_) => return Ok(()),
                            Err(_) => {
                                // TODO: Log?
                            }
                        }
                    }

                    fail!()
                }
            },

            // let a: string | number = 'string';
            Type::Union(Union { ref types, .. }) => {
                // true | false = boolean
                if rhs.is_kwd(TsKeywordTypeKind::TsBooleanKeyword) {
                    if types.iter().any(|ty| match ty.normalize() {
                        Type::Lit(RTsLitType {
                            lit: RTsLit::Bool(RBool { value: true, .. }),
                            ..
                        }) => true,
                        _ => false,
                    }) && types.iter().any(|ty| match ty.normalize() {
                        Type::Lit(RTsLitType {
                            lit: RTsLit::Bool(RBool { value: false, .. }),
                            ..
                        }) => true,
                        _ => false,
                    }) {
                        return Ok(());
                    }
                }

                let results = types
                    .iter()
                    .map(|to| {
                        self.assign_with_opts(
                            data,
                            AssignOpts {
                                allow_unknown_rhs: true,
                                ..opts
                            },
                            &to,
                            rhs,
                        )
                        .context("tried to assign a type to a union")
                    })
                    .collect::<Vec<_>>();
                if results.iter().any(Result::is_ok) {
                    return Ok(());
                }
                let normalized = types.iter().map(|ty| ty.normalize()).any(|ty| match ty {
                    Type::TypeLit(ty) => ty.metadata.normalized,
                    _ => false,
                });
                let errors = results.into_iter().map(Result::unwrap_err).collect();
                let should_use_single_error = normalized || types.iter().all(|ty| ty.normalize().is_lit());

                if should_use_single_error {
                    return Err(Error::AssignFailed {
                        span,
                        cause: errors,
                        left: box to.clone(),
                        right: box rhs.clone(),
                        right_ident: opts.right_ident_span,
                    });
                } else {
                    return Err(Error::Errors { span, errors }.context("tried to a type to a union type"));
                }
            }

            Type::Intersection(Intersection { ref types, .. }) => {
                let vs = types
                    .iter()
                    .map(|to| self.assign_inner(data, &to, rhs, opts))
                    .collect::<Vec<_>>();

                // TODO: Multiple error
                for v in vs {
                    if let Err(error) = v {
                        return Err(Error::IntersectionError { span, error: box error });
                    }
                }

                return Ok(());
            }

            // Handle same keyword type.
            Type::Keyword(RTsKeywordType { kind, .. }) => {
                match *rhs {
                    Type::Keyword(RTsKeywordType { kind: rhs_kind, .. }) => {
                        if rhs_kind == *kind {
                            return Ok(());
                        }

                        if rhs_kind == TsKeywordTypeKind::TsUndefinedKeyword
                            && *kind == TsKeywordTypeKind::TsVoidKeyword
                        {
                            return Ok(());
                        }

                        if *kind == TsKeywordTypeKind::TsUndefinedKeyword
                            && rhs_kind == TsKeywordTypeKind::TsVoidKeyword
                        {
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

                    Type::Array(..) | Type::Tuple(..) | Type::Class(..) | Type::ClassDef(..) => fail!(),

                    _ => {}
                }

                match kind {
                    TsKeywordTypeKind::TsStringKeyword => match *rhs {
                        Type::Lit(RTsLitType {
                            lit: RTsLit::Str(..), ..
                        }) => return Ok(()),
                        _ => {}
                    },

                    TsKeywordTypeKind::TsNumberKeyword => match *rhs {
                        Type::Lit(RTsLitType {
                            lit: RTsLit::Number(..),
                            ..
                        }) => return Ok(()),

                        Type::EnumVariant(ref v) => {
                            // Allow assigning enum with numeric values to
                            // number.
                            if let Some(types) = self.find_type(v.ctxt, &v.enum_name)? {
                                for ty in types {
                                    match *ty.normalize() {
                                        Type::Enum(ref e) => {
                                            let is_num = !e.has_str;
                                            if is_num {
                                                return Ok(());
                                            }
                                        }
                                        _ => {}
                                    }
                                }
                            }

                            fail!()
                        }
                        _ => {}
                    },

                    TsKeywordTypeKind::TsBooleanKeyword => match *rhs {
                        Type::Lit(RTsLitType {
                            lit: RTsLit::Bool(..), ..
                        }) => return Ok(()),
                        _ => {}
                    },

                    TsKeywordTypeKind::TsVoidKeyword | TsKeywordTypeKind::TsUndefinedKeyword => {
                        //

                        match rhs {
                            Type::Keyword(RTsKeywordType {
                                kind: TsKeywordTypeKind::TsVoidKeyword,
                                ..
                            }) => return Ok(()),
                            Type::Lit(..)
                            | Type::Keyword(..)
                            | Type::TypeLit(..)
                            | Type::Class(..)
                            | Type::ClassDef(..)
                            | Type::Interface(..)
                            | Type::Module(..)
                            | Type::EnumVariant(..) => fail!(),
                            Type::Function(..) => return Err(Error::CannotAssignToNonVariable { span: rhs.span() }),
                            _ => {}
                        }
                    }

                    TsKeywordTypeKind::TsSymbolKeyword => {
                        //

                        match *rhs.normalize() {
                            Type::Keyword(RTsKeywordType {
                                kind: TsKeywordTypeKind::TsSymbolKeyword,
                                ..
                            }) => return Ok(()),
                            _ => fail!(),
                        }
                    }

                    TsKeywordTypeKind::TsObjectKeyword => {
                        match *rhs {
                            Type::Keyword(RTsKeywordType {
                                kind: TsKeywordTypeKind::TsNumberKeyword,
                                ..
                            })
                            | Type::Keyword(RTsKeywordType {
                                kind: TsKeywordTypeKind::TsStringKeyword,
                                ..
                            })
                            | Type::Keyword(RTsKeywordType {
                                kind: TsKeywordTypeKind::TsBooleanKeyword,
                                ..
                            })
                            | Type::Keyword(RTsKeywordType {
                                kind: TsKeywordTypeKind::TsBigIntKeyword,
                                ..
                            })
                            | Type::Keyword(RTsKeywordType {
                                kind: TsKeywordTypeKind::TsVoidKeyword,
                                ..
                            })
                            | Type::Keyword(RTsKeywordType {
                                kind: TsKeywordTypeKind::TsNullKeyword,
                                ..
                            })
                            | Type::Keyword(RTsKeywordType {
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
                        Type::Lit(..)
                        | Type::Interface(..)
                        | Type::TypeLit(..)
                        | Type::Function(..)
                        | Type::Constructor(..) => fail!(),
                        _ => {}
                    },
                    _ => {}
                }
            }

            Type::Enum(ref e) => {
                //
                match *rhs {
                    Type::EnumVariant(ref r) => {
                        if r.enum_name == e.id {
                            return Ok(());
                        }
                    }
                    Type::Lit(..)
                    | Type::TypeLit(..)
                    | Type::Keyword(RTsKeywordType {
                        kind: TsKeywordTypeKind::TsVoidKeyword,
                        ..
                    })
                    | Type::Keyword(RTsKeywordType {
                        kind: TsKeywordTypeKind::TsStringKeyword,
                        ..
                    })
                    | Type::Keyword(RTsKeywordType {
                        kind: TsKeywordTypeKind::TsNumberKeyword,
                        ..
                    })
                    | Type::Keyword(RTsKeywordType {
                        kind: TsKeywordTypeKind::TsBooleanKeyword,
                        ..
                    }) => fail!(),
                    _ => {}
                }
            }

            Type::EnumVariant(ref l) => match *rhs {
                Type::EnumVariant(ref r) => {
                    if l.enum_name == r.enum_name && l.name == r.name {
                        return Ok(());
                    }
                }
                Type::Lit(..)
                | Type::TypeLit(..)
                | Type::Keyword(RTsKeywordType {
                    kind: TsKeywordTypeKind::TsStringKeyword,
                    ..
                })
                | Type::Keyword(RTsKeywordType {
                    kind: TsKeywordTypeKind::TsNumberKeyword,
                    ..
                })
                | Type::Keyword(RTsKeywordType {
                    kind: TsKeywordTypeKind::TsBooleanKeyword,
                    ..
                }) => {
                    fail!()
                }
                _ => {}
            },

            Type::This(RTsThisType { span }) => return Err(Error::CannotAssingToThis { span: *span }),

            Type::Interface(Interface {
                ref body, ref extends, ..
            }) => {
                // TODO: Optimize handling of unknown rhs

                self.assign_to_type_elements(
                    data,
                    AssignOpts {
                        allow_unknown_rhs: true,
                        allow_assignment_of_array_to_optional_type_lit: true,
                        ..opts
                    },
                    span,
                    &body,
                    rhs,
                    Default::default(),
                )
                .context("tried to assign a type to an interface")?;

                let mut errors = vec![];
                for parent in extends {
                    let parent = self.type_of_ts_entity_name(
                        span,
                        self.ctx.module_id,
                        &parent.expr,
                        parent.type_args.as_deref(),
                    )?;

                    // An interface can extend a class.
                    let parent = self.instantiate_class(span, &parent)?;

                    let res = self.assign_with_opts(
                        data,
                        AssignOpts {
                            allow_unknown_rhs: true,
                            ..opts
                        },
                        &parent,
                        &rhs,
                    );
                    if res.is_ok() {
                        slog::debug!(
                            self.logger,
                            "[assign] Parent assign successful: {} = {}",
                            dump_type_as_string(&self.cm, &parent),
                            dump_type_as_string(&self.cm, &rhs),
                        );
                        return Ok(());
                    }

                    errors.extend(res.err());
                }

                // TODO: Prevent recursion and uncomment the code below.
                //
                // // We try assigning as builtin interfaces.
                // match rhs {
                //     Type::Keyword(RTsKeywordType {
                //         kind: TsKeywordTypeKind::TsStringKeyword,
                //         ..
                //     })
                //     | Type::Lit(RTsLitType {
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
                //             .context("tried to assign by converting rhs to builtin inferface
                // 'String'")     }
                //     _ => {}
                // }

                // Assignment failed. This check is required to distinguish an empty interface
                // from an interface with parents.
                //
                // TODO: Use errors returned from parent assignment.
                if body.is_empty() && !extends.is_empty() {
                    return Err(Error::AssignFailed {
                        span,
                        left: box to.clone(),
                        right: box rhs.clone(),
                        right_ident: opts.right_ident_span,
                        cause: errors,
                    });
                }

                // We should check for unknown rhs, while allowing assignment to parent
                // interfaces.
                if !opts.allow_unknown_rhs {
                    let lhs = self.type_to_type_lit(span, to)?;
                    if let Some(lhs) = lhs {
                        self.assign_to_type_elements(data, opts, span, &lhs.members, rhs, Default::default())
                            .with_context(|| {
                                format!(
                                    "tried to assign a type to an interface to check if unknown rhs exists\nLHS: {}",
                                    dump_type_as_string(&self.cm, &Type::TypeLit(lhs.into_owned()))
                                )
                            })?;
                    }
                }

                if !errors.is_empty() {
                    return Err(Error::AssignFailed {
                        span,
                        left: box to.clone(),
                        right: box rhs.clone(),
                        right_ident: opts.right_ident_span,
                        cause: errors,
                    });
                }

                return Ok(());
            }

            Type::TypeLit(TypeLit {
                ref members, metadata, ..
            }) => {
                return self
                    .assign_to_type_elements(data, opts, span, &members, rhs, *metadata)
                    .context("tried to assign a type to type elements");
            }

            Type::Lit(RTsLitType { ref lit, .. }) => match *rhs {
                Type::Lit(RTsLitType { lit: ref r_lit, .. }) => {
                    if lit.eq_ignore_span(r_lit) {
                        return Ok(());
                    }

                    // Extra check to handle "has_escape"
                    match (lit, r_lit) {
                        (&RTsLit::Str(ref l), &RTsLit::Str(ref r)) if l.value == r.value => return Ok(()),
                        _ => {}
                    }

                    fail!()
                }

                Type::Ref(..) => {
                    // We use reference handler below.
                }

                // TODO: allow
                // let a: true | false = bool
                _ => fail!(),
            },

            Type::Function(lf) => match rhs {
                Type::Function(..) | Type::Lit(..) | Type::TypeLit(..) | Type::Interface(..) => {
                    return self
                        .assign_to_function(data, opts, to, lf, rhs)
                        .context("tried to assign to a function type")
                }
                Type::Keyword(RTsKeywordType {
                    kind: TsKeywordTypeKind::TsVoidKeyword,
                    ..
                })
                | Type::Keyword(RTsKeywordType {
                    kind: TsKeywordTypeKind::TsNumberKeyword,
                    ..
                })
                | Type::Keyword(RTsKeywordType {
                    kind: TsKeywordTypeKind::TsStringKeyword,
                    ..
                })
                | Type::Keyword(RTsKeywordType {
                    kind: TsKeywordTypeKind::TsBigIntKeyword,
                    ..
                })
                | Type::Keyword(RTsKeywordType {
                    kind: TsKeywordTypeKind::TsBooleanKeyword,
                    ..
                })
                | Type::Constructor(..) => {
                    fail!()
                }
                _ => {}
            },

            Type::Tuple(Tuple { ref elems, .. }) => {
                if elems.is_empty() {
                    match rhs {
                        Type::Array(..) | Type::Tuple(..) => return Ok(()),
                        _ => {}
                    }
                }
                //
                match *rhs.normalize() {
                    Type::Tuple(Tuple {
                        elems: ref rhs_elems, ..
                    }) => {
                        if elems.len() < rhs_elems.len() {
                            fail!();
                        }
                        if !elems.is_empty() && rhs_elems.is_empty() {
                            fail!();
                        }

                        let mut errors = vec![];
                        for (l, r) in elems.into_iter().zip(rhs_elems) {
                            for el in elems {
                                match *r.ty.normalize() {
                                    Type::Keyword(RTsKeywordType {
                                        kind: TsKeywordTypeKind::TsUndefinedKeyword,
                                        ..
                                    }) => continue,
                                    _ => {}
                                }

                                errors.extend(self.assign_inner(data, &l.ty, &r.ty, opts).err());
                            }
                        }

                        if !errors.is_empty() {
                            return Err(Error::TupleAssignError { span, errors });
                        }

                        return Ok(());
                    }
                    Type::Lit(..)
                    | Type::Interface(..)
                    | Type::TypeLit(..)
                    | Type::Keyword(..)
                    | Type::Array(..)
                    | Type::Class(..)
                    | Type::ClassDef(..)
                        if !opts.allow_iterable_on_rhs =>
                    {
                        fail!()
                    }

                    _ => {
                        // Try to assign by converting rhs to an iterable.
                        if opts.allow_iterable_on_rhs {
                            let r = self
                                .get_iterator(span, Cow::Borrowed(&rhs), Default::default())
                                .context("tried to convert a type to an iterator to assign to a tuple")?;
                            //
                            for (i, elem) in elems.iter().enumerate() {
                                let r_ty = self
                                    .get_element_from_iterator(span, Cow::Borrowed(&r), i)
                                    .context("tried to get an element of type to assign to a tuple element")?;

                                self.assign_with_opts(
                                    data,
                                    AssignOpts {
                                        allow_iterable_on_rhs: false,
                                        ..opts
                                    },
                                    &elem.ty,
                                    &r_ty,
                                )?;
                            }

                            return Ok(());
                        }
                    }
                }
            }

            Type::Constructor(ref lc) => {
                return self
                    .assign_to_constructor(data, opts, to, &lc, rhs)
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

            Type::EnumVariant(EnumVariant {
                ref ctxt,
                ref enum_name,
                ..
            }) => {
                if let Some(types) = self.find_type(*ctxt, enum_name)? {
                    for ty in types {
                        if let Type::Enum(ref e) = ty.normalize() {
                            match to {
                                Type::Interface(..) | Type::TypeLit(..) => {}
                                _ => {
                                    handle_enum_in_rhs!(e)
                                }
                            }
                        }
                    }
                }

                fail!()
            }

            _ => {}
        }

        match to {
            // Handle symbol assignments
            Type::Operator(Operator {
                op: TsTypeOperatorOp::Unique,
                ty,
                ..
            }) if ty.is_kwd(TsKeywordTypeKind::TsSymbolKeyword) => {
                if rhs.is_symbol() {
                    return Ok(());
                }
            }

            Type::Predicate(..) => {
                if rhs.is_kwd(TsKeywordTypeKind::TsBooleanKeyword) {
                    return Ok(());
                }
            }

            Type::Operator(Operator {
                op: TsTypeOperatorOp::KeyOf,
                ty,
                ..
            }) if ty.normalize().is_type_param() => {
                return self
                    .assign_with_opts(
                        data,
                        opts,
                        &Type::Keyword(RTsKeywordType {
                            span: DUMMY_SP,
                            kind: TsKeywordTypeKind::TsStringKeyword,
                        }),
                        rhs,
                    )
                    .context("tried to assign a type to a `keyof TypeParam`")
            }

            _ => {}
        }

        if to.is_symbol()
            || to.is_kwd(TsKeywordTypeKind::TsNeverKeyword)
            || rhs.is_kwd(TsKeywordTypeKind::TsVoidKeyword)
        {
            fail!()
        }

        match (to, rhs) {
            (Type::Tpl(l), r) => {
                return self
                    .assign_to_tpl(l, r, opts)
                    .context("tried to assign to a template type")
            }
            _ => {}
        }

        // TODO: Implement full type checker
        slog::error!(self.logger, "unimplemented: assign: \nLeft: {:?}\nRight: {:?}", to, rhs);
        Ok(())
    }

    #[context("tried to extract keys")]
    fn extract_keys(&mut self, span: Span, ty: &Type) -> ValidationResult {
        let ty = self.normalize(
            Some(span),
            Cow::Borrowed(&ty),
            NormalizeTypeOpts {
                normalize_keywords: true,
                ..Default::default()
            },
        )?;
        let ty = ty.normalize();

        match ty {
            Type::TypeLit(ty) => {
                //
                let mut keys = vec![];
                for member in &ty.members {
                    match member {
                        TypeElement::Property(PropertySignature {
                            span,
                            key: Key::Normal { sym: key, .. },
                            ..
                        }) => {
                            keys.push(Type::Lit(RTsLitType {
                                node_id: NodeId::invalid(),
                                span: *span,
                                lit: RTsLit::Str(RStr {
                                    span: *span,
                                    has_escape: false,
                                    kind: Default::default(),
                                    value: key.clone(),
                                }),
                            }));
                        }
                        _ => {}
                    }
                }

                return Ok(Type::union(keys));
            }
            _ => {}
        }

        if let Some(ty) = self
            .type_to_type_lit(span, &ty)?
            .map(Cow::into_owned)
            .map(Type::TypeLit)
        {
            return self.extract_keys(span, &ty);
        }

        Err(Error::Unimplemented {
            span,
            msg: format!("Extract keys"),
        })?
    }

    /// Handles `P in 'foo' | 'bar'`. Note that `'foo' | 'bar'` part should be
    /// passed as `keys`.
    ///
    ///
    /// Currently only literals and unions are supported for `keys`.
    fn assign_keys(
        &mut self,
        data: &mut AssignData,
        opts: AssignOpts,
        keys: &Type,
        rhs: &Type,
    ) -> ValidationResult<()> {
        let keys = keys.normalize();
        let rhs = rhs.normalize();

        let rhs_keys = self.extract_keys(opts.span, &rhs)?;

        self.assign_with_opts(data, opts, &keys, &rhs_keys)
    }

    fn assign_to_mapped(
        &mut self,
        data: &mut AssignData,
        opts: AssignOpts,
        to: &Mapped,
        rhs: &Type,
    ) -> ValidationResult<()> {
        let rhs = rhs.normalize();

        // Validate keys
        match &to.type_param.constraint {
            Some(constraint) => self.assign_keys(data, opts, &constraint, rhs)?,
            None => {}
        }

        let ty = match &to.ty {
            Some(v) => v.normalize(),
            None => return Ok(()),
        };

        match rhs {
            Type::TypeLit(rhs) => {
                //
                for member in &rhs.members {
                    match member {
                        TypeElement::Property(prop) => {
                            if let Some(prop_ty) = &prop.type_ann {
                                self.assign_with_opts(data, opts, &ty, &prop_ty)?;
                            }
                        }
                        _ => {
                            return Err(Error::Unimplemented {
                                span: opts.span,
                                msg: format!("Assignment to mapped type: type element - {:?}", member),
                            })
                        }
                    }
                }

                return Ok(());
            }
            _ => {}
        }

        Err(Error::Unimplemented {
            span: opts.span,
            msg: format!("Assignment to mapped type"),
        })
    }

    /// Returns true for `A | B | | C = A | B` and simillar cases.
    ///
    /// Should be called iff lhs is a union type.
    fn should_use_union_assignment(&mut self, span: Span, r: &Type) -> ValidationResult<bool> {
        match r.normalize() {
            Type::Union(..) => return Ok(true),
            Type::TypeLit(r) => {
                if r.members.iter().all(|el| match el {
                    TypeElement::Call(..) => true,
                    _ => false,
                }) {
                    return Ok(true);
                }

                if r.members.iter().all(|el| match el {
                    TypeElement::Constructor(..) => true,
                    _ => false,
                }) {
                    return Ok(true);
                }
            }
            _ => {}
        }

        Ok(false)
    }
}

/// Returns true if l and r are lieteral and equal to each other.
fn is_key_eq(l: &RExpr, r: &RExpr) -> bool {
    (match (l, r) {
        (&RExpr::Ident(..), &RExpr::Ident(..)) => true,
        (&RExpr::Member(..), &RExpr::Member(..)) => true,
        _ => false,
    }) && l.eq_ignore_span(r)
}

fn count_required_params(v: &[FnParam]) -> usize {
    v.iter().filter(|v| v.required).count()
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
