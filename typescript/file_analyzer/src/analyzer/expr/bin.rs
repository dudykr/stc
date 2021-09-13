use crate::{
    analyzer::{
        assign::AssignOpts,
        expr::{type_cast::CastableOpts, TypeOfMode},
        generic::ExtendsOpts,
        scope::ExpandOpts,
        util::{Comparator, ResultExt},
        Analyzer, Ctx, ScopeKind,
    },
    ty::{Operator, Type, TypeExt},
    type_facts::TypeFacts,
    util::{is_str_or_union, RemoveTypes},
    validator,
    validator::ValidateWith,
    ValidationResult,
};
use stc_ts_ast_rnode::{
    RBinExpr, RExpr, RExprOrSuper, RIdent, RLit, RMemberExpr, ROptChainExpr, RPat, RPatOrExpr, RStr, RTpl,
    RTsEntityName, RTsLit, RUnaryExpr,
};
use stc_ts_errors::{DebugExt, Error, Errors};
use stc_ts_file_analyzer_macros::extra_validator;
use stc_ts_type_ops::{generalization::prevent_generalize, is_str_lit_or_union, Fix};
use stc_ts_types::{
    name::Name, Class, IdCtx, Intersection, Key, KeywordType, KeywordTypeMetadata, LitType, ModuleId, Ref, TypeElement,
    Union, UnionMetadata,
};
use stc_utils::cache::Freeze;
use std::{
    borrow::Cow,
    collections::hash_map::Entry,
    convert::{TryFrom, TryInto},
};
use swc_atoms::js_word;
use swc_common::{Span, Spanned, SyntaxContext, TypeEq};
use swc_ecma_ast::{op, BinaryOp, TsKeywordTypeKind, TsTypeOperatorOp};
use swc_ecma_utils::Value::Known;
use tracing::info;

#[validator]
impl Analyzer<'_, '_> {
    fn validate(&mut self, e: &RBinExpr, type_ann: Option<&Type>) -> ValidationResult {
        let RBinExpr {
            span,
            op,
            ref left,
            ref right,
            ..
        } = *e;

        let marks = self.marks();
        let add_type_facts = self.ctx.in_cond;

        let prev_facts = self.cur_facts.clone();

        self.report_errors_for_mixed_nullish_coalescing(e);

        let mut errors = vec![];

        let ctx = Ctx {
            should_store_truthy_for_access: self.ctx.should_store_truthy_for_access
                && match op {
                    op!("&&") => true,
                    _ => false,
                },
            in_cond: self.ctx.in_cond
                || match op {
                    op!("&&") | op!("||") => true,
                    _ => false,
                },
            check_for_implicit_any: true,
            ..self.ctx
        };

        let child_ctxt = (
            TypeOfMode::RValue,
            None,
            match op {
                op!("??") | op!("||") => type_ann,
                _ => None,
            },
        );

        let mut lt = {
            let mut a = self.with_ctx(ctx);
            left.validate_with_args(&mut *a, child_ctxt)
        }
        .and_then(|mut ty| {
            if ty.is_ref_type() {
                let ctx = Ctx {
                    preserve_ref: false,
                    ignore_expand_prevention_for_top: true,
                    ..self.ctx
                };
                ty = self.with_ctx(ctx).expand(
                    span,
                    ty,
                    ExpandOpts {
                        full: true,
                        expand_union: true,
                        ..Default::default()
                    },
                )?;
            }
            let span = ty.span();
            ty.reposition(left.span());

            Ok(ty)
        })
        .store(&mut errors);
        lt.make_clone_cheap();

        let true_facts_for_rhs = if op == op!("&&") {
            // We need a new virtual scope.
            self.cur_facts.true_facts.take()
        } else if op == op!("||") {
            self.cur_facts.false_facts.clone()
        } else {
            Default::default()
        };

        let mut additional_false_facts = if op == op!("&&") {
            self.cur_facts.false_facts.take()
        } else {
            Default::default()
        };

        let mut lhs_facts = if op == op!("||") {
            self.cur_facts.take()
        } else {
            Default::default()
        };

        self.cur_facts = prev_facts.clone();

        let rhs = self
            .with_child(
                ScopeKind::Flow,
                true_facts_for_rhs.clone(),
                |child: &mut Analyzer| -> ValidationResult<_> {
                    child.ctx.should_store_truthy_for_access = false;

                    let truthy_lt;
                    let child_ctxt = (
                        TypeOfMode::RValue,
                        None,
                        match op {
                            op!("??") | op!("&&") | op!("||") => match type_ann {
                                Some(ty) => Some(ty),
                                _ => match op {
                                    op!("||") | op!("??") => {
                                        truthy_lt = lt
                                            .clone()
                                            .map(|ty| child.apply_type_facts_to_type(TypeFacts::Truthy, ty));
                                        truthy_lt.as_ref()
                                    }
                                    _ => lt.as_ref(),
                                },
                            },
                            _ => None,
                        },
                    );

                    let ty = right.validate_with_args(child, child_ctxt).and_then(|mut ty| {
                        if ty.is_ref_type() {
                            let ctx = Ctx {
                                preserve_ref: false,
                                ignore_expand_prevention_for_top: true,
                                ..child.ctx
                            };
                            ty = child.with_ctx(ctx).expand(
                                span,
                                ty,
                                ExpandOpts {
                                    full: true,
                                    expand_union: true,
                                    ..Default::default()
                                },
                            )?;
                        }

                        let span = ty.span();
                        ty.reposition(right.span());

                        Ok(ty)
                    })?;

                    Ok(ty)
                },
            )
            .store(&mut errors);

        let rt = rhs;

        self.report_errors_for_bin_expr(
            span,
            op,
            &lt.as_ref().map(Cow::Borrowed).unwrap_or_else(|| {
                Cow::Owned(Type::any(
                    left.span().with_ctxt(SyntaxContext::empty()),
                    Default::default(),
                ))
            }),
            &rt.as_ref().map(Cow::Borrowed).unwrap_or_else(|| {
                Cow::Owned(Type::any(
                    left.span().with_ctxt(SyntaxContext::empty()),
                    Default::default(),
                ))
            }),
        );

        if add_type_facts {
            if op == op!("||") {
                for (k, type_fact) in lhs_facts.true_facts.facts.drain() {
                    match self.cur_facts.true_facts.facts.entry(k) {
                        // (typeof a === 'string' || typeof a === 'number')
                        Entry::Occupied(mut e) => {
                            *e.get_mut() &= type_fact;
                        }
                        // (typeof a === 'string' || a !== foo)
                        Entry::Vacant(..) => {}
                    }
                }

                self.cur_facts += lhs_facts;
            } else if op == op!("&&") {
                self.cur_facts.true_facts += true_facts_for_rhs;

                for (k, v) in additional_false_facts.facts.drain() {
                    *self
                        .cur_facts
                        .false_facts
                        .facts
                        .entry(k.clone())
                        .or_insert(TypeFacts::None) &= v;
                }
            }

            self.cur_facts.false_facts += additional_false_facts;
        } else {
            self.cur_facts = prev_facts;
        }

        let (mut lt, mut rt): (Type, Type) = match (lt, rt) {
            (Some(l), Some(r)) => (l, r),
            _ => return Err(Error::Errors { span, errors }),
        };
        lt.make_clone_cheap();
        rt.make_clone_cheap();

        if !self.is_builtin {
            debug_assert!(!lt.span().is_dummy());

            debug_assert!(!rt.span().is_dummy());
        }

        // Handle control-flow based typing
        match op {
            op!("===") | op!("!==") | op!("==") | op!("!=") => {
                let is_eq = op == op!("===") || op == op!("==");

                let c = Comparator {
                    left: &**left,
                    right: &**right,
                };

                self.add_type_facts_for_typeof(span, &left, &right, is_eq)
                    .report(&mut self.storage);

                // Try narrowing type
                let c = Comparator {
                    left: (&**left, lt.normalize()),
                    right: (&**right, rt.normalize()),
                };

                if !self.is_valid_for_switch_case(span, &lt, &rt)? {
                    if self.ctx.in_switch_case_test {
                        self.storage.report(Error::SwitchCaseTestNotCompatible { span })
                    } else {
                        self.storage.report(Error::NoOverlap {
                            span,
                            value: true,
                            left: lt.span(),
                            right: rt.span(),
                        })
                    }
                }

                match c.take_if_any_matches(|(l, l_ty), (_, r_ty)| match *l_ty {
                    Type::Keyword(KeywordType {
                        kind: TsKeywordTypeKind::TsUnknownKeyword,
                        ..
                    }) => {
                        //
                        Some((Name::try_from(l), r_ty))
                    }
                    _ => None,
                }) {
                    Some((Ok(name), ty)) => {
                        let ty = ty.clone().cheap();
                        if is_eq {
                            self.add_deep_type_fact(span, name.clone(), ty.clone(), false);
                        } else {
                            self.add_deep_type_fact(span, name.clone(), ty.clone(), true);
                        }
                    }
                    _ => {}
                }

                self.add_type_facts_for_opt_chains(span, &left, &right, &lt, &rt)
                    .report(&mut self.storage);

                match c.take_if_any_matches(|(l, _), (_, r_ty)| match (l, r_ty) {
                    (
                        RExpr::Ident(RIdent {
                            sym: js_word!("undefined"),
                            ..
                        }),
                        _,
                    )
                    | (RExpr::Lit(RLit::Null(..)), _) => None,

                    (l, r) => Some((extract_name_for_assignment(l, op == op!("==="))?, r_ty)),
                }) {
                    Some((l, r_ty)) => {
                        if self.ctx.in_cond {
                            let (name, mut r) = self.calc_type_facts_for_equality(l, r_ty)?;
                            prevent_generalize(&mut r);
                            r.make_cheap();

                            if op == op!("===") {
                                self.cur_facts
                                    .false_facts
                                    .excludes
                                    .entry(name.clone())
                                    .or_default()
                                    .push(r.clone());

                                self.add_deep_type_fact(span, name, r, true);
                            } else if !is_eq {
                                // Remove from union
                                self.cur_facts
                                    .true_facts
                                    .excludes
                                    .entry(name.clone())
                                    .or_default()
                                    .push(r.clone());

                                self.add_deep_type_fact(span, name, r, false);
                            }
                        }
                    }
                    _ => {}
                }
            }

            op!("instanceof") => {
                match **left {
                    RExpr::Ident(ref i) => {
                        // typeGuardsTypeParameters.ts says
                        //
                        // Type guards involving type parameters produce intersection types
                        let mut orig_ty = self.type_of_var(i, TypeOfMode::RValue, None)?;
                        orig_ty.make_clone_cheap();

                        //
                        let ty = self.validate_rhs_of_instanceof(span, &rt, rt.clone());

                        // typeGuardsWithInstanceOfByConstructorSignature.ts
                        //
                        // says
                        //
                        // `can't narrow type from 'any' to 'Object'`
                        // `can't narrow type from 'any' to 'Function'
                        let cannot_narrow = orig_ty.is_any()
                            && match &**right {
                                RExpr::Ident(RIdent {
                                    sym: js_word!("Object"),
                                    ..
                                })
                                | RExpr::Ident(RIdent {
                                    sym: js_word!("Function"),
                                    ..
                                }) => true,

                                _ => false,
                            };

                        if self.ctx.in_cond && !cannot_narrow {
                            let narrowed_ty = self
                                .narrow_with_instanceof(span, Cow::Borrowed(&ty), &orig_ty)
                                .context("tried to narrow type with instanceof")?
                                .freezed();

                            narrowed_ty.assert_valid();

                            // TODO(kdy1): Maybe we need to check for intersection or union
                            if orig_ty.normalize().is_type_param() {
                                self.cur_facts.true_facts.vars.insert(
                                    Name::from(i),
                                    Type::Intersection(Intersection {
                                        span,
                                        types: vec![orig_ty, narrowed_ty],
                                        metadata: Default::default(),
                                    })
                                    .fixed()
                                    .freezed(),
                                );
                            } else {
                                self.cur_facts
                                    .true_facts
                                    .vars
                                    .insert(Name::from(i), narrowed_ty.clone());

                                self.cur_facts
                                    .false_facts
                                    .excludes
                                    .entry(Name::from(i))
                                    .or_default()
                                    .push(narrowed_ty);
                            }
                        }
                    }

                    _ => {}
                }
            }

            _ => {}
        }

        macro_rules! no_unknown {
            () => {{
                no_unknown!(lt);
                no_unknown!(rt);
            }};
            ($ty:expr) => {{
                match &$ty {
                    Type::Keyword(KeywordType {
                        kind: TsKeywordTypeKind::TsUnknownKeyword,
                        ..
                    }) => {
                        debug_assert!(!span.is_dummy());
                        return Err(Error::Unknown { span });
                    }
                    _ => {}
                }
            }};
        }

        match op {
            op!(bin, "+") => {
                no_unknown!();

                let c = Comparator {
                    left: (&**left, &lt),
                    right: (&**right, &rt),
                };

                if let Some(()) = c.take_if_any_matches(|(_, lt), (_, _)| match lt {
                    Type::Keyword(KeywordType {
                        kind: TsKeywordTypeKind::TsUnknownKeyword,
                        ..
                    }) => Some(()),

                    _ => None,
                }) {
                    debug_assert!(!span.is_dummy());
                    return Err(Error::Unknown { span });
                }

                if lt.is_num() && rt.is_num() {
                    return Ok(Type::Keyword(KeywordType {
                        span,
                        kind: TsKeywordTypeKind::TsNumberKeyword,
                        metadata: Default::default(),
                    }));
                }

                if let Some(()) = c.take_if_any_matches(|(_, lt), (_, _)| match *lt {
                    Type::Keyword(KeywordType {
                        kind: TsKeywordTypeKind::TsStringKeyword,
                        ..
                    })
                    | Type::Lit(LitType {
                        lit: RTsLit::Str(..), ..
                    }) => Some(()),

                    _ => None,
                }) {
                    return Ok(Type::Keyword(KeywordType {
                        span,
                        kind: TsKeywordTypeKind::TsStringKeyword,
                        metadata: Default::default(),
                    }));
                }

                // Rule:
                //  - any + string is string
                //  - any + other is any
                if let Some(kind) = c.take_if_any_matches(|(_, lt), (_, rt)| {
                    if lt.is_any() {
                        if rt.is_str() {
                            return Some(TsKeywordTypeKind::TsStringKeyword);
                        }
                        return Some(TsKeywordTypeKind::TsAnyKeyword);
                    }

                    None
                }) {
                    return Ok(Type::Keyword(KeywordType {
                        span,
                        kind,
                        metadata: Default::default(),
                    }));
                }

                if c.any(|(_, ty)| {
                    ty.is_kwd(TsKeywordTypeKind::TsUndefinedKeyword) || ty.is_kwd(TsKeywordTypeKind::TsNullKeyword)
                }) {
                    return Err(Error::TS2365 { span });
                }

                // Rule:
                //  - null is invalid operand
                //  - undefined is invalid operand
                if c.both(|(_, ty)| match *ty {
                    Type::Keyword(KeywordType {
                        kind: TsKeywordTypeKind::TsUndefinedKeyword,
                        ..
                    })
                    | Type::Keyword(KeywordType {
                        kind: TsKeywordTypeKind::TsNullKeyword,
                        ..
                    }) => true,

                    _ => false,
                }) {
                    return Err(Error::TS2365 { span });
                }

                if is_str_or_union(&lt) || is_str_or_union(&rt) {
                    return Ok(Type::Keyword(KeywordType {
                        span,
                        kind: TsKeywordTypeKind::TsStringKeyword,
                        metadata: Default::default(),
                    }));
                }
                // At this point rhs cannot be string.
                //
                // Known numeric operations are all handled above

                if self.can_be_casted_to_number_in_rhs(lt.span(), &lt)
                    && self.can_be_casted_to_number_in_rhs(rt.span(), &rt)
                {
                    return Ok(Type::Keyword(KeywordType {
                        span,
                        kind: TsKeywordTypeKind::TsNumberKeyword,
                        metadata: Default::default(),
                    }));
                }

                return Err(Error::InvalidBinaryOp { span, op });
            }
            op!("*") | op!("/") => {
                no_unknown!();

                return Ok(Type::Keyword(KeywordType {
                    span,
                    kind: TsKeywordTypeKind::TsNumberKeyword,
                    metadata: Default::default(),
                }));
            }

            op!(bin, "-")
            | op!("<<")
            | op!(">>")
            | op!(">>>")
            | op!("%")
            | op!("|")
            | op!("&")
            | op!("^")
            | op!("**") => {
                no_unknown!();

                if op == op!("**") {
                    let lt = lt.normalize();
                    let rt = rt.normalize();

                    self.report_possibly_null_or_undefined(lt.span(), &lt)
                        .report(&mut self.storage);

                    if lt.is_kwd(TsKeywordTypeKind::TsVoidKeyword)
                        || lt.is_str()
                        || lt.is_bool()
                        || lt.is_type_lit()
                        || lt.is_type_param()
                        || lt.is_interface()
                        || lt.is_tpl()
                    {
                        self.storage.report(Error::WrongTypeForLhsOfNumericOperation { span });
                    }

                    self.report_possibly_null_or_undefined(rt.span(), &rt)
                        .report(&mut self.storage);

                    if rt.is_kwd(TsKeywordTypeKind::TsVoidKeyword)
                        || rt.is_str()
                        || rt.is_bool()
                        || rt.is_type_lit()
                        || rt.is_type_param()
                        || rt.is_interface()
                        || rt.is_tpl()
                    {
                        self.storage.report(Error::WrongTypeForRhsOfNumericOperation { span });
                    }
                }

                return Ok(Type::Keyword(KeywordType {
                    kind: TsKeywordTypeKind::TsNumberKeyword,
                    span,
                    metadata: Default::default(),
                }));
            }

            op!("===") | op!("!==") | op!("!=") | op!("==") => {
                return Ok(Type::Keyword(KeywordType {
                    span,
                    kind: TsKeywordTypeKind::TsBooleanKeyword,
                    metadata: Default::default(),
                }));
            }

            op!("instanceof") => {
                if !self.is_valid_lhs_of_instanceof(span, &lt) {
                    self.storage.report(Error::InvalidLhsInInstanceOf {
                        ty: box lt.clone(),
                        span: left.span(),
                    })
                }

                return Ok(Type::Keyword(KeywordType {
                    span,
                    kind: TsKeywordTypeKind::TsBooleanKeyword,
                    metadata: Default::default(),
                }));
            }

            op!("<=") | op!("<") | op!(">=") | op!(">") => {
                no_unknown!();

                let mut check_for_invalid_operand = |ty: &Type| {
                    let res: ValidationResult<_> = try {
                        self.deny_null_or_undefined(ty.span(), ty)?;
                    };
                    res.report(&mut self.storage);
                };

                check_for_invalid_operand(&lt);
                check_for_invalid_operand(&rt);

                self.validate_relative_comparison_operands(span, op, &lt, &rt);

                return Ok(Type::Keyword(KeywordType {
                    span,
                    kind: TsKeywordTypeKind::TsBooleanKeyword,
                    metadata: Default::default(),
                }));
            }

            op!("in") => {
                if self.ctx.in_cond {
                    let left = match &**left {
                        RExpr::Lit(RLit::Str(s)) => Some(s.value.clone()),
                        RExpr::Tpl(t) if t.quasis.len() == 1 => t.quasis[0].cooked.clone().map(|v| v.value),
                        _ => None,
                    };
                    let name = Name::try_from(&**right).ok();

                    if let Some(name) = name {
                        if let Some(property) = left {
                            let mut new_ty = self.filter_types_with_property(&rt, &property, None)?.cheap();

                            self.add_deep_type_fact(span, name.clone(), new_ty.clone(), true);
                        }
                    }
                }

                return Ok(Type::Keyword(KeywordType {
                    span,
                    kind: TsKeywordTypeKind::TsBooleanKeyword,
                    metadata: Default::default(),
                }));
            }

            op!("||") | op!("&&") => {
                no_unknown!();
                let mut lt = lt;
                let mut rt = rt;

                if lt.type_eq(&rt) {
                    return Ok(lt);
                }

                let can_generalize = type_ann.is_none()
                    && match (&**left, &**right) {
                        (_, RExpr::Ident(..)) => false,
                        _ => true,
                    };

                if self.ctx.can_generalize_literals() && (can_generalize || self.may_generalize(&lt)) {
                    lt = lt.generalize_lit();
                    lt = lt.force_generalize_top_level_literals();
                }
                if self.ctx.can_generalize_literals() && (can_generalize || self.may_generalize(&rt)) {
                    rt = rt.generalize_lit();
                    rt = rt.force_generalize_top_level_literals();
                }

                if lt.type_eq(&rt) {
                    return Ok(lt);
                }

                match lt.normalize() {
                    Type::Keyword(KeywordType {
                        kind: TsKeywordTypeKind::TsAnyKeyword,
                        ..
                    }) => return Ok(Type::any(span, Default::default())),

                    _ => {}
                }

                match op {
                    op!("||") => {
                        if lt.is_never() {
                            return Ok(lt);
                        }

                        if is_str_lit_or_union(&lt) && is_str_lit_or_union(&rt) {
                            return Ok(Type::union(vec![lt, rt]));
                        }

                        if let Known(v) = lt.as_bool() {
                            return Ok(if v { lt } else { rt });
                        }

                        // if let (_, Known(v)) = left.as_bool() {
                        //     return Ok(if v { lt } else { rt });
                        // }

                        // Remove falsy types from lhs
                        let lt = lt.remove_falsy();

                        return Ok(Type::union(vec![lt, rt]));
                    }

                    op!("&&") => {
                        if lt.is_never() {
                            return Ok(lt);
                        }

                        if let Known(v) = lt.as_bool() {
                            return Ok(if v { rt } else { lt });
                        }

                        // if let (_, Known(v)) = left.as_bool() {
                        //     return Ok(if v { rt } else { lt });
                        // }
                    }

                    _ => unreachable!(),
                }
                return Ok(rt);
            }

            op!("??") => {
                let may_generalize_lt = self.may_generalize(&lt);

                let mut lt = lt.remove_falsy();
                let mut rt = rt;
                if may_generalize_lt {
                    lt = lt.generalize_lit();
                }
                if self.may_generalize(&rt) {
                    rt = rt.generalize_lit();
                }
                //
                if lt.type_eq(&rt) {
                    return Ok(lt);
                }

                let mut ty = Type::union(vec![lt, rt]);
                if !may_generalize_lt {
                    prevent_generalize(&mut ty);
                }

                Ok(ty)
            }
        }
    }
}

impl Analyzer<'_, '_> {
    fn add_type_facts_for_typeof(&mut self, span: Span, l: &RExpr, r: &RExpr, is_eq: bool) -> ValidationResult<()> {
        if !self.ctx.in_cond {
            return Ok(());
        }

        let c = Comparator { left: l, right: r };

        // Check typeof a === 'string'
        match c.take_if_any_matches(|l, r| match l {
            RExpr::Unary(RUnaryExpr {
                op: op!("typeof"),
                ref arg,
                ..
            }) => {
                //
                let name = Name::try_from(&**arg);
                info!("cond_facts: typeof {:?}", name);
                match r {
                    RExpr::Tpl(RTpl { quasis, .. }) if quasis.len() == 1 => {
                        let value = &quasis[0].cooked.as_ref()?.value;
                        Some((
                            name,
                            if is_eq {
                                (TypeFacts::typeof_eq(&*value), TypeFacts::typeof_neq(&*value))
                            } else {
                                (TypeFacts::typeof_neq(&*value), TypeFacts::typeof_eq(&*value))
                            },
                        ))
                    }
                    RExpr::Lit(RLit::Str(RStr { ref value, .. })) => Some((
                        name,
                        if is_eq {
                            (TypeFacts::typeof_eq(&*value), TypeFacts::typeof_neq(&*value))
                        } else {
                            (TypeFacts::typeof_neq(&*value), TypeFacts::typeof_eq(&*value))
                        },
                    )),
                    _ => None,
                }
            }
            _ => None,
        }) {
            Some((Ok(name), (Some(t), Some(f)))) => {
                // Add type facts
                self.cur_facts.true_facts.facts.insert(name.clone(), t);
                self.cur_facts.false_facts.facts.insert(name.clone(), f);
            }
            _ => {}
        }

        Ok(())
    }

    ///
    /// # Exmaple
    ///
    ///
    ///
    /// ```ts
    /// // Note: feature.geometry can be undefined
    ///
    /// function extractCoordinates(f: Feature): number[] {
    ///     if (f.geometry?.type !== 'test') {
    ///         return [];
    ///     }
    ///     return f.geometry.coordinates;
    /// }
    /// ```
    ///
    /// The condition in the if statement above will be `true` if `f.geometry`
    /// is `undefined`.
    fn add_type_facts_for_opt_chains(
        &mut self,
        span: Span,
        l: &RExpr,
        r: &RExpr,
        lt: &Type,
        rt: &Type,
    ) -> ValidationResult<()> {
        /// Convert expression to names.
        ///
        /// This may return multiple names if there are optional chaining
        /// expressions.
        fn non_undefined_names(e: &RExpr) -> Vec<Name> {
            match e {
                RExpr::OptChain(ROptChainExpr {
                    expr:
                        box RExpr::Member(
                            e
                            @
                            RMemberExpr {
                                obj: RExprOrSuper::Expr(..),
                                ..
                            },
                        ),
                    ..
                }) => {
                    let mut names = non_undefined_names(match &e.obj {
                        RExprOrSuper::Super(_) => unreachable!(),
                        RExprOrSuper::Expr(v) => &v,
                    });

                    names.extend(e.try_into().ok());
                    names
                }

                RExpr::Member(
                    e
                    @
                    RMemberExpr {
                        obj: RExprOrSuper::Expr(..),
                        ..
                    },
                ) => {
                    let mut names = non_undefined_names(match &e.obj {
                        RExprOrSuper::Super(_) => unreachable!(),
                        RExprOrSuper::Expr(v) => &v,
                    });

                    names.extend(e.try_into().ok());
                    names
                }

                _ => vec![],
            }
        }

        if !self.ctx.in_cond {
            return Ok(());
        }

        let c = Comparator {
            left: (l, lt),
            right: (r, rt),
        };

        match c.take_if_any_matches(|(l, _), (_, r_ty)| match (l, r_ty) {
            (
                RExpr::Ident(RIdent {
                    sym: js_word!("undefined"),
                    ..
                }),
                _,
            )
            | (RExpr::Lit(RLit::Null(..)), _) => None,

            (l, r) => {
                let names = non_undefined_names(l);
                if names.is_empty() {
                    return None;
                }

                Some((names, r_ty))
            }
        }) {
            Some((names, r_ty)) => {
                if !self.can_be_undefined(span, &r_ty)? {
                    for name in names {
                        self.cur_facts
                            .false_facts
                            .facts
                            .insert(name.clone(), TypeFacts::NEUndefined);
                    }
                }
            }
            _ => {}
        }

        // TODO
        Ok(())
    }

    fn is_valid_for_switch_case(&mut self, span: Span, disc_ty: &Type, case_ty: &Type) -> ValidationResult<bool> {
        let disc_ty = disc_ty.normalize();
        let case_ty = case_ty.normalize();

        if disc_ty.type_eq(case_ty) {
            return Ok(true);
        }

        if disc_ty.is_num_lit() && case_ty.is_num_lit() {
            return Ok(false);
        }

        if self.ctx.in_switch_case_test {
            if disc_ty.is_intersection_type() {
                return Ok(true);
            }
        }

        self.has_overlap(
            span,
            &disc_ty,
            &case_ty,
            CastableOpts {
                allow_assignment_to_param_constraint: true,
                ..Default::default()
            },
        )
    }

    /// We have to check for inheritnace.
    ///
    /// ```ts
    /// class C1 {
    ///     p1: string;
    /// }
    /// class C2 {
    ///     p2: number;
    /// }
    /// class D1 extends C1 {
    ///     p3: number;
    /// }
    /// var ctor2: C2 | D1;
    ///
    /// var r2: D1 | C2 = ctor2 instanceof C1 && ctor2; // C2 | D1
    /// ```
    ///
    /// in this case, we cannot store ctor2 as C1 because it would result in an
    /// error.
    ///
    /// TODO: Use Cow
    ///
    /// # Related tests
    ///
    /// ## narrowingConstrainedTypeVaraible.ts
    ///
    /// In the test, there's `function f2<T extends C, U extends D>(v: T | U)
    /// {}`.
    ///
    /// If we apply `instanceof C` to `v`, `v` becomes `T`.
    /// Note that `C extends D` and `D extends C` are true because both of `C`
    /// and `D` are empty classes.
    fn narrow_with_instanceof(&mut self, span: Span, ty: Cow<Type>, orig_ty: &Type) -> ValidationResult {
        let orig_ty = orig_ty.normalize();

        match orig_ty {
            Type::Ref(..) | Type::Query(..) => {
                let orig_ty = self.normalize(None, Cow::Borrowed(orig_ty), Default::default())?;
                return self.narrow_with_instanceof(span, ty, &orig_ty);
            }

            Type::Union(orig) => {
                let mut new_types = orig
                    .types
                    .iter()
                    .map(|orig_ty| self.narrow_with_instanceof(span, ty.clone(), orig_ty))
                    .collect::<Result<Vec<_>, _>>()?;

                new_types.retain(|ty| !ty.is_never());

                return Ok(Type::Union(Union {
                    span: orig.span,
                    types: new_types,
                    metadata: orig.metadata,
                })
                .fixed());
            }

            _ => {}
        }

        if orig_ty.is_kwd(TsKeywordTypeKind::TsStringKeyword)
            || orig_ty.is_kwd(TsKeywordTypeKind::TsNumberKeyword)
            || orig_ty.is_kwd(TsKeywordTypeKind::TsBooleanKeyword)
        {
            if ty.normalize().is_interface() {
                return Ok(Type::never(span, Default::default()));
            }
        }

        match ty.normalize() {
            Type::ClassDef(ty) => {
                return self.narrow_with_instanceof(
                    span,
                    Cow::Owned(Type::Class(Class {
                        span,
                        def: box ty.clone(),
                        metadata: Default::default(),
                    })),
                    orig_ty,
                )
            }
            _ => {}
        }

        if let Some(v) = self.extends(
            span,
            ExtendsOpts {
                disallow_different_classes: true,
                ..Default::default()
            },
            orig_ty,
            &ty,
        ) {
            if v {
                match orig_ty.normalize() {
                    Type::ClassDef(def) => {
                        return Ok(Type::Class(Class {
                            span,
                            def: box def.clone(),
                            metadata: Default::default(),
                        }))
                    }
                    _ => {}
                }
                return Ok(orig_ty.clone());
            } else {
                match (orig_ty, ty.normalize()) {
                    (Type::Interface(..), Type::Interface(..)) => return Ok(ty.into_owned()),
                    _ => {}
                }

                if !self
                    .has_overlap(
                        span,
                        orig_ty,
                        &ty,
                        CastableOpts {
                            disallow_different_classes: true,
                            disallow_special_assignment_to_empty_class: true,
                            ..Default::default()
                        },
                    )
                    .context("tried to check if overlap exists to calculate the type created by instanceof")?
                {
                    return Ok(Type::never(span, Default::default()));
                }
            }
        }

        match ty.normalize() {
            Type::ClassDef(def) => {
                return Ok(Type::Class(Class {
                    span,
                    def: box def.clone(),
                    metadata: Default::default(),
                }))
            }
            _ => {}
        }
        Ok(ty.into_owned())
    }

    #[extra_validator]
    fn validate_relative_comparison_operands(&mut self, span: Span, op: BinaryOp, l: &Type, r: &Type) {
        let marks = self.marks();

        let l = l.normalize();
        let r = r.normalize();

        match (l, r) {
            (Type::Ref(..), _) => {
                if let Ok(l) = self.expand_top_ref(l.span(), Cow::Borrowed(l), Default::default()) {
                    return self.validate_relative_comparison_operands(span, op, &l.freezed(), r);
                }
            }
            (l, Type::Ref(..)) => {
                if let Ok(r) = self.expand_top_ref(r.span(), Cow::Borrowed(r), Default::default()) {
                    return self.validate_relative_comparison_operands(span, op, l, &r.freezed());
                }
            }
            (Type::TypeLit(lt), Type::TypeLit(rt)) => {
                // It's an error if type of the parameter of index signature is same but type
                // annotation is different.
                for lm in &lt.members {
                    for rm in &rt.members {
                        match (lm, rm) {
                            (TypeElement::Index(lm), TypeElement::Index(rm)) if lm.params.type_eq(&rm.params) => {
                                if let Some(lt) = &lm.type_ann {
                                    if let Some(rt) = &rm.type_ann {
                                        if self.assign(span, &mut Default::default(), &lt, &rt).is_ok()
                                            || self.assign(span, &mut Default::default(), &rt, &lt).is_ok()
                                        {
                                            continue;
                                        }
                                    } else {
                                        continue;
                                    }
                                } else {
                                    continue;
                                }
                                //
                                self.storage.report(Error::CannotCompareWithOp {
                                    span,
                                    op,
                                    left: box l.clone(),
                                    right: box r.clone(),
                                });
                                return;
                            }
                            _ => {}
                        }
                    }
                }
            }
            _ => {}
        }

        let l = l.clone().generalize_lit();
        let r = r.clone().generalize_lit();
        if self.can_compare_relatively(span, &l, &r)? {
            return;
        }

        self.storage.report(Error::CannotCompareWithOp {
            span,
            op,
            left: box l.clone(),
            right: box r.clone(),
        });
    }

    fn can_compare_relatively(&mut self, span: Span, l: &Type, r: &Type) -> ValidationResult<bool> {
        let l = l.normalize();
        let r = r.normalize();

        if l.type_eq(r) {
            return Ok(true);
        }

        if l.is_str_lit() && r.is_str_lit() {
            return Ok(true);
        }

        let c = Comparator { left: l, right: r };

        if let Some(v) = c.take_if_any_matches(|l, r| {
            if l.is_type_param() {
                // Different type params cannot be compared relatively, although they can
                // overlap with other types.
                if r.is_type_param() {
                    return Some(false);
                }

                if r.is_kwd(TsKeywordTypeKind::TsBooleanKeyword)
                    || r.is_kwd(TsKeywordTypeKind::TsNumberKeyword)
                    || r.is_kwd(TsKeywordTypeKind::TsStringKeyword)
                    || r.is_kwd(TsKeywordTypeKind::TsVoidKeyword)
                    || r.is_enum_type()
                    || r.is_tuple()
                    || r.is_array()
                {
                    return Some(false);
                }

                if let Type::TypeLit(r) = r {
                    if r.members.is_empty() {
                        return Some(true);
                    }

                    return Some(false);
                }
            }

            None
        }) {
            return Ok(v);
        }

        // Basically we depend on assign's behavior, but there's are some corner cases
        // where it's not enough.
        match (l, r) {
            (Type::Class(l), Type::Class(r)) => {
                if l.def.super_class.is_none() && r.def.super_class.is_none() {
                    if l.def.body.is_empty() || r.def.body.is_empty() {
                        return Ok(false);
                    }
                }
            }

            (Type::TypeLit(lt), Type::TypeLit(rt)) => {
                if let Ok(Some(v)) = self.can_compare_type_elements_relatively(span, &lt.members, &rt.members) {
                    return Ok(v);
                }
            }
            _ => {}
        }

        self.has_overlap(span, &l, &r, Default::default())
    }

    /// Returns Ok(Some(v)) if this method has a special rule to handle type
    /// elements.
    fn can_compare_type_elements_relatively(
        &mut self,
        span: Span,
        l: &[TypeElement],
        r: &[TypeElement],
    ) -> ValidationResult<Option<bool>> {
        for lm in l {
            for rm in r {
                match (lm, rm) {
                    (TypeElement::Method(lm), TypeElement::Method(rm)) => {
                        if let Ok(()) = self.assign(span, &mut Default::default(), &lm.key.ty(), &rm.key.ty()) {
                            if lm.type_params.as_ref().map(|v| v.params.len()).unwrap_or(0)
                                != rm.type_params.as_ref().map(|v| v.params.len()).unwrap_or(0)
                            {
                                return Ok(Some(true));
                            }

                            let params_res = self.assign_params(
                                &mut Default::default(),
                                AssignOpts {
                                    span,
                                    ..Default::default()
                                },
                                &lm.params,
                                &rm.params,
                            );

                            if params_res.is_err() {
                                return Ok(Some(true));
                            }

                            let ret_ty_res = match (lm.ret_ty.as_deref(), rm.ret_ty.as_deref()) {
                                (Some(lt), Some(rt)) => self.assign_with_opts(
                                    &mut Default::default(),
                                    AssignOpts {
                                        span,
                                        allow_unknown_rhs: true,
                                        ..Default::default()
                                    },
                                    &lt,
                                    &rt,
                                ),
                                _ => Ok(()),
                            };
                        }
                    }

                    _ => {}
                }
            }
        }

        let lk = self.kinds_of_type_elements(l);
        let rk = self.kinds_of_type_elements(r);
        if lk != rk {
            return Ok(Some(false));
        }

        Ok(None)
    }

    fn is_valid_lhs_of_instanceof(&mut self, span: Span, ty: &Type) -> bool {
        let ty = ty.normalize();

        match ty {
            ty if ty.is_any() || ty.is_kwd(TsKeywordTypeKind::TsObjectKeyword) => true,

            Type::TypeLit(..)
            | Type::Interface(..)
            | Type::Class(..)
            | Type::This(..)
            | Type::Param(..)
            | Type::Mapped(..)
            | Type::Ref(..) => true,

            Type::Intersection(ty) => ty.types.iter().all(|ty| self.is_valid_lhs_of_instanceof(span, ty)),

            Type::Union(ty) => ty.types.iter().any(|ty| self.is_valid_lhs_of_instanceof(span, ty)),

            _ => false,
        }
    }

    /// The right operand to be of type Any or a subtype of the 'Function'
    /// interface type.
    fn validate_rhs_of_instanceof(&mut self, span: Span, type_for_error: &Type, ty: Type) -> Type {
        if ty.is_any() {
            return ty;
        }

        // TODO: We should assign this to builtin interface `Function`.
        match ty.normalize() {
            // Error
            Type::Keyword(KeywordType {
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
            })
            | Type::Keyword(KeywordType {
                kind: TsKeywordTypeKind::TsVoidKeyword,
                ..
            })
            | Type::Lit(..)
            | Type::Class(..)
            | Type::Ref(Ref {
                type_name:
                    RTsEntityName::Ident(RIdent {
                        sym: js_word!("Object"),
                        ..
                    }),
                ..
            })
            | Type::Symbol(..) => {
                self.storage.report(Error::InvalidRhsInInstanceOf {
                    span,
                    ty: box type_for_error.clone(),
                });
            }

            Type::TypeLit(e) if e.members.is_empty() => {
                self.storage.report(Error::InvalidRhsInInstanceOf {
                    span,
                    ty: box type_for_error.clone(),
                });
            }

            Type::Union(u) => {
                let types = u
                    .types
                    .iter()
                    .map(|ty| self.validate_rhs_of_instanceof(span, type_for_error, ty.clone()))
                    .collect();

                return Type::Union(Union {
                    span: u.span,
                    types,
                    metadata: u.metadata,
                });
            }

            // Ok
            Type::ClassDef(..) => {}

            // Conditionally error.
            //
            // Ok if it's assignable to `Function`.
            Type::TypeLit(..) | Type::Interface(..) => {
                if let Err(..) = self.assign(
                    span,
                    &mut Default::default(),
                    &Type::Ref(Ref {
                        span,
                        ctxt: ModuleId::builtin(),
                        type_name: RTsEntityName::Ident(RIdent::new(
                            "Function".into(),
                            span.with_ctxt(SyntaxContext::empty()),
                        )),
                        type_args: None,
                        metadata: Default::default(),
                    }),
                    &ty,
                ) {
                    self.storage.report(Error::InvalidRhsInInstanceOf {
                        span,
                        ty: box type_for_error.clone(),
                    });
                }
            }

            Type::Ref(..) => {
                // Report error and return ref type back.
                self.make_instance_or_report(span, &ty);
            }

            _ => return self.make_instance_or_report(span, &ty),
        }

        ty
    }

    /// We should create a type fact for `foo` in `if (foo.type === 'bar');`.
    fn calc_type_facts_for_equality(&mut self, name: Name, equals_to: &Type) -> ValidationResult<(Name, Type)> {
        let span = equals_to.span();

        let mut id: RIdent = name.as_ids()[0].clone().into();
        id.span.lo = span.lo;
        id.span.hi = span.hi;

        if name.len() == 1 {
            let orig_ty = self.type_of_var(&id, TypeOfMode::RValue, None)?;

            let narrowed = self
                .narrow_with_equality(&orig_ty, equals_to)
                .context("tried to narrow type with equality")?;

            return Ok((name, narrowed));
        }

        let eq_ty = equals_to.normalize();

        // We create a type fact for `foo` in `if (foo.type === 'bar');`

        let ids = name.as_ids();

        let prop = Key::Normal {
            span,
            sym: ids[ids.len() - 1].sym().clone(),
        };

        let ty = self.type_of_name(span, &name.as_ids()[..name.len() - 1], TypeOfMode::RValue, None)?;
        let ty = self
            .expand_top_ref(span, Cow::Owned(ty), Default::default())?
            .into_owned();

        match ty.normalize() {
            Type::Union(u) => {
                let mut candidates = vec![];
                for ty in &u.types {
                    let prop_res =
                        self.access_property(span, ty, &prop, TypeOfMode::RValue, IdCtx::Var, Default::default());

                    match prop_res {
                        Ok(prop_ty) => {
                            let prop_ty =
                                self.expand_top_ref(prop_ty.span(), Cow::Owned(prop_ty), Default::default())?;
                            let possible = match prop_ty.normalize() {
                                // Type parameters might have same value.
                                Type::Param(..) => true,
                                _ => prop_ty.type_eq(equals_to),
                            };
                            if possible {
                                candidates.push(ty.clone())
                            }
                        }
                        _ => {}
                    }
                }
                let actual = Name::from(&name.as_ids()[..name.len() - 1]);

                return Ok((actual, Type::union(candidates)));
            }
            _ => {}
        }

        Ok((name, eq_ty.clone()))
    }

    /// Returns new type of the variable after comparision with `===`.
    ///
    /// # Parameters
    ///
    /// ## orig_ty
    ///
    /// Original type of the variable.
    fn narrow_with_equality(&mut self, orig_ty: &Type, equals_to: &Type) -> ValidationResult<Type> {
        let span = equals_to.span();

        if orig_ty.type_eq(&equals_to) {
            return Ok(orig_ty.clone());
        }

        let orig_ty = self.normalize(Some(span), Cow::Borrowed(orig_ty), Default::default())?;
        let equals_to = self.normalize(Some(span), Cow::Borrowed(equals_to), Default::default())?;

        if orig_ty.type_eq(&equals_to) {
            return Ok(orig_ty.into_owned());
        }

        // Exclude nevers.
        match &*orig_ty {
            Type::Union(orig) => {
                let mut types = vec![];
                // We
                for orig in &orig.types {
                    let new_ty = self
                        .narrow_with_equality(&orig, &equals_to)
                        .context("tried to narrow element of a union type")?;

                    if new_ty.is_never() {
                        continue;
                    }
                    types.push(new_ty);
                }

                return Ok(Type::Union(Union {
                    span,
                    types,
                    metadata: UnionMetadata {
                        common: equals_to.metadata(),
                        ..Default::default()
                    },
                })
                .fixed());
            }
            _ => {}
        }

        // At here two variants are different from each other because we checked with
        // type_eq above.
        if orig_ty.is_enum_variant() && equals_to.is_enum_variant() {
            return Ok(Type::never(
                span,
                KeywordTypeMetadata {
                    common: equals_to.metadata(),
                    ..Default::default()
                },
            ));
        }

        // Defaults to new type.
        Ok(equals_to.into_owned())
    }

    fn report_errors_for_bin_expr(&mut self, span: Span, op: BinaryOp, lt: &Type, rt: &Type) {
        let ls = lt.span();
        let rs = rt.span();

        let lt = lt.normalize();
        let rt = rt.normalize();

        let mut errors = Errors::default();

        match op {
            op!(bin, "+") => {
                // Validation is performed in type_of_bin_expr because
                // validation of types is required to compute type of the
                // expression.
            }
            op!("||") | op!("&&") => match lt.normalize() {
                Type::Keyword(KeywordType {
                    kind: TsKeywordTypeKind::TsVoidKeyword,
                    ..
                }) => errors.push(Error::TS1345 { span }),
                _ => {}
            },

            op!("*")
            | op!("/")
            | op!("%")
            | op!(bin, "-")
            | op!("<<")
            | op!(">>")
            | op!(">>>")
            | op!("&")
            | op!("^")
            | op!("|") => {
                let mut check = |ty: &Type, is_left| {
                    if ty.is_any() {
                        return;
                    }
                    if self.can_be_casted_to_number_in_rhs(ty.span(), &ty) {
                        return;
                    }

                    match ty.normalize() {
                        Type::Keyword(KeywordType {
                            span,
                            kind: TsKeywordTypeKind::TsUndefinedKeyword,
                            ..
                        }) => {
                            self.storage.report(Error::ObjectIsPossiblyUndefined { span: *span });
                        }

                        Type::Keyword(KeywordType {
                            span,
                            kind: TsKeywordTypeKind::TsNullKeyword,
                            ..
                        }) => {
                            self.storage.report(Error::ObjectIsPossiblyNull { span: *span });
                        }

                        _ => errors.push(if is_left {
                            Error::WrongTypeForLhsOfNumericOperation { span: ty.span() }
                        } else {
                            Error::WrongTypeForRhsOfNumericOperation { span: ty.span() }
                        }),
                    }
                };

                if (op == op!("&") || op == op!("^") || op == op!("|"))
                    && match lt.normalize() {
                        Type::Keyword(KeywordType {
                            kind: TsKeywordTypeKind::TsBooleanKeyword,
                            ..
                        })
                        | Type::Lit(LitType {
                            lit: RTsLit::Bool(..), ..
                        }) => true,
                        _ => false,
                    }
                    && match rt.normalize() {
                        Type::Keyword(KeywordType {
                            kind: TsKeywordTypeKind::TsBooleanKeyword,
                            ..
                        })
                        | Type::Lit(LitType {
                            lit: RTsLit::Bool(..), ..
                        }) => true,
                        _ => false,
                    }
                {
                    errors.push(Error::TS2447 { span });
                } else {
                    check(&lt, true);
                    check(&rt, false);
                }
            }

            op!("in") => {
                match lt.normalize() {
                    Type::Keyword(KeywordType {
                        kind: TsKeywordTypeKind::TsNullKeyword,
                        ..
                    }) => {
                        self.storage.report(Error::ObjectIsPossiblyNull { span });
                    }

                    Type::Keyword(KeywordType {
                        kind: TsKeywordTypeKind::TsUndefinedKeyword,
                        ..
                    }) => {
                        self.storage.report(Error::ObjectIsPossiblyUndefined { span });
                    }

                    ty => {
                        if !self.is_valid_lhs_of_in(&ty) {
                            errors.push(Error::TS2360 { span: ls });
                        }
                    }
                }

                match rt.normalize() {
                    Type::Keyword(KeywordType {
                        kind: TsKeywordTypeKind::TsNullKeyword,
                        ..
                    }) => {
                        self.storage.report(Error::ObjectIsPossiblyNull { span });
                    }

                    Type::Keyword(KeywordType {
                        kind: TsKeywordTypeKind::TsUndefinedKeyword,
                        ..
                    }) => {
                        self.storage.report(Error::ObjectIsPossiblyUndefined { span });
                    }

                    _ => {
                        if !self.is_valid_rhs_of_in(&rt) {
                            errors.push(Error::TS2361 { span: rs })
                        }
                    }
                }
            }

            _ => {}
        }

        self.storage.report_all(errors);
    }

    fn is_valid_lhs_of_in(&mut self, ty: &Type) -> bool {
        let ty = ty.normalize();

        match ty {
            Type::Ref(..) => {
                if let Ok(ty) = self.expand_top_ref(ty.span(), Cow::Borrowed(ty), Default::default()) {
                    return self.is_valid_lhs_of_in(&ty);
                }

                true
            }

            Type::Keyword(KeywordType {
                kind: TsKeywordTypeKind::TsAnyKeyword,
                ..
            })
            | Type::Keyword(KeywordType {
                kind: TsKeywordTypeKind::TsStringKeyword,
                ..
            })
            | Type::Keyword(KeywordType {
                kind: TsKeywordTypeKind::TsNumberKeyword,
                ..
            })
            | Type::Keyword(KeywordType {
                kind: TsKeywordTypeKind::TsBigIntKeyword,
                ..
            })
            | Type::Keyword(KeywordType {
                kind: TsKeywordTypeKind::TsSymbolKeyword,
                ..
            })
            | Type::Lit(LitType {
                lit: RTsLit::Number(..),
                ..
            })
            | Type::Lit(LitType {
                lit: RTsLit::Str(..), ..
            })
            | Type::Enum(..)
            | Type::EnumVariant(..)
            | Type::Param(..)
            | Type::Operator(Operator {
                op: TsTypeOperatorOp::KeyOf,
                ..
            })
            | Type::Symbol(..) => true,

            Type::Union(ref u) => u.types.iter().all(|ty| self.is_valid_lhs_of_in(&ty)),

            _ => false,
        }
    }

    fn is_valid_rhs_of_in(&mut self, ty: &Type) -> bool {
        if ty.is_any() {
            return true;
        }

        match ty.normalize() {
            Type::Ref(..) => {
                if let Ok(ty) = self.expand_top_ref(ty.span(), Cow::Borrowed(ty), Default::default()) {
                    return self.is_valid_rhs_of_in(&ty);
                }

                true
            }

            Type::TypeLit(..)
            | Type::Param(..)
            | Type::Mapped(..)
            | Type::Array(..)
            | Type::Tuple(..)
            | Type::IndexedAccessType(..)
            | Type::Interface(..)
            | Type::Keyword(KeywordType {
                kind: TsKeywordTypeKind::TsObjectKeyword,
                ..
            }) => true,
            Type::Union(ref u) => u.types.iter().all(|ty| self.is_valid_rhs_of_in(&ty)),

            _ => false,
        }
    }

    #[extra_validator]
    fn report_errors_for_mixed_nullish_coalescing(&mut self, e: &RBinExpr) {
        fn search(span: Span, op: BinaryOp, operand: &RExpr) -> ValidationResult<()> {
            if op == op!("??") {
                match operand {
                    RExpr::Bin(bin) => {
                        if bin.op == op!("||") || bin.op == op!("&&") {
                            return Err(Error::NullishCoalescingMixedWithLogicalWithoutParen { span });
                        }
                    }
                    _ => {}
                }
            } else if op == op!("||") || op == op!("&&") {
                match operand {
                    RExpr::Bin(bin) => {
                        if bin.op == op!("??") {
                            return Err(Error::NullishCoalescingMixedWithLogicalWithoutParen { span });
                        }
                    }
                    _ => {}
                }
            }

            Ok(())
        }

        search(e.span, e.op, &e.left)?;
        search(e.span, e.op, &e.right)?;
    }
}

pub(super) fn extract_name_for_assignment(e: &RExpr, is_exact_eq: bool) -> Option<Name> {
    match e {
        RExpr::Paren(e) => extract_name_for_assignment(&e.expr, is_exact_eq),
        RExpr::Assign(e) => match &e.left {
            RPatOrExpr::Expr(e) => extract_name_for_assignment(e, is_exact_eq),
            RPatOrExpr::Pat(pat) => match &**pat {
                RPat::Ident(i) => Some(i.id.clone().into()),
                RPat::Expr(e) => extract_name_for_assignment(e, is_exact_eq),
                _ => None,
            },
        },
        RExpr::Member(RMemberExpr {
            obj: RExprOrSuper::Expr(obj),
            prop,
            computed,
            ..
        }) => {
            let mut name = extract_name_for_assignment(obj, is_exact_eq)?;

            name.push(match &**prop {
                RExpr::Ident(i) if !*computed => i.sym.clone(),
                RExpr::Lit(RLit::Str(s)) if *computed => s.value.clone(),
                _ => return None,
            });

            Some(name)
        }

        _ => Name::try_from(e).ok(),
    }
}
