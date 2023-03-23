use std::{
    borrow::Cow,
    collections::hash_map::Entry,
    convert::{TryFrom, TryInto},
};

use fxhash::FxHashMap;
use stc_ts_ast_rnode::{
    RBinExpr, RBindingIdent, RComputedPropName, RExpr, RIdent, RLit, RMemberExpr, RMemberProp, ROptChainBase, ROptChainExpr, RPat,
    RPatOrExpr, RStr, RTpl, RTsEntityName, RTsLit, RUnaryExpr,
};
use stc_ts_errors::{DebugExt, ErrorKind, Errors};
use stc_ts_file_analyzer_macros::extra_validator;
use stc_ts_type_ops::{generalization::prevent_generalize, is_str_lit_or_union, Fix};
use stc_ts_types::{
    name::Name, Class, IdCtx, Intersection, Key, KeywordType, KeywordTypeMetadata, LitType, Ref, Tuple, TypeElement, TypeLit, TypeParam,
    TypeParamInstantiation, Union, UnionMetadata,
};
use stc_utils::{cache::Freeze, dev_span, stack};
use swc_atoms::js_word;
use swc_common::{Span, Spanned, SyntaxContext, TypeEq};
use swc_ecma_ast::{op, BinaryOp, TsKeywordTypeKind};
use swc_ecma_utils::Value::Known;
use tracing::info;

use crate::{
    analyzer::{
        assign::AssignOpts,
        expr::{type_cast::CastableOpts, TypeOfMode},
        generic::ExtendsOpts,
        scope::ExpandOpts,
        types::NormalizeTypeOpts,
        util::{Comparator, ResultExt},
        Analyzer, Ctx, ScopeKind,
    },
    ty::{Type, TypeExt},
    type_facts::TypeFacts,
    util::RemoveTypes,
    validator,
    validator::ValidateWith,
    VResult,
};

#[validator]
impl Analyzer<'_, '_> {
    fn validate(&mut self, e: &RBinExpr, type_ann: Option<&Type>) -> VResult<Type> {
        self.validate_bin_expr(e, type_ann)
    }
}

impl Analyzer<'_, '_> {
    fn validate_bin_expr(&mut self, e: &RBinExpr, type_ann: Option<&Type>) -> VResult<Type> {
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
            should_store_truthy_for_access: self.ctx.should_store_truthy_for_access && matches!(op, op!("&&")),
            in_cond: self.ctx.in_cond || matches!(op, op!("&&") | op!("||")),
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
                ty = self.expand(
                    span,
                    ty,
                    ExpandOpts {
                        full: true,
                        expand_union: true,
                        preserve_ref: false,
                        ignore_expand_prevention_for_top: true,
                        ..Default::default()
                    },
                )?;
            }
            let span = ty.span();
            ty.reposition(left.span());

            Ok(ty)
        })
        .store(&mut errors);
        lt.freeze();

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
            .with_child(ScopeKind::Flow, true_facts_for_rhs.clone(), |child: &mut Analyzer| -> VResult<_> {
                child.ctx.should_store_truthy_for_access = false;

                let any_type_param = TypeParamInstantiation {
                    span,
                    params: vec![Type::any(span, Default::default())],
                };
                let type_args = if let RExpr::Ident(..) = &**right {
                    Some(&any_type_param)
                } else {
                    None
                };

                let truthy_lt;
                let child_ctxt = (
                    TypeOfMode::RValue,
                    type_args,
                    match op {
                        op!("??") | op!("&&") | op!("||") => match type_ann {
                            Some(ty) => Some(ty),
                            _ => match op {
                                op!("||") | op!("??") => {
                                    truthy_lt = lt.clone().map(|ty| child.apply_type_facts_to_type(TypeFacts::Truthy, ty));
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
                        ty = child.expand(
                            span,
                            ty,
                            ExpandOpts {
                                full: true,
                                expand_union: true,
                                preserve_ref: false,
                                ignore_expand_prevention_for_top: true,
                                ..Default::default()
                            },
                        )?;
                    }

                    let span = ty.span();
                    ty.reposition(right.span());

                    Ok(ty)
                })?;

                Ok(ty)
            })
            .store(&mut errors);

        let rt = rhs;

        self.report_errors_for_bin_expr(
            span,
            op,
            &lt.as_ref()
                .map(Cow::Borrowed)
                .unwrap_or_else(|| Cow::Owned(Type::any(left.span().with_ctxt(SyntaxContext::empty()), Default::default()))),
            &rt.as_ref()
                .map(Cow::Borrowed)
                .unwrap_or_else(|| Cow::Owned(Type::any(left.span().with_ctxt(SyntaxContext::empty()), Default::default()))),
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
                    *self.cur_facts.false_facts.facts.entry(k.clone()).or_default() &= v;
                }
            }

            self.cur_facts.false_facts += additional_false_facts;
        } else {
            self.cur_facts = prev_facts;
        }

        let (mut lt, mut rt): (Type, Type) = match (lt, rt) {
            (Some(l), Some(r)) => (l, r),
            _ => return Err(ErrorKind::Errors { span, errors }.into()),
        };
        if self.ctx.in_switch_case_test {
            if lt.is_tpl() {
                lt = lt.generalize_lit();
            }
        }

        lt.freeze();
        rt.freeze();

        if !self.config.is_builtin {
            debug_assert!(!lt.span().is_dummy());

            debug_assert!(!rt.span().is_dummy());
        }

        let mut reported_null_or_undefined = false;

        match op {
            op!("**")
            | op!("<=")
            | op!("<")
            | op!(">=")
            | op!(">")
            | op!("*")
            | op!("/")
            | op!("%")
            | op!(bin, "-")
            | op!("<<")
            | op!(">>")
            | op!(">>>")
            | op!("&")
            | op!("^")
            | op!("|") => {
                if matches!(
                    &*e.left,
                    RExpr::Lit(RLit::Null(..))
                        | RExpr::Ident(RIdent {
                            sym: js_word!("undefined"),
                            ..
                        })
                ) {
                    self.storage
                        .report(ErrorKind::UndefinedOrNullIsNotValidOperand { span: e.left.span() }.into());
                    reported_null_or_undefined = true;
                }

                if matches!(
                    &*e.right,
                    RExpr::Lit(RLit::Null(..))
                        | RExpr::Ident(RIdent {
                            sym: js_word!("undefined"),
                            ..
                        })
                ) {
                    self.storage
                        .report(ErrorKind::UndefinedOrNullIsNotValidOperand { span: e.right.span() }.into());
                    reported_null_or_undefined = true;
                }
            }
            _ => {}
        }

        // Handle control-flow based typing
        match op {
            op!("===") | op!("!==") | op!("==") | op!("!=") => {
                let is_eq = op == op!("===") || op == op!("==");

                self.add_type_facts_for_typeof(span, left, right, is_eq, &lt, &rt)
                    .report(&mut self.storage);

                // Try narrowing type
                let c = Comparator {
                    left: (&**left, lt.normalize()),
                    right: (&**right, rt.normalize()),
                };

                let mut has_switch_case_test_not_compatible = false;
                if !self.can_compare_with_eq(span, &lt, &rt)? {
                    if self.ctx.in_switch_case_test {
                        has_switch_case_test_not_compatible = true;
                        self.storage.report(
                            ErrorKind::SwitchCaseTestNotCompatible {
                                span,
                                disc: box lt.clone(),
                                test: box rt.clone(),
                            }
                            .into(),
                        )
                    } else {
                        self.storage.report(
                            ErrorKind::NoOverlap {
                                span,
                                value: true,
                                left: box lt.clone(),
                                right: box rt.clone(),
                            }
                            .into(),
                        )
                    }
                }

                if let Some((Ok(name), ty)) = c.take_if_any_matches(|(l, l_ty), (_, r_ty)| match *l_ty {
                    Type::Keyword(KeywordType {
                        kind: TsKeywordTypeKind::TsUnknownKeyword,
                        ..
                    }) => {
                        //
                        Some((Name::try_from(l), r_ty))
                    }
                    _ => None,
                }) {
                    let ty = ty.clone().freezed();
                    if is_eq {
                        self.add_deep_type_fact(span, name, ty, true);
                    } else {
                        self.add_deep_type_fact(span, name, ty, false);
                    }
                }

                self.add_type_facts_for_opt_chains(span, left, right, &lt, &rt)
                    .report(&mut self.storage);

                if let Some((l, r_ty)) = c.take_if_any_matches(|(l, _), (_, r_ty)| match (l, r_ty) {
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
                    // === with an unknown does not narrow type
                    if self.ctx.in_cond && !r_ty.is_unknown() {
                        let (name, mut r, exclude) = self.calc_type_facts_for_equality(l, r_ty)?;

                        r = if has_switch_case_test_not_compatible {
                            Type::Keyword(KeywordType {
                                span,
                                kind: TsKeywordTypeKind::TsNeverKeyword,
                                metadata: Default::default(),
                                tracker: Default::default(),
                            })
                        } else {
                            prevent_generalize(&mut r);
                            r.freeze();
                            r
                        };

                        let mut is_loose_comparison_with_null_or_undefined = false;
                        match op {
                            op!("==") | op!("!=") => {
                                if r.is_null() | r.is_undefined() {
                                    is_loose_comparison_with_null_or_undefined = true;
                                    let eq = TypeFacts::EQUndefinedOrNull | TypeFacts::EQNull | TypeFacts::EQUndefined;
                                    let neq = TypeFacts::NEUndefinedOrNull | TypeFacts::NENull | TypeFacts::NEUndefined;

                                    if op == op!("==") {
                                        *self.cur_facts.true_facts.facts.entry(name.clone()).or_default() |= eq;
                                        *self.cur_facts.false_facts.facts.entry(name.clone()).or_default() |= neq;
                                    } else {
                                        *self.cur_facts.true_facts.facts.entry(name.clone()).or_default() |= neq;
                                        *self.cur_facts.false_facts.facts.entry(name.clone()).or_default() |= eq;
                                    }
                                }
                            }
                            op!("===") => {
                                if r.is_null() {
                                    let eq = TypeFacts::EQNull;
                                    let neq = TypeFacts::NENull;

                                    if op == op!("===") {
                                        *self.cur_facts.true_facts.facts.entry(name.clone()).or_default() |= eq;
                                        *self.cur_facts.false_facts.facts.entry(name.clone()).or_default() |= neq;
                                    } else {
                                        *self.cur_facts.true_facts.facts.entry(name.clone()).or_default() |= neq;
                                        *self.cur_facts.false_facts.facts.entry(name.clone()).or_default() |= eq;
                                    }
                                } else if r.is_undefined() {
                                    let eq = TypeFacts::EQUndefined;
                                    let neq = TypeFacts::NEUndefined;

                                    if op == op!("===") {
                                        *self.cur_facts.true_facts.facts.entry(name.clone()).or_default() |= eq;
                                        *self.cur_facts.false_facts.facts.entry(name.clone()).or_default() |= neq;
                                    } else {
                                        *self.cur_facts.true_facts.facts.entry(name.clone()).or_default() |= neq;
                                        *self.cur_facts.false_facts.facts.entry(name.clone()).or_default() |= eq;
                                    }
                                }
                            }
                            _ => (),
                        }
                        let additional_target = if lt.metadata().destructure_key.0 > 0 {
                            let origin_ty = self.find_destructor(lt.metadata().destructure_key);
                            if let Some(ty) = origin_ty {
                                let ty = ty.into_owned();
                                self.get_additional_exclude_target(span, ty, &r, name.clone(), is_loose_comparison_with_null_or_undefined)
                            } else {
                                Default::default()
                            }
                        } else {
                            Default::default()
                        };

                        let exclude_types = if is_loose_comparison_with_null_or_undefined {
                            vec![
                                Type::Keyword(KeywordType {
                                    span,
                                    kind: TsKeywordTypeKind::TsNullKeyword,
                                    metadata: Default::default(),
                                    tracker: Default::default(),
                                }),
                                Type::Keyword(KeywordType {
                                    span,
                                    kind: TsKeywordTypeKind::TsUndefinedKeyword,
                                    metadata: Default::default(),
                                    tracker: Default::default(),
                                }),
                            ]
                        } else {
                            exclude.freezed()
                        };

                        if is_eq {
                            self.cur_facts
                                .false_facts
                                .excludes
                                .entry(name.clone())
                                .or_default()
                                .extend(exclude_types);
                            for (name, exclude_type) in additional_target.iter() {
                                self.cur_facts
                                    .false_facts
                                    .excludes
                                    .entry(name.clone())
                                    .or_default()
                                    .extend(exclude_type.clone());
                            }
                        } else {
                            self.cur_facts
                                .true_facts
                                .excludes
                                .entry(name.clone())
                                .or_default()
                                .extend(exclude_types);
                            for (name, exclude_type) in additional_target.iter() {
                                self.cur_facts
                                    .false_facts
                                    .excludes
                                    .entry(name.clone())
                                    .or_default()
                                    .extend(exclude_type.clone());
                            }
                        }

                        r = if let Type::Param(TypeParam {
                            span: param_span,
                            constraint: Some(param),
                            name: param_name,
                            metadata,
                            default,
                            ..
                        }) = c.left.1.normalize()
                        {
                            if param.is_unknown() || param.is_any() {
                                Type::Param(TypeParam {
                                    span: *param_span,
                                    constraint: Some(box Type::Keyword(KeywordType {
                                        span: *param_span,
                                        kind: TsKeywordTypeKind::TsUnknownKeyword,
                                        metadata: Default::default(),
                                        tracker: Default::default(),
                                    })),
                                    name: param_name.clone(),
                                    default: default.clone(),
                                    metadata: *metadata,
                                    tracker: Default::default(),
                                })
                            } else {
                                r
                            }
                        } else {
                            r
                        };
                        self.add_deep_type_fact(span, name, r, is_eq);
                        for (name, exclude_type) in additional_target.iter() {
                            for exclude in exclude_type.iter() {
                                self.add_deep_type_fact(span, name.clone(), exclude.clone(), is_eq);
                            }
                        }
                    }
                }
            }

            op!("instanceof") => {
                if !self.config.is_builtin {
                    if let Ok(name) = Name::try_from(&**left) {
                        // typeGuardsTypeParameters.ts says
                        //
                        // Type guards involving type parameters produce intersection types
                        let mut orig_ty = self.type_of_name(span, &name, TypeOfMode::RValue, None)?;
                        if !self.is_valid_lhs_of_instanceof(span, &orig_ty) {
                            self.storage.report(
                                ErrorKind::InvalidLhsInInstanceOf {
                                    ty: box lt.clone(),
                                    span: left.span(),
                                }
                                .into(),
                            )
                        }

                        orig_ty.freeze();

                        //
                        let ty = self.validate_rhs_of_instanceof(span, &rt, rt.clone());

                        // `o` cannot be null in the following code
                        // if (o?.baz instanceof Error) {
                        //     o.baz;
                        // }
                        for len in 1..=name.len() {
                            *self.cur_facts.true_facts.facts.entry(name.slice_to(len)).or_default() |=
                                TypeFacts::NENull & TypeFacts::NEUndefined & TypeFacts::NEUndefinedOrNull;
                        }

                        // typeGuardsWithInstanceOfByConstructorSignature.ts
                        //
                        // says
                        //
                        // `can't narrow type from 'any' to 'Object'`
                        // `can't narrow type from 'any' to 'Function'
                        let cannot_narrow = orig_ty.is_any()
                            && matches!(
                                &**right,
                                RExpr::Ident(RIdent {
                                    sym: js_word!("Object"),
                                    ..
                                }) | RExpr::Ident(RIdent {
                                    sym: js_word!("Function"),
                                    ..
                                })
                            );

                        if self.ctx.in_cond && !cannot_narrow {
                            let narrowed_ty = self
                                .narrow_with_instanceof(span, Cow::Borrowed(&ty), &orig_ty)
                                .context("tried to narrow type with instanceof")?
                                .freezed();

                            let filtered_ty = if narrowed_ty.is_any() { orig_ty.clone() } else { narrowed_ty };

                            filtered_ty.assert_valid();

                            // TODO(kdy1): Maybe we need to check for intersection or union
                            if orig_ty.is_type_param() {
                                self.cur_facts.true_facts.vars.insert(
                                    name,
                                    Type::Intersection(Intersection {
                                        span,
                                        types: vec![orig_ty, filtered_ty],
                                        metadata: Default::default(),
                                        tracker: Default::default(),
                                    })
                                    .fixed()
                                    .freezed(),
                                );
                            } else {
                                self.cur_facts.true_facts.vars.insert(name.clone(), filtered_ty.clone());

                                self.cur_facts.false_facts.excludes.entry(name).or_default().push(filtered_ty);
                            }
                        }
                    }
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
                        return Err(ErrorKind::IsTypeUnknown { span }.into());
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
                    return Err(ErrorKind::Unknown { span }.into());
                }

                if lt.is_num() && rt.is_num() {
                    return Ok(Type::Keyword(KeywordType {
                        span,
                        kind: TsKeywordTypeKind::TsNumberKeyword,
                        metadata: Default::default(),
                        tracker: Default::default(),
                    }));
                }

                if let Some(()) = c.take_if_any_matches(|(_, lt), (_, _)| match *lt {
                    Type::Keyword(KeywordType {
                        kind: TsKeywordTypeKind::TsStringKeyword,
                        ..
                    })
                    | Type::Lit(LitType { lit: RTsLit::Str(..), .. }) => Some(()),

                    _ => None,
                }) {
                    return Ok(Type::Keyword(KeywordType {
                        span,
                        kind: TsKeywordTypeKind::TsStringKeyword,
                        metadata: Default::default(),
                        tracker: Default::default(),
                    }));
                }

                // Rule:
                //  - any + string is string
                //  - any + other is any
                if let Some(kind) = c.take_if_any_matches(|(_, lt), (_, rt)| {
                    if lt.is_any() {
                        if rt.is_str() || rt.is_tpl() {
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
                        tracker: Default::default(),
                    }));
                }

                if c.any(|(_, ty)| ty.is_kwd(TsKeywordTypeKind::TsUndefinedKeyword) || ty.is_kwd(TsKeywordTypeKind::TsNullKeyword)) {
                    return Err(ErrorKind::TS2365 { span }.into());
                }

                // Rule:
                //  - null is invalid operand
                //  - undefined is invalid operand
                if c.both(|(_, ty)| {
                    matches!(
                        *ty,
                        Type::Keyword(KeywordType {
                            kind: TsKeywordTypeKind::TsUndefinedKeyword,
                            ..
                        }) | Type::Keyword(KeywordType {
                            kind: TsKeywordTypeKind::TsNullKeyword,
                            ..
                        })
                    )
                }) {
                    return Err(ErrorKind::TS2365 { span }.into());
                }

                if is_str_like_for_addition(&lt) || is_str_like_for_addition(&rt) {
                    return Ok(Type::Keyword(KeywordType {
                        span,
                        kind: TsKeywordTypeKind::TsStringKeyword,
                        metadata: Default::default(),
                        tracker: Default::default(),
                    }));
                }
                // At this point rhs cannot be string.
                //
                // Known numeric operations are all handled above

                if self.can_be_casted_to_number_in_rhs(lt.span(), &lt) && self.can_be_casted_to_number_in_rhs(rt.span(), &rt) {
                    return Ok(Type::Keyword(KeywordType {
                        span,
                        kind: TsKeywordTypeKind::TsNumberKeyword,
                        metadata: Default::default(),
                        tracker: Default::default(),
                    }));
                }

                Err(ErrorKind::InvalidBinaryOp {
                    span,
                    op,
                    left: box lt,
                    right: box rt,
                }
                .into())
            }
            op!("*") | op!("/") => {
                no_unknown!();

                Ok(Type::Keyword(KeywordType {
                    span,
                    kind: TsKeywordTypeKind::TsNumberKeyword,
                    metadata: Default::default(),
                    tracker: Default::default(),
                }))
            }

            op!(bin, "-") | op!("<<") | op!(">>") | op!(">>>") | op!("%") | op!("|") | op!("&") | op!("^") | op!("**") => {
                no_unknown!();

                if op == op!("**") {
                    let lt = lt.normalize();
                    let rt = rt.normalize();

                    if !reported_null_or_undefined {
                        self.report_possibly_null_or_undefined(lt.span(), lt).report(&mut self.storage);
                    }

                    if lt.is_kwd(TsKeywordTypeKind::TsVoidKeyword)
                        || lt.is_str()
                        || lt.is_bool()
                        || lt.is_type_lit()
                        || lt.is_type_param()
                        || lt.is_interface()
                        || lt.is_tpl()
                    {
                        self.storage.report(ErrorKind::WrongTypeForLhsOfNumericOperation { span }.into());
                    }

                    if !reported_null_or_undefined {
                        self.report_possibly_null_or_undefined(rt.span(), rt).report(&mut self.storage);
                    }

                    if rt.is_kwd(TsKeywordTypeKind::TsVoidKeyword)
                        || rt.is_str()
                        || rt.is_bool()
                        || rt.is_type_lit()
                        || rt.is_type_param()
                        || rt.is_interface()
                        || rt.is_tpl()
                    {
                        self.storage
                            .report(ErrorKind::WrongTypeForRhsOfNumericOperation { span, ty: box rt.clone() }.into());
                    }
                }

                Ok(Type::Keyword(KeywordType {
                    kind: TsKeywordTypeKind::TsNumberKeyword,
                    span,
                    metadata: Default::default(),
                    tracker: Default::default(),
                }))
            }

            op!("===") | op!("!==") | op!("!=") | op!("==") => Ok(Type::Keyword(KeywordType {
                span,
                kind: TsKeywordTypeKind::TsBooleanKeyword,
                metadata: Default::default(),
                tracker: Default::default(),
            })),

            op!("instanceof") => {
                if !self.is_valid_lhs_of_instanceof(span, &lt) {
                    self.storage.report(
                        ErrorKind::InvalidLhsInInstanceOf {
                            ty: box lt.clone(),
                            span: left.span(),
                        }
                        .into(),
                    )
                }

                Ok(Type::Keyword(KeywordType {
                    span,
                    kind: TsKeywordTypeKind::TsBooleanKeyword,
                    metadata: Default::default(),
                    tracker: Default::default(),
                }))
            }

            op!("<=") | op!("<") | op!(">=") | op!(">") => {
                no_unknown!();

                if !reported_null_or_undefined {
                    let mut check_for_invalid_operand = |ty: &Type| {
                        let res: VResult<_> = try {
                            self.deny_null_or_undefined(ty.span(), ty)?;
                        };
                        res.report(&mut self.storage);
                    };

                    check_for_invalid_operand(&lt);
                    check_for_invalid_operand(&rt);

                    self.validate_relative_comparison_operands(span, op, &lt, &rt)
                        .report(&mut self.storage);
                }

                Ok(Type::Keyword(KeywordType {
                    span,
                    kind: TsKeywordTypeKind::TsBooleanKeyword,
                    metadata: Default::default(),
                    tracker: Default::default(),
                }))
            }

            op!("in") => {
                if self.ctx.in_cond {
                    let left = match &**left {
                        RExpr::Lit(RLit::Str(s)) => Some(s.value.clone()),
                        RExpr::Tpl(t) if t.quasis.len() == 1 => t.quasis[0].cooked.clone().map(|v| (&*v).into()),
                        _ => match lt.normalize() {
                            Type::Lit(LitType { lit: RTsLit::Str(s), .. }) => Some(s.value.clone()),
                            _ => None,
                        },
                    };
                    let name = Name::try_from(&**right).ok();

                    if let Some(name) = name {
                        if let Some(property) = left {
                            let new_ty = self.narrow_types_with_property(span, &rt, &property, None)?.fixed().freezed();

                            self.add_deep_type_fact(span, name.clone(), new_ty.clone(), true);
                            if rt.is_union_type() {
                                self.cur_facts.false_facts.excludes.entry(name).or_default().push(new_ty);
                            }
                        }
                    }
                }

                Ok(Type::Keyword(KeywordType {
                    span,
                    kind: TsKeywordTypeKind::TsBooleanKeyword,
                    metadata: Default::default(),
                    tracker: Default::default(),
                }))
            }

            op!("||") | op!("&&") => {
                no_unknown!();
                let mut lt = lt;
                let mut rt = rt;

                if lt.type_eq(&rt) {
                    return Ok(lt);
                }

                let can_generalize = type_ann.is_none() && !matches!((&**left, &**right), (_, RExpr::Ident(..)));

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

                if let Type::Keyword(KeywordType {
                    kind: TsKeywordTypeKind::TsAnyKeyword,
                    ..
                }) = lt.normalize()
                {
                    return Ok(Type::any(span, Default::default()));
                }

                match op {
                    op!("||") => {
                        if lt.is_never() {
                            return Ok(lt);
                        }

                        if is_str_lit_or_union(&lt) && is_str_lit_or_union(&rt) {
                            return Ok(Type::new_union(span, vec![lt, rt]));
                        }

                        if let Known(v) = lt.as_bool() {
                            return Ok(if v { lt } else { rt });
                        }

                        // if let (_, Known(v)) = left.as_bool() {
                        //     return Ok(if v { lt } else { rt });
                        // }

                        // Remove falsy types from lhs
                        let lt = lt.remove_falsy();

                        return Ok(Type::new_union(span, vec![lt, rt]));
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
                Ok(rt)
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

                let mut ty = Type::new_union(span, vec![lt, rt]);
                if !may_generalize_lt {
                    prevent_generalize(&mut ty);
                }

                Ok(ty)
            }
        }
    }

    fn add_type_facts_for_typeof(&mut self, span: Span, l: &RExpr, r: &RExpr, is_eq: bool, l_ty: &Type, r_ty: &Type) -> VResult<()> {
        if !self.ctx.in_cond {
            return Ok(());
        }

        let c = Comparator { left: l, right: r };

        // Check typeof a === 'string'
        if let Some((Ok(name), (Some(t), Some(f)))) = c.take_if_any_matches(|l, r| {
            if let RExpr::Unary(RUnaryExpr {
                op: op!("typeof"),
                ref arg,
                ..
            }) = l
            {
                //
                let name = Name::try_from(&**arg);
                info!("cond_facts: typeof {:?}", name);
                match r {
                    RExpr::Tpl(RTpl { quasis, .. }) if quasis.len() == 1 => {
                        let value = &quasis[0].cooked.as_ref()?;
                        Some((
                            name,
                            if is_eq {
                                (TypeFacts::typeof_eq(value), TypeFacts::typeof_neq(value))
                            } else {
                                (TypeFacts::typeof_neq(value), TypeFacts::typeof_eq(value))
                            },
                        ))
                    }
                    RExpr::Lit(RLit::Str(RStr { ref value, .. })) => match (TypeFacts::typeof_eq(value), TypeFacts::typeof_neq(value)) {
                        (Some(t), Some(f)) => Some((name, if is_eq { (Some(t), Some(f)) } else { (Some(f), Some(t)) })),
                        (None, None) => {
                            self.storage.report(
                                ErrorKind::NoOverlap {
                                    span,
                                    value: true,
                                    left: box l_ty.clone(),
                                    right: box r_ty.clone(),
                                }
                                .into(),
                            );
                            // A type guard of the form typeof x === s and typeof x !== s,
                            // where s is a string literal with any value but 'string', 'number' or
                            // 'boolean'
                            let name = Name::try_from(&**arg).unwrap();

                            if is_eq {
                                //  - typeof x === s
                                //  removes the primitive types string, number, and boolean from
                                //  the type of x in true facts.
                                self.cur_facts.true_facts.excludes.entry(name).or_default().extend(vec![
                                    Type::Keyword(KeywordType {
                                        span,
                                        kind: TsKeywordTypeKind::TsStringKeyword,
                                        metadata: Default::default(),
                                        tracker: Default::default(),
                                    }),
                                    Type::Keyword(KeywordType {
                                        span,
                                        kind: TsKeywordTypeKind::TsBooleanKeyword,
                                        metadata: Default::default(),
                                        tracker: Default::default(),
                                    }),
                                    Type::Keyword(KeywordType {
                                        span,
                                        kind: TsKeywordTypeKind::TsNumberKeyword,
                                        metadata: Default::default(),
                                        tracker: Default::default(),
                                    }),
                                ]);
                            } else {
                                //  - typeof x !== s
                                //  removes the primitive types string, number, and boolean from
                                //  the type of x in false facts.
                                self.cur_facts.false_facts.excludes.entry(name).or_default().extend(vec![
                                    Type::Keyword(KeywordType {
                                        span,
                                        kind: TsKeywordTypeKind::TsStringKeyword,
                                        metadata: Default::default(),
                                        tracker: Default::default(),
                                    }),
                                    Type::Keyword(KeywordType {
                                        span,
                                        kind: TsKeywordTypeKind::TsBooleanKeyword,
                                        metadata: Default::default(),
                                        tracker: Default::default(),
                                    }),
                                    Type::Keyword(KeywordType {
                                        span,
                                        kind: TsKeywordTypeKind::TsNumberKeyword,
                                        metadata: Default::default(),
                                        tracker: Default::default(),
                                    }),
                                ]);
                            }
                            None
                        }
                        _ => None,
                    },
                    _ => None,
                }
            } else {
                None
            }
        }) {
            // If typeof foo.bar is `string`, `foo` cannot be undefined nor null
            if t != TypeFacts::EQUndefined {
                for idx in 1..name.len() {
                    let sub = name.slice_to(idx);

                    self.cur_facts.true_facts.facts.insert(sub.clone(), TypeFacts::NEUndefinedOrNull);
                }
            }

            if f != TypeFacts::EQUndefined {
                for idx in 1..name.len() {
                    let sub = name.slice_to(idx);

                    self.cur_facts.false_facts.facts.insert(sub.clone(), TypeFacts::NEUndefinedOrNull);
                }
            }

            // Add type facts
            self.cur_facts.true_facts.facts.insert(name.clone(), t);
            self.cur_facts.false_facts.facts.insert(name, f);
        }

        Ok(())
    }

    ///
    /// # Example
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
    fn add_type_facts_for_opt_chains(&mut self, span: Span, l: &RExpr, r: &RExpr, lt: &Type, rt: &Type) -> VResult<()> {
        /// Convert expression to names.
        ///
        /// This may return multiple names if there are optional chaining
        /// expressions.
        fn non_undefined_names(e: &RExpr) -> Vec<Name> {
            match e {
                RExpr::OptChain(ROptChainExpr {
                    base: box ROptChainBase::Member(me),
                    ..
                }) => {
                    let mut names = non_undefined_names(&me.obj);

                    names.extend(e.try_into().ok());
                    names
                }

                RExpr::Member(e) => {
                    let mut names = non_undefined_names(&e.obj);

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

        if let Some((names, r_ty)) = c.take_if_any_matches(|(l, _), (_, r_ty)| match (l, r_ty) {
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
            if !self.can_be_undefined(span, r_ty, false)? {
                for name in names {
                    self.cur_facts.false_facts.facts.insert(name.clone(), TypeFacts::NEUndefined);
                }
            }
        }

        // TODO
        Ok(())
    }

    fn can_compare_with_eq(&mut self, span: Span, disc_ty: &Type, case_ty: &Type) -> VResult<bool> {
        let disc_ty = disc_ty.normalize();
        let case_ty = case_ty.normalize();

        if disc_ty.type_eq(case_ty) {
            return Ok(true);
        }

        if disc_ty.is_num_lit() && case_ty.is_num_lit() {
            return Ok(false);
        }

        if self.ctx.in_switch_case_test {
            if disc_ty.is_intersection() {
                return Ok(true);
            }
        }

        self.has_overlap(
            span,
            disc_ty,
            case_ty,
            CastableOpts {
                allow_assignment_to_param_constraint: true,
                ..Default::default()
            },
        )
    }

    /// We have to check for inheritance.
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
    /// TODO(kdy1): Use Cow
    ///
    /// # Related tests
    ///
    /// ## narrowingConstrainedTypeVariable.ts
    ///
    /// In the test, there's `function f2<T extends C, U extends D>(v: T | U)
    /// {}`.
    ///
    /// If we apply `instanceof C` to `v`, `v` becomes `T`.
    /// Note that `C extends D` and `D extends C` are true because both of `C`
    /// and `D` are empty classes.
    fn narrow_with_instanceof(&mut self, span: Span, ty: Cow<Type>, orig_ty: &Type) -> VResult<Type> {
        let _tracing = dev_span!("narrow_with_instanceof");

        let mut orig_ty = self.normalize(
            Some(span),
            Cow::Borrowed(orig_ty),
            NormalizeTypeOpts {
                preserve_global_this: true,
                preserve_union: true,
                ..Default::default()
            },
        )?;
        orig_ty.freeze();

        let _stack = stack::track(span)?;

        if let Type::Union(orig) = orig_ty.normalize() {
            if ty.is_interface() || ty.is_type_lit() {
                if let Ok(out_result) = self.access_property(
                    span,
                    &ty,
                    &Key::Normal {
                        span,
                        sym: "prototype".into(),
                    },
                    TypeOfMode::RValue,
                    IdCtx::Type,
                    Default::default(),
                ) {
                    if let Ok(result) = self.normalize(Some(span), Cow::Borrowed(&out_result), Default::default()) {
                        let result = result.normalize();
                        if orig.types.iter().any(|ty| ty.type_eq(result)) {
                            return Ok(out_result.freezed());
                        }
                    }
                }
            }
            let new_types = orig
                .types
                .iter()
                .map(|orig_ty| self.narrow_with_instanceof(span, ty.clone(), orig_ty))
                .collect::<Result<Vec<_>, _>>()?;

            for elem in new_types.iter() {
                if orig.types.iter().any(|o_ty| elem.type_eq(o_ty)) {
                    return Ok(elem.to_owned());
                }
            }

            return Ok(Type::new_union(orig.span, new_types).fixed());
        }

        if orig_ty.is_kwd(TsKeywordTypeKind::TsStringKeyword)
            || orig_ty.is_kwd(TsKeywordTypeKind::TsNumberKeyword)
            || orig_ty.is_kwd(TsKeywordTypeKind::TsBooleanKeyword)
        {
            if ty.is_interface() {
                return Ok(Type::never(span, Default::default()));
            }
        }

        if orig_ty.is_any() {
            if ty.is_interface() || ty.is_type_lit() {
                if let Ok(result) = self.access_property(
                    span,
                    &ty,
                    &Key::Normal {
                        span,
                        sym: "prototype".into(),
                    },
                    TypeOfMode::RValue,
                    IdCtx::Type,
                    Default::default(),
                ) {
                    if !result.is_any() {
                        return Ok(result);
                    }
                }
                if let Ok(result) = self.make_instance(span, &ty) {
                    return Ok(result);
                }
            }
            return Ok(ty.into_owned());
        }

        match ty.normalize() {
            Type::ClassDef(ty) => {
                return self.narrow_with_instanceof(
                    span,
                    Cow::Owned(Type::Class(Class {
                        span,
                        def: ty.clone(),
                        metadata: Default::default(),
                        tracker: Default::default(),
                    })),
                    &orig_ty,
                )
            }
            Type::Interface(..) | Type::TypeLit(..) => {
                // Find constructor signature
                if let Some(ty) = self.convert_type_to_type_lit(span, Cow::Borrowed(&ty))? {
                    for m in &ty.members {
                        if let TypeElement::Constructor(c) = m {
                            if let Some(ret_ty) = &c.ret_ty {
                                if ret_ty.is_any() {
                                    return Ok(*ret_ty.clone());
                                }
                                return self
                                    .narrow_with_instanceof(span, Cow::Borrowed(ret_ty), &orig_ty)
                                    .context("tried to narrow constructor return type");
                            }
                        }
                    }
                }
            }
            _ => {}
        }

        if let Some(v) = self.extends(
            span,
            &orig_ty,
            &ty,
            ExtendsOpts {
                disallow_different_classes: true,
                ..Default::default()
            },
        ) {
            if v {
                if let Type::ClassDef(def) = orig_ty.normalize() {
                    return Ok(Type::Class(Class {
                        span,
                        def: def.clone(),
                        metadata: Default::default(),
                        tracker: Default::default(),
                    }));
                }
                return Ok(orig_ty.into_owned());
            } else {
                if let (Type::Interface(..), Type::Interface(..)) = (orig_ty.normalize(), ty.normalize()) {
                    return Ok(ty.into_owned());
                }

                if !self
                    .has_overlap(
                        span,
                        &orig_ty,
                        &ty,
                        CastableOpts {
                            disallow_different_classes: true,
                            disallow_special_assignment_to_empty_class: true,
                            ..Default::default()
                        },
                    )
                    .context("tried to check if overlap exists to calculate the type created by instanceof")?
                {
                    if ty.is_class() {
                        if orig_ty.is_kwd(TsKeywordTypeKind::TsAnyKeyword) || orig_ty.is_kwd(TsKeywordTypeKind::TsObjectKeyword) {
                            return Ok(ty.into_owned());
                        }
                    }
                    return Ok(Type::never(span, Default::default()));
                }
            }
        }

        if let Type::ClassDef(def) = ty.normalize() {
            return Ok(Type::Class(Class {
                span,
                def: def.clone(),
                metadata: Default::default(),
                tracker: Default::default(),
            }));
        }
        Ok(ty.into_owned())
    }

    fn validate_relative_comparison_operands(&mut self, span: Span, op: BinaryOp, l: &Type, r: &Type) -> VResult<()> {
        let marks = self.marks();

        let l = self
            .normalize(
                None,
                Cow::Borrowed(l),
                NormalizeTypeOpts {
                    preserve_global_this: true,
                    preserve_intersection: true,
                    preserve_union: true,
                    ..Default::default()
                },
            )?
            .into_owned()
            .freezed();
        let r = self
            .normalize(
                None,
                Cow::Borrowed(r),
                NormalizeTypeOpts {
                    preserve_global_this: true,
                    preserve_intersection: true,
                    preserve_union: true,
                    ..Default::default()
                },
            )?
            .into_owned()
            .freezed();

        let l = l.normalize();
        let r = r.normalize();

        if let (Type::TypeLit(lt), Type::TypeLit(rt)) = (l, r) {
            // It's an error if type of the parameter of index signature is same but type
            // annotation is different.
            for lm in &lt.members {
                for rm in &rt.members {
                    match (lm, rm) {
                        (TypeElement::Index(lm), TypeElement::Index(rm)) if lm.params.type_eq(&rm.params) => {
                            if let Some(lt) = &lm.type_ann {
                                if let Some(rt) = &rm.type_ann {
                                    if self.assign(span, &mut Default::default(), lt, rt).is_ok()
                                        || self.assign(span, &mut Default::default(), rt, lt).is_ok()
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
                            self.storage.report(
                                ErrorKind::CannotCompareWithOp {
                                    span,
                                    op,
                                    left: box l.clone(),
                                    right: box r.clone(),
                                }
                                .into(),
                            );
                            return Ok(());
                        }
                        _ => {}
                    }
                }
            }
        }

        let l = l.clone().generalize_lit();
        let r = r.clone().generalize_lit();
        self.verify_rel_cmp_operands(span, op, &l, &r)?;

        Ok(())
    }

    fn verify_rel_cmp_operands(&mut self, span: Span, op: BinaryOp, l: &Type, r: &Type) -> VResult<()> {
        let l = l.normalize();
        let r = r.normalize();

        macro_rules! error {
            () => {{
                self.storage.report(
                    ErrorKind::CannotCompareWithOp {
                        span,
                        op,
                        left: box l.clone(),
                        right: box r.clone(),
                    }
                    .into(),
                );
                return Ok(());
            }};
        }

        {
            if l.is_symbol_like() {
                self.storage.report(ErrorKind::NumericOpToSymbol { span: l.span() }.into());
                return Ok(());
            }

            if r.is_symbol_like() {
                self.storage.report(ErrorKind::NumericOpToSymbol { span: r.span() }.into());
                return Ok(());
            }
        }

        if l.type_eq(r) {
            return Ok(());
        }

        if l.is_str_lit() && r.is_str_lit() {
            return Ok(());
        }
        //

        if l.is_type_param() && !r.is_type_param() {
            return Ok(());
        }
        if !l.is_type_param() && r.is_type_param() {
            return Ok(());
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
            if !v {
                error!()
            }
            return Ok(());
        }

        // Basically we depend on assign's behavior, but there's are some corner cases
        // where it's not enough.
        match (l, r) {
            (Type::Class(l), Type::Class(r)) => {
                if l.def.super_class.is_none() && r.def.super_class.is_none() {
                    if l.def.body.is_empty() || r.def.body.is_empty() {
                        error!();
                    }
                }
            }

            (Type::TypeLit(lt), Type::TypeLit(rt)) => {
                if let Ok(Some(v)) = self.can_compare_type_elements_relatively(span, &lt.members, &rt.members) {
                    if v {
                        return Ok(());
                    } else {
                        error!();
                    }
                }
            }
            _ => {}
        }

        if self.has_overlap(span, l, r, Default::default())? {
            return Ok(());
        }

        error!()
    }

    /// Returns Ok(Some(v)) if this method has a special rule to handle type
    /// elements.
    fn can_compare_type_elements_relatively(&mut self, span: Span, l: &[TypeElement], r: &[TypeElement]) -> VResult<Option<bool>> {
        for lm in l {
            for rm in r {
                if let (TypeElement::Method(lm), TypeElement::Method(rm)) = (lm, rm) {
                    if let Ok(()) = self.assign(span, &mut Default::default(), &lm.key.ty(), &rm.key.ty()) {
                        if lm.type_params.as_ref().map(|v| v.params.len()).unwrap_or(0)
                            != rm.type_params.as_ref().map(|v| v.params.len()).unwrap_or(0)
                        {
                            return Ok(Some(true));
                        }

                        let params_res = self.assign_params(
                            &mut Default::default(),
                            &lm.params,
                            &rm.params,
                            AssignOpts {
                                span,
                                ..Default::default()
                            },
                        );

                        if params_res.is_err() {
                            return Ok(Some(true));
                        }

                        let ret_ty_res = match (lm.ret_ty.as_deref(), rm.ret_ty.as_deref()) {
                            (Some(lt), Some(rt)) => self.assign_with_opts(
                                &mut Default::default(),
                                lt,
                                rt,
                                AssignOpts {
                                    span,
                                    allow_unknown_rhs: Some(true),
                                    ..Default::default()
                                },
                            ),
                            _ => Ok(()),
                        };
                    }
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

        if ty.is_any() || ty.is_unknown() || ty.is_kwd(TsKeywordTypeKind::TsObjectKeyword) {
            return true;
        }

        match ty {
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

        // TODO(kdy1): We should assign this to builtin interface `Function`.
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
                type_name: RTsEntityName::Ident(RIdent {
                    sym: js_word!("Object"), ..
                }),
                ..
            })
            | Type::Symbol(..) => {
                self.storage.report(
                    ErrorKind::InvalidRhsInInstanceOf {
                        span,
                        ty: box type_for_error.clone(),
                    }
                    .into(),
                );
            }

            Type::TypeLit(e) if e.members.is_empty() => {
                self.storage.report(
                    ErrorKind::InvalidRhsInInstanceOf {
                        span,
                        ty: box type_for_error.clone(),
                    }
                    .into(),
                );
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
                    tracker: Default::default(),
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
                        type_name: RTsEntityName::Ident(RIdent::new("Function".into(), span.with_ctxt(SyntaxContext::empty()))),
                        type_args: None,
                        metadata: Default::default(),
                        tracker: Default::default(),
                    }),
                    &ty,
                ) {
                    self.storage.report(
                        ErrorKind::InvalidRhsInInstanceOf {
                            span,
                            ty: box type_for_error.clone(),
                        }
                        .into(),
                    );
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
    ///
    /// Returns `(name, true_fact, false_fact)`.
    fn calc_type_facts_for_equality(&mut self, name: Name, equals_to: &Type) -> VResult<(Name, Type, Vec<Type>)> {
        let span = equals_to.span();

        let mut id: RIdent = name.top().into();
        id.span.lo = span.lo;
        id.span.hi = span.hi;

        if name.len() == 1 {
            let orig_ty = self.type_of_var(&id, TypeOfMode::RValue, None)?;

            let narrowed = self
                .narrow_with_equality(&orig_ty, equals_to)
                .context("tried to narrow type with equality")?
                .freezed();

            return Ok((name, narrowed.clone(), vec![narrowed]));
        }

        let eq_ty = equals_to.normalize();

        // We create a type fact for `foo` in `if (foo.type === 'bar');`

        let ids = name.inner();

        let prop = Key::Normal {
            span,
            sym: name.last().clone(),
        };

        let ty = self.type_of_name(span, &name.slice_to(name.len() - 1), TypeOfMode::RValue, None)?;

        let ty = self.normalize(Some(span), Cow::Owned(ty), Default::default())?.into_owned();

        if let Type::Union(u) = ty.normalize() {
            let mut has_undefined = false;
            let mut candidates = vec![];
            let mut excluded = vec![];
            for ty in &u.types {
                // TODO(kdy1): Enable this logic iff it's an optional chaining
                if ty.is_undefined() {
                    has_undefined = true;
                    continue;
                }

                let prop_res = self.access_property(span, ty, &prop, TypeOfMode::RValue, IdCtx::Var, Default::default());

                if let Ok(prop_ty) = prop_res {
                    let prop_ty = self.normalize(Some(prop_ty.span()), Cow::Owned(prop_ty), Default::default())?;
                    let possible = match prop_ty.normalize() {
                        // Type parameters might have same value.
                        Type::Param(..) => true,
                        _ => {
                            (equals_to.is_undefined() && prop_ty.contains_undefined())
                                || (if equals_to.is_null_or_undefined() {
                                    prop_ty.type_eq(equals_to)
                                } else {
                                    self.has_overlap(span, &prop_ty, equals_to, CastableOpts { ..Default::default() })?
                                })
                        }
                    };
                    if possible {
                        if prop_ty.iter_union().any(|prop_ty| prop_ty.type_eq(equals_to)) {
                            excluded.push(ty.clone())
                        }

                        candidates.push(ty.clone())
                    }
                }
            }
            let actual = name.slice_to(name.len() - 1);

            if has_undefined && candidates.is_empty() {
                return Ok((
                    actual,
                    Type::Keyword(KeywordType {
                        span: u.span,
                        kind: TsKeywordTypeKind::TsUndefinedKeyword,
                        metadata: Default::default(),
                        tracker: Default::default(),
                    }),
                    excluded.freezed(),
                ));
            }
            let ty = Type::new_union(span, candidates).freezed();
            return Ok((actual, ty, excluded.freezed()));
        }

        Ok((name, equals_to.clone(), vec![equals_to.clone()]))
    }

    /// Returns new type of the variable after comparison with `===`.
    ///
    /// # Parameters
    ///
    /// ## orig_ty
    ///
    /// Original type of the variable.
    fn narrow_with_equality(&mut self, orig_ty: &Type, equals_to: &Type) -> VResult<Type> {
        let span = equals_to.span();

        if orig_ty.type_eq(equals_to) {
            return Ok(orig_ty.clone());
        }

        let orig_ty = self.normalize(Some(span), Cow::Borrowed(orig_ty), Default::default())?;
        let equals_to = self.normalize(Some(span), Cow::Borrowed(equals_to), Default::default())?;

        if orig_ty.type_eq(&equals_to) {
            return Ok(orig_ty.into_owned());
        }

        // Exclude nevers.
        if let Type::Union(orig) = &*orig_ty {
            let mut types = vec![];
            // We
            for orig in &orig.types {
                let new_ty = self
                    .narrow_with_equality(orig, &equals_to)
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
                tracker: Default::default(),
            })
            .fixed());
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
            op!("||") | op!("&&") => {
                if let Type::Keyword(KeywordType {
                    kind: TsKeywordTypeKind::TsVoidKeyword,
                    ..
                }) = lt.normalize()
                {
                    errors.push(ErrorKind::TS1345 { span }.into())
                }
            }

            op!("*") | op!("/") | op!("%") | op!(bin, "-") | op!("<<") | op!(">>") | op!(">>>") | op!("&") | op!("^") | op!("|") => {
                let mut check = |ty: &Type, is_left| {
                    if ty.is_any() {
                        return;
                    }
                    if self.can_be_casted_to_number_in_rhs(ty.span(), ty) {
                        return;
                    }

                    match ty.normalize() {
                        Type::Keyword(KeywordType {
                            span,
                            kind:
                                TsKeywordTypeKind::TsUndefinedKeyword | TsKeywordTypeKind::TsNullKeyword | TsKeywordTypeKind::TsUnknownKeyword,
                            ..
                        }) => {}

                        _ => errors.push(if is_left {
                            ErrorKind::WrongTypeForLhsOfNumericOperation { span: ty.span() }.into()
                        } else {
                            ErrorKind::WrongTypeForRhsOfNumericOperation {
                                span: ty.span(),
                                ty: box ty.clone(),
                            }
                            .into()
                        }),
                    }
                };

                if (op == op!("&") || op == op!("^") || op == op!("|"))
                    && matches!(
                        lt.normalize(),
                        Type::Keyword(KeywordType {
                            kind: TsKeywordTypeKind::TsBooleanKeyword,
                            ..
                        }) | Type::Lit(LitType { lit: RTsLit::Bool(..), .. })
                    )
                    && matches!(
                        rt.normalize(),
                        Type::Keyword(KeywordType {
                            kind: TsKeywordTypeKind::TsBooleanKeyword,
                            ..
                        }) | Type::Lit(LitType { lit: RTsLit::Bool(..), .. })
                    )
                {
                    errors.push(ErrorKind::TS2447 { span }.into());
                } else {
                    check(lt, true);
                    check(rt, false);
                }
            }

            op!("in") => {
                match lt.normalize() {
                    Type::Keyword(KeywordType {
                        kind: TsKeywordTypeKind::TsNullKeyword,
                        ..
                    }) => {
                        self.storage.report(ErrorKind::UndefinedOrNullIsNotValidOperand { span }.into());
                    }

                    Type::Keyword(KeywordType {
                        kind: TsKeywordTypeKind::TsUndefinedKeyword,
                        ..
                    }) => {
                        self.storage.report(ErrorKind::UndefinedOrNullIsNotValidOperand { span }.into());
                    }

                    ty => {
                        if let Err(err) = self.assign_with_opts(
                            &mut Default::default(),
                            &Type::Union(Union {
                                span,
                                types: vec![
                                    Type::Keyword(KeywordType {
                                        span,
                                        kind: TsKeywordTypeKind::TsStringKeyword,
                                        metadata: Default::default(),
                                        tracker: Default::default(),
                                    }),
                                    Type::Keyword(KeywordType {
                                        span,
                                        kind: TsKeywordTypeKind::TsNumberKeyword,
                                        metadata: Default::default(),
                                        tracker: Default::default(),
                                    }),
                                    Type::Keyword(KeywordType {
                                        span,
                                        kind: TsKeywordTypeKind::TsSymbolKeyword,
                                        metadata: Default::default(),
                                        tracker: Default::default(),
                                    }),
                                ],
                                metadata: Default::default(),
                                tracker: Default::default(),
                            })
                            .freezed(),
                            ty,
                            AssignOpts {
                                span: ls,
                                ..Default::default()
                            },
                        ) {
                            errors.push(err.context("tried to assign for LHS of `in` operator"));
                        } else if !self.is_valid_lhs_of_in(ty) {
                            errors.push(ErrorKind::InvalidLhsOfInOperator { span: ls }.into());
                        }
                    }
                }

                match rt.normalize() {
                    Type::Keyword(KeywordType {
                        kind: TsKeywordTypeKind::TsNullKeyword,
                        ..
                    }) => {
                        self.storage.report(ErrorKind::UndefinedOrNullIsNotValidOperand { span }.into());
                    }

                    Type::Keyword(KeywordType {
                        kind: TsKeywordTypeKind::TsUndefinedKeyword,
                        ..
                    }) => {
                        self.storage.report(ErrorKind::UndefinedOrNullIsNotValidOperand { span }.into());
                    }

                    _ => {
                        if let Err(err) = self.assign_with_opts(
                            &mut Default::default(),
                            &Type::Keyword(KeywordType {
                                span,
                                kind: TsKeywordTypeKind::TsObjectKeyword,
                                metadata: KeywordTypeMetadata::default(),
                                tracker: Default::default(),
                            }),
                            rt,
                            AssignOpts {
                                span: rs,
                                ..Default::default()
                            },
                        ) {
                            errors.push(err.context("tried to assign for RHS of `in` operator"));
                        } else if !self.is_valid_rhs_of_in(rs, rt) {
                            errors.push(
                                ErrorKind::InvalidRhsForInOperator {
                                    span: rs,
                                    ty: box rt.clone(),
                                }
                                .into(),
                            )
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
                lit: RTsLit::Number(..), ..
            })
            | Type::Lit(LitType { lit: RTsLit::Str(..), .. })
            | Type::Enum(..)
            | Type::EnumVariant(..)
            | Type::Param(..)
            | Type::Index(..)
            | Type::Symbol(..)
            | Type::Tpl(..) => true,

            Type::Union(ref u) => u.types.iter().all(|ty| self.is_valid_lhs_of_in(ty)),

            _ => false,
        }
    }

    fn is_valid_rhs_of_in(&mut self, span: Span, ty: &Type) -> bool {
        if ty.is_any() || ty.is_never() {
            return true;
        }

        let ty = match self.normalize(
            Some(span),
            Cow::Borrowed(ty),
            NormalizeTypeOpts {
                preserve_mapped: true,
                ..Default::default()
            },
        ) {
            Ok(v) => v,
            Err(_) => return true,
        };

        match ty.normalize() {
            Type::This(..)
            | Type::Class(..)
            | Type::ClassDef(..)
            | Type::TypeLit(..)
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
            Type::Union(ref u) => u.types.iter().all(|ty| self.is_valid_rhs_of_in(span, ty)),

            _ => false,
        }
    }

    #[extra_validator]
    fn report_errors_for_mixed_nullish_coalescing(&mut self, e: &RBinExpr) {
        fn search(span: Span, op: BinaryOp, operand: &RExpr) -> VResult<()> {
            if op == op!("??") {
                if let RExpr::Bin(bin) = operand {
                    if bin.op == op!("||") || bin.op == op!("&&") {
                        return Err(ErrorKind::NullishCoalescingMixedWithLogicalWithoutParen { span }.into());
                    }
                }
            } else if op == op!("||") || op == op!("&&") {
                if let RExpr::Bin(bin) = operand {
                    if bin.op == op!("??") {
                        return Err(ErrorKind::NullishCoalescingMixedWithLogicalWithoutParen { span }.into());
                    }
                }
            }

            Ok(())
        }

        search(e.span, e.op, &e.left)?;
        search(e.span, e.op, &e.right)?;
    }

    fn get_additional_exclude_target(
        &mut self,
        span: Span,
        origin_ty: Type,
        r: &Type,
        name: Name,
        is_loose_comparison: bool,
    ) -> FxHashMap<Name, Vec<Type>> {
        let mut additional_target: FxHashMap<Name, Vec<Type>> = Default::default();

        if let Type::Union(Union { types, .. }) = origin_ty.normalize() {
            for ty in types {
                match ty.normalize() {
                    Type::Interface(interface) => {
                        if let Ok(Some(tl)) = self.convert_type_to_type_lit(span, Cow::Borrowed(ty)) {
                            let tl = tl.into_owned();
                            self.get_additional_exclude_target_for_type_lit(
                                span,
                                ty,
                                &tl,
                                r,
                                name.clone(),
                                &mut additional_target,
                                is_loose_comparison,
                            );
                        }
                    }
                    Type::TypeLit(tl) => {
                        self.get_additional_exclude_target_for_type_lit(
                            span,
                            ty,
                            tl,
                            r,
                            name.clone(),
                            &mut additional_target,
                            is_loose_comparison,
                        );
                    }
                    Type::Tuple(tu @ Tuple { elems, .. }) => {
                        if elems.iter().any(|elem| (*elem.ty).type_eq(r)) {
                            for elem in tu.elems.iter() {
                                if let Some(RPat::Ident(RBindingIdent {
                                    id: RIdent { sym, .. }, ..
                                })) = &elem.label
                                {
                                    let l_name = Name::new(sym.clone(), name.get_ctxt());
                                    if name == l_name {
                                        continue;
                                    }
                                    let mut temp_vec = if let Some(temp_vec) = additional_target.get(&l_name) {
                                        temp_vec.clone()
                                    } else {
                                        if is_loose_comparison {
                                            vec![
                                                Type::Keyword(KeywordType {
                                                    span,
                                                    kind: TsKeywordTypeKind::TsNullKeyword,
                                                    metadata: Default::default(),
                                                    tracker: Default::default(),
                                                }),
                                                Type::Keyword(KeywordType {
                                                    span,
                                                    kind: TsKeywordTypeKind::TsUndefinedKeyword,
                                                    metadata: Default::default(),
                                                    tracker: Default::default(),
                                                }),
                                            ]
                                        } else {
                                            vec![]
                                        }
                                    };
                                    temp_vec.push((*elem.ty).clone().freezed());
                                    additional_target.insert(l_name, temp_vec);
                                }
                            }
                        }
                    }
                    _ => {}
                }
            }
        }
        additional_target
    }

    fn get_additional_exclude_target_for_type_lit(
        &mut self,
        span: Span,
        origin_ty: &Type,
        tl: &TypeLit,
        r: &Type,
        name: Name,
        additional_target: &mut FxHashMap<Name, Vec<Type>>,
        is_loose_comparison: bool,
    ) {
        if let Ok(property) = self.access_property(
            span,
            origin_ty.normalize(),
            &Key::Normal {
                span,
                sym: name.top().sym().clone(),
            },
            TypeOfMode::RValue,
            IdCtx::Type,
            Default::default(),
        ) {
            if property.type_eq(r) {
                for m in tl.members.iter() {
                    if let Some(key) = m.non_computed_key() {
                        let l_name = Name::new(key.clone(), name.get_ctxt());
                        if name == l_name {
                            continue;
                        }
                        if let Some(act_ty) = m.get_type().cloned() {
                            let mut temp_vec = if let Some(temp_vec) = additional_target.get(&l_name) {
                                temp_vec.clone()
                            } else {
                                if is_loose_comparison {
                                    vec![
                                        Type::Keyword(KeywordType {
                                            span,
                                            kind: TsKeywordTypeKind::TsNullKeyword,
                                            metadata: Default::default(),
                                            tracker: Default::default(),
                                        }),
                                        Type::Keyword(KeywordType {
                                            span,
                                            kind: TsKeywordTypeKind::TsUndefinedKeyword,
                                            metadata: Default::default(),
                                            tracker: Default::default(),
                                        }),
                                    ]
                                } else {
                                    vec![]
                                }
                            };
                            temp_vec.push(act_ty.freezed());
                            additional_target.insert(l_name, temp_vec);
                        }
                    }
                }
            }
        }
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
        RExpr::Member(RMemberExpr { obj, prop, .. }) => {
            let mut name = extract_name_for_assignment(obj, is_exact_eq)?;

            name.push(match prop {
                RMemberProp::Ident(i) => i.sym.clone(),
                RMemberProp::Computed(RComputedPropName {
                    expr: box RExpr::Lit(RLit::Str(s)),
                    ..
                }) => s.value.clone(),
                _ => return None,
            });

            Some(name)
        }

        _ => Name::try_from(e).ok(),
    }
}

fn is_str_like_for_addition(t: &Type) -> bool {
    match t.normalize() {
        Type::Lit(LitType { lit: RTsLit::Str(..), .. }) | Type::Tpl(..) => true,
        Type::Keyword(KeywordType {
            kind: TsKeywordTypeKind::TsStringKeyword,
            ..
        }) => true,
        Type::Intersection(Intersection { types, .. }) => types.iter().any(is_str_like_for_addition),
        Type::Union(Union { types, .. }) => types.iter().all(is_str_like_for_addition),
        _ => false,
    }
}
