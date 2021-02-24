use super::super::{
    util::{Comparator, ResultExt},
    Analyzer,
};
use super::TypeOfMode;
use crate::analyzer::assign::AssignOpts;
use crate::ty::type_facts::TypeFactsHandler;
use crate::util::type_ext::TypeVecExt;
use crate::{
    analyzer::{Ctx, ScopeKind},
    ty::{Operator, Type, TypeExt},
    type_facts::TypeFacts,
    util::{is_str_lit_or_union, is_str_or_union, RemoveTypes},
    validator,
    validator::ValidateWith,
    ValidationResult,
};
use rnode::FoldWith;
use stc_ts_ast_rnode::RBinExpr;
use stc_ts_ast_rnode::RExpr;
use stc_ts_ast_rnode::RIdent;
use stc_ts_ast_rnode::RLit;
use stc_ts_ast_rnode::RPat;
use stc_ts_ast_rnode::RPatOrExpr;
use stc_ts_ast_rnode::RStr;
use stc_ts_ast_rnode::RTpl;
use stc_ts_ast_rnode::RTsEntityName;
use stc_ts_ast_rnode::RTsKeywordType;
use stc_ts_ast_rnode::RTsLit;
use stc_ts_ast_rnode::RTsLitType;
use stc_ts_ast_rnode::RUnaryExpr;
use stc_ts_errors::DebugExt;
use stc_ts_errors::Error;
use stc_ts_errors::Errors;
use stc_ts_file_analyzer_macros::extra_validator;
use stc_ts_types::name::Name;
use stc_ts_types::Intersection;
use stc_ts_types::ModuleId;
use stc_ts_types::Ref;
use stc_ts_types::TypeElement;
use stc_ts_types::Union;
use std::borrow::Cow;
use std::collections::hash_map::Entry;
use std::convert::TryFrom;
use swc_atoms::js_word;
use swc_common::SyntaxContext;
use swc_common::TypeEq;
use swc_common::{Span, Spanned};
use swc_ecma_ast::*;
use swc_ecma_utils::Value::Known;

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

        let prev_facts = self.cur_facts.clone();

        self.check_for_mixed_nullish_coalescing(e);

        let mut errors = vec![];

        let ctx = Ctx {
            should_store_truthy_for_access: false,
            ..self.ctx
        };

        let child_ctxt = (
            TypeOfMode::RValue,
            None,
            match op {
                op!("??") | op!("&&") | op!("||") => type_ann,
                _ => None,
            },
        );

        let lt = {
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
                ty = self.with_ctx(ctx).expand_fully(span, ty, true)?;
            }
            let span = ty.span();
            ty.reposition(left.span());

            Ok(ty)
        })
        .store(&mut errors);

        let true_facts_for_rhs = if op == op!("&&") {
            // We need a new virtual scope.
            self.cur_facts.true_facts.take()
        } else if op == op!("||") {
            self.cur_facts.false_facts.clone()
        } else {
            Default::default()
        };

        let mut lhs_facts = if op == op!("||") {
            self.cur_facts.take()
        } else {
            Default::default()
        };

        self.cur_facts = prev_facts;

        let rhs = self
            .with_child(
                ScopeKind::Flow,
                true_facts_for_rhs,
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
                                        truthy_lt = lt.clone().map(|ty| {
                                            ty.fold_with(&mut TypeFactsHandler {
                                                analyzer: child,
                                                facts: TypeFacts::Truthy,
                                            })
                                        });
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
                            ty = child.with_ctx(ctx).expand_fully(span, ty, true)?;
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

        self.validate_bin_inner(span, op, lt.as_ref(), rt.as_ref());

        if op == op!("||") {
            for (k, type_fact) in lhs_facts.true_facts.facts.drain() {
                match self.cur_facts.true_facts.facts.entry(k) {
                    // (typeof a === 'string' || typeof a === 'number')
                    Entry::Occupied(mut e) => {
                        *e.get_mut() |= type_fact;
                    }
                    // (typeof a === 'string' || a !== foo)
                    Entry::Vacant(..) => {}
                }
            }

            self.cur_facts += lhs_facts;
        }

        let (lt, rt): (Type, Type) = match (lt, rt) {
            (Some(l), Some(r)) => (l, r),
            _ => return Err(Error::Errors { span, errors }),
        };

        // Handle control-flow based typing
        match op {
            op!("===") | op!("!==") | op!("==") | op!("!=") => {
                let is_eq = op == op!("===") || op == op!("==");

                let c = Comparator {
                    left: &**left,
                    right: &**right,
                };

                // Check typeof a === 'string'
                {
                    match c.take_if_any_matches(|l, r| match l {
                        RExpr::Unary(RUnaryExpr {
                            op: op!("typeof"),
                            ref arg,
                            ..
                        }) => {
                            //
                            let name = Name::try_from(&**arg);
                            slog::info!(self.logger, "cond_facts: typeof {:?}", name);
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
                }

                // Try narrowing type
                let c = Comparator {
                    left: (&**left, lt.normalize()),
                    right: (&**right, rt.normalize()),
                };

                if !self.has_overlap(span, &lt, &rt)? {
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
                    Type::Keyword(RTsKeywordType {
                        kind: TsKeywordTypeKind::TsUnknownKeyword,
                        ..
                    }) => {
                        //
                        Some((Name::try_from(l), r_ty))
                    }
                    _ => None,
                }) {
                    Some((Ok(name), ty)) => {
                        if is_eq {
                            self.add_deep_type_fact(name.clone(), ty.clone(), false);
                        } else {
                            self.add_deep_type_fact(name.clone(), ty.clone(), true);
                        }
                    }
                    _ => {}
                }

                match c.take_if_any_matches(|(l, _), (_, r_ty)| match (l, r_ty) {
                    (
                        RExpr::Ident(RIdent {
                            sym: js_word!("undefined"),
                            ..
                        }),
                        _,
                    )
                    | (
                        RExpr::Ident(RIdent {
                            sym: js_word!("null"), ..
                        }),
                        _,
                    ) => None,

                    (l, r) => Some((extract_name_for_assignment(l)?, r_ty)),
                }) {
                    Some((l, r)) => {
                        if self.ctx.in_cond && op == op!("===") {
                            let mut r = r.clone();
                            self.cur_facts
                                .false_facts
                                .excludes
                                .entry(l.clone())
                                .or_default()
                                .push(r.clone());

                            self.prevent_generalize(&mut r);
                            self.add_deep_type_fact(l, r, true);
                        } else if self.ctx.in_cond && !is_eq {
                            // Remove from union
                            let mut r = r.clone();
                            self.cur_facts
                                .true_facts
                                .excludes
                                .entry(l.clone())
                                .or_default()
                                .push(r.clone());

                            self.prevent_generalize(&mut r);
                            self.add_deep_type_fact(l, r, false);
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
                        let orig_ty = self.type_of_var(i, TypeOfMode::RValue, None)?;

                        //
                        let ty = self.validate_rhs_of_instanceof(span, rt.clone());

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

                        if !cannot_narrow {
                            let narrowed_ty = self
                                .narrow_with_instanceof(span, ty.clone(), &orig_ty)
                                .context("tried to narrow type with instanceof")?
                                .cheap();

                            // TODO(kdy1): Maybe we need to check for intersection or union
                            if orig_ty.is_type_param() {
                                self.cur_facts.true_facts.vars.insert(
                                    Name::from(i),
                                    Type::Intersection(Intersection {
                                        span,
                                        types: vec![orig_ty, narrowed_ty],
                                    })
                                    .cheap(),
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
                    Type::Keyword(RTsKeywordType {
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
                    Type::Keyword(RTsKeywordType {
                        kind: TsKeywordTypeKind::TsUnknownKeyword,
                        ..
                    }) => Some(()),

                    _ => None,
                }) {
                    debug_assert!(!span.is_dummy());
                    return Err(Error::Unknown { span });
                }

                if lt.is_num() && rt.is_num() {
                    return Ok(Type::Keyword(RTsKeywordType {
                        span,
                        kind: TsKeywordTypeKind::TsNumberKeyword,
                    }));
                }

                if let Some(()) = c.take_if_any_matches(|(_, lt), (_, _)| match *lt {
                    Type::Keyword(RTsKeywordType {
                        kind: TsKeywordTypeKind::TsStringKeyword,
                        ..
                    })
                    | Type::Lit(RTsLitType {
                        lit: RTsLit::Str(..), ..
                    }) => Some(()),

                    _ => None,
                }) {
                    return Ok(Type::Keyword(RTsKeywordType {
                        span,
                        kind: TsKeywordTypeKind::TsStringKeyword,
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
                    return Ok(Type::Keyword(RTsKeywordType { span, kind }));
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
                    Type::Keyword(RTsKeywordType {
                        kind: TsKeywordTypeKind::TsUndefinedKeyword,
                        ..
                    })
                    | Type::Keyword(RTsKeywordType {
                        kind: TsKeywordTypeKind::TsNullKeyword,
                        ..
                    }) => true,

                    _ => false,
                }) {
                    return Err(Error::TS2365 { span });
                }

                if is_str_or_union(&lt) || is_str_or_union(&rt) {
                    return Ok(Type::Keyword(RTsKeywordType {
                        span,
                        kind: TsKeywordTypeKind::TsStringKeyword,
                    }));
                }
                // At this point rhs cannot be string.
                //
                // Known numeric operations are all handled above

                if self.can_be_casted_to_number_in_rhs(lt.span(), &lt)
                    && self.can_be_casted_to_number_in_rhs(rt.span(), &rt)
                {
                    return Ok(Type::Keyword(RTsKeywordType {
                        span,
                        kind: TsKeywordTypeKind::TsNumberKeyword,
                    }));
                }

                return Err(Error::InvalidBinaryOp { span, op });
            }
            op!("*") | op!("/") => {
                no_unknown!();

                return Ok(Type::Keyword(RTsKeywordType {
                    span,
                    kind: TsKeywordTypeKind::TsNumberKeyword,
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

                return Ok(Type::Keyword(RTsKeywordType {
                    kind: TsKeywordTypeKind::TsNumberKeyword,
                    span,
                }));
            }

            op!("===") | op!("!==") | op!("!=") | op!("==") => {
                return Ok(Type::Keyword(RTsKeywordType {
                    span,
                    kind: TsKeywordTypeKind::TsBooleanKeyword,
                }));
            }

            op!("instanceof") => {
                if !self.is_valid_lhs_of_instanceof(span, &lt) {
                    self.storage.report(Error::InvalidLhsInInstanceOf {
                        ty: box lt.clone(),
                        span: left.span(),
                    })
                }

                return Ok(Type::Keyword(RTsKeywordType {
                    span,
                    kind: TsKeywordTypeKind::TsBooleanKeyword,
                }));
            }

            op!("<=") | op!("<") | op!(">=") | op!(">") => {
                no_unknown!();

                let mut check_for_invalid_operand = |ty: &Type| match ty.normalize() {
                    Type::Keyword(RTsKeywordType {
                        span,
                        kind: TsKeywordTypeKind::TsUndefinedKeyword,
                    }) => {
                        self.storage.report(Error::ObjectIsPossiblyUndefined { span: *span });
                    }

                    Type::Keyword(RTsKeywordType {
                        span,
                        kind: TsKeywordTypeKind::TsNullKeyword,
                    }) => {
                        self.storage.report(Error::ObjectIsPossiblyNull { span: *span });
                    }

                    _ => {}
                };

                check_for_invalid_operand(&lt);
                check_for_invalid_operand(&rt);

                self.validate_relative_comparison_operands(span, op, &lt, &rt);

                return Ok(Type::Keyword(RTsKeywordType {
                    span,
                    kind: TsKeywordTypeKind::TsBooleanKeyword,
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
                            let mut new_ty = self.filter_types_with_property(&rt, &property)?.cheap();

                            self.add_deep_type_fact(name.clone(), new_ty.clone(), true);
                        }
                    }
                }

                return Ok(Type::Keyword(RTsKeywordType {
                    span,
                    kind: TsKeywordTypeKind::TsBooleanKeyword,
                }));
            }

            op!("||") | op!("&&") => {
                no_unknown!();
                let mut lt = lt;
                let mut rt = rt;

                if self.may_generalize(&lt) {
                    lt = lt.generalize_lit();
                }
                if self.may_generalize(&rt) {
                    rt = rt.generalize_lit();
                }

                match lt.normalize() {
                    Type::Keyword(RTsKeywordType {
                        kind: TsKeywordTypeKind::TsAnyKeyword,
                        ..
                    }) => return Ok(Type::any(span)),

                    _ => {}
                }

                match op {
                    op!("||") => {
                        if lt.is_never() {
                            return Ok(lt);
                        }

                        if lt.type_eq(&rt) {
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
                if lt.normalize().type_eq(rt.normalize()) {
                    return Ok(lt);
                }

                let mut ty = Type::union(vec![lt, rt]);
                if !may_generalize_lt {
                    self.prevent_generalize(&mut ty);
                }

                Ok(ty)
            }
        }
    }
}

impl Analyzer<'_, '_> {
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
    fn narrow_with_instanceof(&mut self, span: Span, ty: Type, orig_ty: &Type) -> ValidationResult {
        let orig_ty = orig_ty.normalize();

        match orig_ty {
            Type::Ref(..) => {
                let orig_ty = self.expand_top_ref(span, Cow::Borrowed(orig_ty))?;
                return self.narrow_with_instanceof(span, ty, &orig_ty);
            }

            Type::Union(orig) => {
                let mut new_types = orig
                    .types
                    .iter()
                    .map(|orig_ty| self.narrow_with_instanceof(span, ty.clone(), orig_ty))
                    .collect::<Result<Vec<_>, _>>()?;

                new_types.retain(|ty| !ty.is_never());

                new_types.dedup_type();

                return Ok(Type::Union(Union {
                    span: orig.span,
                    types: new_types,
                }));
            }

            _ => {}
        }

        if orig_ty.is_kwd(TsKeywordTypeKind::TsStringKeyword)
            || orig_ty.is_kwd(TsKeywordTypeKind::TsNumberKeyword)
            || orig_ty.is_kwd(TsKeywordTypeKind::TsBooleanKeyword)
        {
            if ty.normalize().is_interface() {
                return Ok(Type::never(span));
            }
        }

        if let Some(v) = self.extends(span, orig_ty, &ty) {
            if v {
                return Ok(orig_ty.clone());
            } else {
                match (orig_ty, ty.normalize()) {
                    (Type::Interface(..), Type::Interface(..)) => return Ok(ty),
                    _ => {}
                }

                if !self
                    .has_overlap(span, orig_ty, &ty)
                    .context("tried to check if overlap exists to calculate the type created by instanceof")?
                {
                    return Ok(Type::never(span));
                }
            }
        }

        Ok(ty)
    }

    #[extra_validator]
    fn validate_relative_comparison_operands(&mut self, span: Span, op: BinaryOp, l: &Type, r: &Type) {
        let l = l.normalize();
        let r = r.normalize();

        match (l, r) {
            (Type::Ref(..), _) => {
                if let Ok(l) = self.expand_top_ref(l.span(), Cow::Borrowed(l)) {
                    return self.validate_relative_comparison_operands(span, op, &l, r);
                }
            }
            (l, Type::Ref(..)) => {
                if let Ok(r) = self.expand_top_ref(r.span(), Cow::Borrowed(r)) {
                    return self.validate_relative_comparison_operands(span, op, l, &r);
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
                                        if self.assign(&lt, &rt, span).is_ok() || self.assign(&rt, &lt, span).is_ok() {
                                            continue;
                                        }
                                    } else {
                                        continue;
                                    }
                                } else {
                                    continue;
                                }
                                //
                                self.storage.report(Error::CannotCompareWithOp { span, op });
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

        self.storage.report(Error::CannotCompareWithOp { span, op });
    }

    fn can_compare_relatively(&mut self, span: Span, l: &Type, r: &Type) -> ValidationResult<bool> {
        let l = l.normalize();
        let r = r.normalize();

        if l.type_eq(r) {
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
                if l.super_class.is_none() && r.super_class.is_none() {
                    if l.body.is_empty() || r.body.is_empty() {
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

        self.has_overlap(span, &l, &r)
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
                        if let Ok(()) = self.assign(&lm.key.ty(), &rm.key.ty(), span) {
                            if lm.type_params.as_ref().map(|v| v.params.len()).unwrap_or(0)
                                != rm.type_params.as_ref().map(|v| v.params.len()).unwrap_or(0)
                            {
                                return Ok(Some(true));
                            }

                            let params_res = self.assign_params(
                                AssignOpts {
                                    span,
                                    allow_unknown_rhs: false,
                                    allow_assignment_to_param: false,
                                    allow_unknown_type: false,
                                },
                                &lm.params,
                                &rm.params,
                            );

                            if params_res.is_err() {
                                return Ok(Some(true));
                            }

                            let ret_ty_res = match (lm.ret_ty.as_deref(), rm.ret_ty.as_deref()) {
                                (Some(lt), Some(rt)) => self.assign_with_opts(
                                    AssignOpts {
                                        span,
                                        allow_unknown_rhs: true,
                                        allow_assignment_to_param: false,
                                        allow_unknown_type: false,
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
            | Type::Ref(..) => true,

            Type::Intersection(ty) => ty.types.iter().all(|ty| self.is_valid_lhs_of_instanceof(span, ty)),

            Type::Union(ty) => ty.types.iter().any(|ty| self.is_valid_lhs_of_instanceof(span, ty)),

            _ => false,
        }
    }

    /// The right operand to be of type Any or a subtype of the 'Function'
    /// interface type.
    fn validate_rhs_of_instanceof(&mut self, span: Span, ty: Type) -> Type {
        if ty.is_any() {
            return ty;
        }

        // TODO: We should assign this to builtin interface `Function`.
        match ty.normalize() {
            // Error
            Type::Keyword(RTsKeywordType {
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
            })
            | Type::Keyword(RTsKeywordType {
                kind: TsKeywordTypeKind::TsVoidKeyword,
                ..
            })
            | Type::Lit(..)
            | Type::ClassInstance(..)
            | Type::Ref(Ref {
                type_name:
                    RTsEntityName::Ident(RIdent {
                        sym: js_word!("Object"),
                        ..
                    }),
                ..
            }) => {
                self.storage.report(Error::InvalidRhsInInstanceOf {
                    span,
                    ty: box ty.clone(),
                });
            }

            Type::TypeLit(e) if e.members.is_empty() => {
                self.storage.report(Error::InvalidRhsInInstanceOf {
                    span,
                    ty: box ty.clone(),
                });
            }

            // Ok
            Type::Class(..) => {}

            // Conditionally error.
            //
            // Ok if it's assignable to `Function`.
            Type::TypeLit(..) | Type::Interface(..) => {
                if let Err(..) = self.assign(
                    &Type::Ref(Ref {
                        span,
                        ctxt: ModuleId::builtin(),
                        type_name: RTsEntityName::Ident(RIdent::new(
                            "Function".into(),
                            span.with_ctxt(SyntaxContext::empty()),
                        )),
                        type_args: None,
                    }),
                    &ty,
                    span,
                ) {
                    self.storage.report(Error::InvalidRhsInInstanceOf {
                        span,
                        ty: box ty.clone(),
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

    fn validate_bin_inner(&mut self, span: Span, op: BinaryOp, lt: Option<&Type>, rt: Option<&Type>) {
        let ls = lt.span();
        let rs = rt.span();

        let mut errors = Errors::default();

        match op {
            op!(bin, "+") => {
                // Validation is performed in type_of_bin_expr because
                // validation of types is required to compute type of the
                // expression.
            }
            op!("||") | op!("&&") => {
                if lt.is_some() {
                    match *lt.as_ref().unwrap().normalize() {
                        Type::Keyword(RTsKeywordType {
                            kind: TsKeywordTypeKind::TsVoidKeyword,
                            ..
                        }) => errors.push(Error::TS1345 { span }),
                        _ => {}
                    }
                }
            }

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
                if lt.is_some() && rt.is_some() {
                    let lt = lt.unwrap();
                    let rt = rt.unwrap();

                    let mut check = |ty: &Type, is_left| {
                        if ty.is_any() {
                            return;
                        }
                        if self.can_be_casted_to_number_in_rhs(ty.span(), &ty) {
                            return;
                        }

                        match ty.normalize() {
                            Type::Keyword(RTsKeywordType {
                                span,
                                kind: TsKeywordTypeKind::TsUndefinedKeyword,
                            }) => {
                                self.storage.report(Error::ObjectIsPossiblyUndefined { span: *span });
                            }

                            Type::Keyword(RTsKeywordType {
                                span,
                                kind: TsKeywordTypeKind::TsNullKeyword,
                            }) => {
                                self.storage.report(Error::ObjectIsPossiblyNull { span: *span });
                            }

                            _ => errors.push(if is_left {
                                Error::TS2362 { span: ty.span() }
                            } else {
                                Error::TS2363 { span: ty.span() }
                            }),
                        }
                    };

                    if (op == op!("&") || op == op!("^") || op == op!("|"))
                        && match lt.normalize() {
                            Type::Keyword(RTsKeywordType {
                                kind: TsKeywordTypeKind::TsBooleanKeyword,
                                ..
                            })
                            | Type::Lit(RTsLitType {
                                lit: RTsLit::Bool(..), ..
                            }) => true,
                            _ => false,
                        }
                        && match rt.normalize() {
                            Type::Keyword(RTsKeywordType {
                                kind: TsKeywordTypeKind::TsBooleanKeyword,
                                ..
                            })
                            | Type::Lit(RTsLitType {
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
            }

            op!("in") => {
                if lt.is_some() {
                    match lt.unwrap().normalize() {
                        Type::Keyword(RTsKeywordType {
                            kind: TsKeywordTypeKind::TsNullKeyword,
                            ..
                        }) => {
                            self.storage.report(Error::ObjectIsPossiblyNull { span });
                        }

                        Type::Keyword(RTsKeywordType {
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
                }

                if rt.is_some() {
                    match rt.unwrap().normalize() {
                        Type::Keyword(RTsKeywordType {
                            kind: TsKeywordTypeKind::TsNullKeyword,
                            ..
                        }) => {
                            self.storage.report(Error::ObjectIsPossiblyNull { span });
                        }

                        Type::Keyword(RTsKeywordType {
                            kind: TsKeywordTypeKind::TsUndefinedKeyword,
                            ..
                        }) => {
                            self.storage.report(Error::ObjectIsPossiblyUndefined { span });
                        }

                        _ => {
                            if !self.is_valid_rhs_of_in(&rt.unwrap()) {
                                errors.push(Error::TS2361 { span: rs })
                            }
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
                if let Ok(ty) = self.expand_top_ref(ty.span(), Cow::Borrowed(ty)) {
                    return self.is_valid_lhs_of_in(&ty);
                }

                true
            }

            Type::Keyword(RTsKeywordType {
                kind: TsKeywordTypeKind::TsAnyKeyword,
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
                kind: TsKeywordTypeKind::TsBigIntKeyword,
                ..
            })
            | Type::Keyword(RTsKeywordType {
                kind: TsKeywordTypeKind::TsSymbolKeyword,
                ..
            })
            | Type::Lit(RTsLitType {
                lit: RTsLit::Number(..),
                ..
            })
            | Type::Lit(RTsLitType {
                lit: RTsLit::Str(..), ..
            })
            | Type::Enum(..)
            | Type::EnumVariant(..)
            | Type::Param(..)
            | Type::Operator(Operator {
                op: TsTypeOperatorOp::KeyOf,
                ..
            }) => true,

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
                if let Ok(ty) = self.expand_top_ref(ty.span(), Cow::Borrowed(ty)) {
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
            | Type::Keyword(RTsKeywordType {
                kind: TsKeywordTypeKind::TsObjectKeyword,
                ..
            }) => true,
            Type::Union(ref u) => u.types.iter().all(|ty| self.is_valid_rhs_of_in(&ty)),

            _ => false,
        }
    }

    #[extra_validator]
    fn check_for_mixed_nullish_coalescing(&mut self, e: &RBinExpr) {
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

fn extract_name_for_assignment(e: &RExpr) -> Option<Name> {
    match e {
        RExpr::Paren(e) => extract_name_for_assignment(&e.expr),
        RExpr::Assign(e) => match &e.left {
            RPatOrExpr::Expr(e) => extract_name_for_assignment(e),
            RPatOrExpr::Pat(pat) => match &**pat {
                RPat::Ident(i) => Some(i.into()),
                RPat::Expr(e) => extract_name_for_assignment(e),
                _ => None,
            },
        },
        _ => Name::try_from(e).ok(),
    }
}
