use super::super::{
    util::{Comparator, ResultExt},
    Analyzer,
};
use crate::{
    analyzer::{Ctx, ScopeKind},
    ty::{Operator, Type, TypeExt},
    type_facts::TypeFacts,
    util::{is_str_lit_or_union, is_str_or_union, RemoveTypes},
    validator,
    validator::ValidateWith,
    ValidationResult,
};
use stc_ts_ast_rnode::RBinExpr;
use stc_ts_ast_rnode::RExpr;
use stc_ts_ast_rnode::RLit;
use stc_ts_ast_rnode::RStr;
use stc_ts_ast_rnode::RTsKeywordType;
use stc_ts_ast_rnode::RTsLit;
use stc_ts_ast_rnode::RTsLitType;
use stc_ts_ast_rnode::RUnaryExpr;
use stc_ts_errors::Error;
use stc_ts_errors::Errors;
use stc_ts_file_analyzer_macros::extra_validator;
use stc_ts_types::name::Name;
use stc_ts_types::TypeElement;
use std::borrow::Cow;
use std::convert::TryFrom;
use swc_common::TypeEq;
use swc_common::{Span, Spanned};
use swc_ecma_ast::*;
use swc_ecma_utils::Value::Known;

#[validator]
impl Analyzer<'_, '_> {
    fn validate(&mut self, e: &RBinExpr) -> ValidationResult {
        let RBinExpr {
            span,
            op,
            ref left,
            ref right,
            ..
        } = *e;

        let mut errors = vec![];

        let lt = left
            .validate_with_default(self)
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
                ty.respan(left.span().with_ctxt(span.ctxt));

                Ok(ty)
            })
            .store(&mut errors);

        let facts = if op == op!("&&") {
            // We need a new virtual scope.
            self.cur_facts.true_facts.take()
        } else {
            Default::default()
        };

        let rhs = self
            .with_child(ScopeKind::Flow, facts, |child: &mut Analyzer| -> ValidationResult<_> {
                let ty = right.validate_with_default(child).and_then(|mut ty| {
                    if ty.is_ref_type() {
                        let ctx = Ctx {
                            preserve_ref: false,
                            ignore_expand_prevention_for_top: true,
                            ..child.ctx
                        };
                        ty = child.with_ctx(ctx).expand_fully(span, ty, true)?;
                    }

                    let span = ty.span();
                    ty.respan(right.span().with_ctxt(span.ctxt));

                    Ok(ty)
                })?;

                let rhs_true_facts = child.cur_facts.true_facts.take();

                Ok((ty, rhs_true_facts))
            })
            .store(&mut errors);

        let (rt, rhs_facts) = match rhs {
            Some(v) => (Some(v.0), v.1),
            None => (None, Default::default()),
        };

        if op == op!("||") {
            self.cur_facts.true_facts += rhs_facts;
        }

        self.validate_bin_inner(span, op, lt.as_ref().map(|v| &**v), rt.as_ref().map(|v| &**v));

        let (lt, rt): (Box<Type>, Box<Type>) = match (lt, rt) {
            (Some(l), Some(r)) => (l, r),
            _ => return Err(box Error::Errors { span, errors }),
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
                    left: (&**left, &lt),
                    right: (&**right, &rt),
                };

                match c.take_if_any_matches(|(l, l_ty), (_, r_ty)| match **l_ty {
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
            }

            op!("instanceof") => {
                match **left {
                    RExpr::Ident(ref i) => {
                        //
                        let ty = self.make_instance_or_report(&rt);

                        self.cur_facts.true_facts.vars.insert(Name::from(i), ty);
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
                match *$ty {
                    Type::Keyword(RTsKeywordType {
                        kind: TsKeywordTypeKind::TsUnknownKeyword,
                        ..
                    }) => {
                        return Err(box Error::Unknown { span });
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

                if let Some(()) = c.take_if_any_matches(|(_, lt), (_, _)| match **lt {
                    Type::Keyword(RTsKeywordType {
                        kind: TsKeywordTypeKind::TsUnknownKeyword,
                        ..
                    }) => Some(()),

                    _ => None,
                }) {
                    return Err(box Error::Unknown { span });
                }

                if lt.is_num() && rt.is_num() {
                    return Ok(box Type::Keyword(RTsKeywordType {
                        span,
                        kind: TsKeywordTypeKind::TsNumberKeyword,
                    }));
                }

                if let Some(()) = c.take_if_any_matches(|(_, lt), (_, _)| match **lt {
                    Type::Keyword(RTsKeywordType {
                        kind: TsKeywordTypeKind::TsStringKeyword,
                        ..
                    })
                    | Type::Lit(RTsLitType {
                        lit: RTsLit::Str(..), ..
                    }) => Some(()),

                    _ => None,
                }) {
                    return Ok(box Type::Keyword(RTsKeywordType {
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
                    return Ok(box Type::Keyword(RTsKeywordType { span, kind }));
                }

                if c.any(|(_, ty)| {
                    ty.is_kwd(TsKeywordTypeKind::TsUndefinedKeyword) || ty.is_kwd(TsKeywordTypeKind::TsNullKeyword)
                }) {
                    return Err(box Error::TS2365 { span });
                }

                // Rule:
                //  - null is invalid operand
                //  - undefined is invalid operand
                if c.both(|(_, ty)| match **ty {
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
                    return Err(box Error::TS2365 { span });
                }

                if is_str_or_union(&lt) || is_str_or_union(&rt) {
                    return Ok(box Type::Keyword(RTsKeywordType {
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
                    return Ok(box Type::Keyword(RTsKeywordType {
                        span,
                        kind: TsKeywordTypeKind::TsNumberKeyword,
                    }));
                }

                return Err(box Error::InvalidBinaryOp { span, op });
            }
            op!("*") | op!("/") => {
                no_unknown!();

                return Ok(box Type::Keyword(RTsKeywordType {
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

                return Ok(box Type::Keyword(RTsKeywordType {
                    kind: TsKeywordTypeKind::TsNumberKeyword,
                    span,
                }));
            }

            op!("===") | op!("!==") | op!("!=") | op!("==") => {
                return Ok(box Type::Keyword(RTsKeywordType {
                    span,
                    kind: TsKeywordTypeKind::TsBooleanKeyword,
                }));
            }

            op!("instanceof") => {
                if match lt.normalize() {
                    ty if ty.is_any() || ty.is_kwd(TsKeywordTypeKind::TsObjectKeyword) => false,
                    Type::This(..) | Type::Param(..) | Type::Ref(..) => false,
                    _ => true,
                } {
                    self.storage.report(box Error::InvalidLhsInInstanceOf {
                        ty: lt.clone(),
                        span: left.span(),
                    })
                }

                // The right-hand side of an 'instanceof' expression must be of type 'any' or of
                // a type assignable to the 'Function' interface type.ts(2359)
                if match rt.normalize() {
                    Type::Param(..) | Type::Infer(..) => true,
                    ty if ty.is_any() => false,
                    ty if ty.is_kwd(TsKeywordTypeKind::TsSymbolKeyword) => true,
                    _ => false,
                } {
                    self.storage.report(box Error::InvalidRhsInInstanceOf {
                        span: right.span(),
                        ty: rt.clone(),
                    })
                }

                return Ok(box Type::Keyword(RTsKeywordType {
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
                        self.storage
                            .report(box Error::ObjectIsPossiblyUndefined { span: *span });
                    }

                    Type::Keyword(RTsKeywordType {
                        span,
                        kind: TsKeywordTypeKind::TsNullKeyword,
                    }) => {
                        self.storage.report(box Error::ObjectIsPossiblyNull { span: *span });
                    }

                    _ => {}
                };

                check_for_invalid_operand(&lt);
                check_for_invalid_operand(&rt);

                self.validate_relative_comparison_operands(span, op, &lt, &rt);

                return Ok(box Type::Keyword(RTsKeywordType {
                    span,
                    kind: TsKeywordTypeKind::TsBooleanKeyword,
                }));
            }

            op!("in") => {
                return Ok(box Type::Keyword(RTsKeywordType {
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
                        let lt = box lt.remove_falsy();

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

                let mut lt = box lt.remove_falsy();
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
                    self.prevent_generalize(&mut ty);
                }

                Ok(ty)
            }
        }
    }
}

impl Analyzer<'_, '_> {
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
                                self.storage.report(box Error::CannotCompareWithOp { span, op });
                                return;
                            }
                            _ => {}
                        }
                    }
                }
            }
            _ => {}
        }
    }
    fn validate_bin_inner(&mut self, span: Span, op: BinaryOp, lt: Option<&Type>, rt: Option<&Type>) {
        let ls = lt.span();
        let rs = rt.span();

        let mut errors = Errors::default();

        match op {
            op!("===") | op!("!==") => {
                if lt.is_some() && rt.is_some() {
                    let lt = lt.unwrap();
                    let rt = rt.unwrap();

                    let has_overlap = self.has_overlap(span, lt, rt).report(&mut self.storage).unwrap_or(true);

                    if !has_overlap {
                        errors.push(box Error::NoOverlap {
                            span,
                            value: op != op!("==="),
                            left: ls,
                            right: rs,
                        })
                    }
                }
            }
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
                        }) => errors.push(box Error::TS1345 { span }),
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
                                self.storage
                                    .report(box Error::ObjectIsPossiblyUndefined { span: *span });
                            }

                            Type::Keyword(RTsKeywordType {
                                span,
                                kind: TsKeywordTypeKind::TsNullKeyword,
                            }) => {
                                self.storage.report(box Error::ObjectIsPossiblyNull { span: *span });
                            }

                            _ => errors.push(if is_left {
                                box Error::TS2362 { span: ty.span() }
                            } else {
                                box Error::TS2363 { span: ty.span() }
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
                        errors.push(box Error::TS2447 { span });
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
                        }) => {}

                        _ => errors.push(box Error::TS2360 { span: ls }),
                    }
                }

                if rt.is_some() {
                    fn is_ok(ty: &Type) -> bool {
                        if ty.is_any() {
                            return true;
                        }

                        match ty.normalize() {
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
                            Type::Union(ref u) => u.types.iter().all(|ty| is_ok(&ty)),

                            _ => false,
                        }
                    }

                    if !is_ok(&rt.unwrap()) {
                        errors.push(box Error::TS2361 { span: rs })
                    }
                }
            }

            _ => {}
        }

        self.storage.report_all(errors);
    }
}
