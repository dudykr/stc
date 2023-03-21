use stc_ts_ast_rnode::{
    RBigInt, RBool, RExpr, RIdent, RLit, RMemberExpr, RMemberProp, RNumber, ROptChainBase, ROptChainExpr, RParenExpr, RStr, RTsLit,
    RUnaryExpr,
};
use stc_ts_errors::{DebugExt, ErrorKind, Errors};
use stc_ts_types::{KeywordType, KeywordTypeMetadata, LitType, Union};
use swc_atoms::js_word;
use swc_common::{Span, Spanned};
use swc_ecma_ast::*;

use crate::{
    analyzer::{expr::TypeOfMode, scope::ScopeKind, util::ResultExt, Analyzer},
    ty::Type,
    validator,
    validator::ValidateWith,
    VResult,
};

#[validator]
impl Analyzer<'_, '_> {
    fn validate(&mut self, e: &RUnaryExpr) -> VResult<Type> {
        let RUnaryExpr { span, op, arg, .. } = e;
        let span = *span;

        if let op!("delete") = op {
            // `delete foo` returns bool

            self.validate_with(|a| a.validate_delete_operand(arg));
        }

        // TODO(kdy1): Check for `self.ctx.in_cond` to improve performance.
        let arg_ty: Option<Type> = match op {
            op!("!") => {
                let orig_facts = self.cur_facts.take();
                let arg_ty = self
                    .with_child(ScopeKind::Flow, orig_facts.true_facts.clone(), |child: &mut Analyzer| {
                        arg.validate_with_args(child, (TypeOfMode::RValue, None, None))
                    })
                    .report(&mut self.storage)
                    .map(|mut ty| {
                        ty.reposition(arg.span());
                        ty
                    });
                let new_facts = self.cur_facts.take();
                self.cur_facts = orig_facts;

                self.cur_facts.true_facts += new_facts.false_facts;
                self.cur_facts.false_facts += new_facts.true_facts;

                arg_ty
            }
            _ => arg
                .validate_with_args(self, (TypeOfMode::RValue, None, None))
                .report(&mut self.storage)
                .map(|mut ty| {
                    ty.reposition(arg.span());
                    ty
                }),
        };

        if let Some(arg_ty) = &arg_ty {
            self.validate_unary_expr_inner(span, *op, arg, arg_ty);
        }

        match op {
            op!(unary, "+") | op!(unary, "-") | op!("~") => {
                if let Some(arg) = &arg_ty {
                    if arg.is_symbol_like() {
                        self.storage.report(ErrorKind::NumericOpToSymbol { span: arg.span() }.into())
                    }
                }
            }

            _ => {}
        }

        match op {
            op!("typeof") => {
                if self.ctx.in_export_default_expr {
                    return Ok(Type::Union(Union {
                        span,
                        types: [
                            js_word!("string"),
                            js_word!("number"),
                            js_word!("bigint"),
                            js_word!("boolean"),
                            js_word!("symbol"),
                            js_word!("undefined"),
                            js_word!("object"),
                            js_word!("function"),
                        ]
                        .iter()
                        .cloned()
                        .map(|value| LitType {
                            span,
                            lit: RTsLit::Str(RStr { span, value, raw: None }),
                            metadata: Default::default(),
                            tracker: Default::default(),
                        })
                        .map(Type::Lit)
                        .collect(),
                        metadata: Default::default(),
                        tracker: Default::default(),
                    }));
                }
                return Ok(Type::Keyword(KeywordType {
                    span,
                    kind: TsKeywordTypeKind::TsStringKeyword,
                    metadata: Default::default(),
                    tracker: Default::default(),
                }));
            }

            op!("void") => return Ok(Type::undefined(span, Default::default())),

            op!(unary, "-") | op!(unary, "+") => {
                if let Some(arg) = &arg_ty {
                    if let Type::Lit(LitType {
                        lit: RTsLit::Number(RNumber { span, value, .. }),
                        ..
                    }) = arg.normalize()
                    {
                        let span = *span;

                        return Ok(Type::Lit(LitType {
                            span,
                            lit: RTsLit::Number(RNumber {
                                span,
                                value: if *op == op!(unary, "-") { -(*value) } else { *value },
                                raw: None,
                            }),
                            metadata: Default::default(),
                            tracker: Default::default(),
                        }));
                    }
                }

                return Ok(Type::Keyword(KeywordType {
                    span,
                    kind: TsKeywordTypeKind::TsNumberKeyword,
                    metadata: Default::default(),
                    tracker: Default::default(),
                }));
            }

            op!("~") => {
                return Ok(Type::Keyword(KeywordType {
                    span,
                    kind: TsKeywordTypeKind::TsNumberKeyword,
                    metadata: Default::default(),
                    tracker: Default::default(),
                }));
            }
            _ => {}
        }

        if *op != op!("!") {
            if let Some(Type::Keyword(KeywordType {
                kind: TsKeywordTypeKind::TsUnknownKeyword,
                ..
            })) = arg_ty
            {
                debug_assert!(!arg.span().is_dummy());
                return Err(ErrorKind::Unknown { span: arg.span() }.into());
            }
        }

        if let Some(arg) = arg_ty {
            match op {
                op!("!") => return Ok(negate(arg)),

                op!("typeof") | op!("void") => unreachable!(),

                _ => {}
            }
        }

        // This is a worst case. We only return the type without good error reporting.
        match op {
            op!("!") | op!("delete") => {
                return Ok(Type::Keyword(KeywordType {
                    span,
                    kind: TsKeywordTypeKind::TsBooleanKeyword,
                    metadata: Default::default(),
                    tracker: Default::default(),
                }))
            }

            _ => {}
        }

        unimplemented!("validate(UnaryExpr)\n{:?}", e)
    }
}

impl Analyzer<'_, '_> {
    fn validate_delete_operand(&mut self, arg: &RExpr) -> VResult<()> {
        let span = arg.span();
        match arg {
            RExpr::Member(RMemberExpr {
                obj: box RExpr::This(..),
                prop: RMemberProp::PrivateName(..),
                ..
            }) => Err(ErrorKind::CannotDeletePrivateProperty { span }.into()),

            RExpr::Paren(RParenExpr {
                expr: box RExpr::Member(expr),
                ..
            })
            | RExpr::OptChain(ROptChainExpr {
                base: ROptChainBase::Member(expr),
                ..
            })
            | RExpr::Member(expr) => {
                if self.rule().strict_null_checks {
                    let ty = self
                        .type_of_member_expr(expr, TypeOfMode::RValue, None, false)
                        .convert_err(|err| match err {
                            ErrorKind::ObjectIsPossiblyNull { span, .. }
                            | ErrorKind::ObjectIsPossiblyUndefined { span, .. }
                            | ErrorKind::ObjectIsPossiblyNullOrUndefined { span, .. } => ErrorKind::DeleteOperandMustBeOptional { span },
                            _ => err,
                        })?;

                    if !ty.is_optional() && !ty.contains_undefined() {
                        return Err(ErrorKind::DeleteOperandMustBeOptional { span }.into());
                    }
                }
                Ok(())
            }

            //
            // delete (o4.b?.c.d);
            // delete (o4.b?.c.d)?.e;
            RExpr::Paren(RParenExpr { expr, .. }) => self.validate_delete_operand(expr),

            RExpr::Await(..) => Err(ErrorKind::InvalidDeleteOperand { span }.into()),

            _ => Err(ErrorKind::InvalidDeleteOperand { span }.into()),
        }
    }

    fn validate_unary_expr_inner(&mut self, span: Span, op: UnaryOp, arg_expr: &RExpr, arg: &Type) {
        match op {
            op!("delete") | op!("!") | op!("typeof") | op!("void") => {}
            _ => match arg_expr {
                RExpr::Lit(RLit::Null(..))
                | RExpr::Ident(RIdent {
                    sym: js_word!("undefined"),
                    ..
                }) => {
                    self.storage
                        .report(ErrorKind::UndefinedOrNullIsNotValidOperand { span: arg_expr.span() }.into());
                    return;
                }
                _ => {}
            },
        }

        let mut errors = Errors::default();

        match op {
            op!("typeof") | op!("delete") | op!("void") => match arg.normalize() {
                Type::EnumVariant(..) if op == op!("delete") => errors.push(ErrorKind::TS2704 { span: arg.span() }.into()),

                _ => {}
            },

            op!("~") | op!(unary, "-") | op!(unary, "+") => match arg.normalize() {
                Type::Keyword(KeywordType {
                    kind: TsKeywordTypeKind::TsNumberKeyword,
                    ..
                }) => {}

                Type::Keyword(KeywordType {
                    kind: TsKeywordTypeKind::TsUnknownKeyword,
                    ..
                }) => errors.push(ErrorKind::IsTypeUnknown { span: arg.span() }.into()),

                Type::Keyword(KeywordType {
                    kind: TsKeywordTypeKind::TsNullKeyword,
                    ..
                }) => errors.push(ErrorKind::TS2531 { span: arg.span() }.into()),

                Type::Keyword(KeywordType {
                    kind: TsKeywordTypeKind::TsUndefinedKeyword,
                    ..
                }) => errors.push(ErrorKind::ObjectIsPossiblyUndefined { span: arg.span() }.into()),

                _ => {
                    //
                }
            },

            _ => {}
        }

        self.storage.report_all(errors);
    }
}

fn negate(ty: Type) -> Type {
    if let Type::Lit(LitType {
        ref lit, span, metadata, ..
    }) = ty
    {
        match *lit {
            RTsLit::Bool(ref v) => {
                return Type::Lit(LitType {
                    lit: RTsLit::Bool(RBool {
                        value: !v.value,
                        ..v.clone()
                    }),
                    span,
                    metadata,
                    tracker: Default::default(),
                });
            }
            RTsLit::Number(ref v) => {
                return Type::Lit(LitType {
                    lit: RTsLit::Bool(RBool {
                        value: v.value != 0.0,
                        span: v.span,
                    }),
                    span,
                    metadata,
                    tracker: Default::default(),
                });
            }
            RTsLit::Str(ref v) => {
                return Type::Lit(LitType {
                    lit: RTsLit::Bool(RBool {
                        value: v.value != js_word!(""),
                        span: v.span,
                    }),
                    span,
                    metadata,
                    tracker: Default::default(),
                });
            }
            RTsLit::Tpl(ref v) => {
                return Type::Lit(LitType {
                    lit: RTsLit::Bool(RBool {
                        value: !v.quasis.first().as_ref().unwrap().raw.is_empty(),
                        span: v.span,
                    }),
                    span,
                    metadata,
                    tracker: Default::default(),
                });
            }
            RTsLit::BigInt(ref v) => {
                return Type::Lit(LitType {
                    lit: RTsLit::BigInt(RBigInt {
                        value: box -(*v.value.clone()),
                        span: v.span,
                        raw: None,
                    }),
                    span,
                    metadata,
                    tracker: Default::default(),
                });
            }
        }
    }

    KeywordType {
        span: ty.span(),
        kind: TsKeywordTypeKind::TsBooleanKeyword,
        metadata: KeywordTypeMetadata {
            common: ty.metadata(),
            ..Default::default()
        },
        tracker: Default::default(),
    }
    .into()
}
