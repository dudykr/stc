use stc_ts_ast_rnode::{RBigInt, RBool, RExpr, RExprOrSuper, RMemberExpr, RNumber, ROptChainExpr, RParenExpr, RStr, RTsLit, RUnaryExpr};
use stc_ts_errors::{DebugExt, Error, Errors};
use stc_ts_ast_rnode::{RBigInt, RBool, RExpr, RMemberExpr, RNumber, RStr, RTsLit, RUnaryExpr};
use stc_ts_ast_rnode::{
    RBigInt, RBool, RExpr, RMemberExpr, RMemberProp, RNumber, RStr, RTsLit, RUnaryExpr,
};
use stc_ts_ast_rnode::{RBigInt, RBool, RExpr, RMemberExpr, RMemberProp, RNumber, RStr, RTsLit, RUnaryExpr};
use stc_ts_errors::{Error, Errors};
use stc_ts_types::{KeywordType, KeywordTypeMetadata, LitType, Union};
use swc_atoms::js_word;
use swc_common::{Span, Spanned};
use swc_ecma_ast::*;

use crate::{
    analyzer::{expr::TypeOfMode, util::ResultExt, Analyzer, ScopeKind},
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

            self.validate_with(|a| a.validate_delete_operand(&arg));
        }

        // TODO(kdy1): Check for `self.ctx.in_cond` to improve performance.
        let arg: Option<Type> = match op {
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

        if let Some(ref arg) = arg {
            self.validate_unary_expr_inner(span, *op, arg);
        }

        match op {
            op!(unary, "+") | op!(unary, "-") | op!("~") => {
                if let Some(arg) = &arg {
                    if arg.is_kwd(TsKeywordTypeKind::TsSymbolKeyword) {
                        self.storage.report(Error::NumericUnaryOpToSymbol { span: arg.span(), op: *op })
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
                        })
                        .map(Type::Lit)
                        .collect(),
                        metadata: Default::default(),
                    }));
                }
                return Ok(Type::Keyword(KeywordType {
                    span,
                    kind: TsKeywordTypeKind::TsStringKeyword,
                    metadata: Default::default(),
                }));
            }

            op!("void") => return Ok(Type::undefined(span, Default::default())),

            op!(unary, "-") | op!(unary, "+") => {
                if let Some(arg) = &arg {
                    match arg.normalize() {
                        Type::Lit(LitType {
                            lit: RTsLit::Number(RNumber { span, value, raw: None }),
                            ..
                        }) => {
                            let span = *span;

                            return Ok(Type::Lit(LitType {
                                span,
                                lit: RTsLit::Number(RNumber {
                                    span,
                                    value: if *op == op!(unary, "-") { -(*value) } else { *value },
                                    value: if *op == op!(unary, "-") {
                                        -(*value)
                                    } else {
                                        *value
                                    },
                                    raw: None,
                                }),
                                metadata: Default::default(),
                            }));
                        }
                        _ => {}
                    }
                }

                return Ok(Type::Keyword(KeywordType {
                    span,
                    kind: TsKeywordTypeKind::TsNumberKeyword,
                    metadata: Default::default(),
                }));
            }

            op!("~") => {
                return Ok(Type::Keyword(KeywordType {
                    span,
                    kind: TsKeywordTypeKind::TsNumberKeyword,
                    metadata: Default::default(),
                }));
            }
            _ => {}
        }

        match arg {
            Some(Type::Keyword(KeywordType {
                kind: TsKeywordTypeKind::TsUnknownKeyword,
                ..
            })) => {
                debug_assert!(!arg.span().is_dummy());
                return Err(Error::Unknown { span: arg.span() });
            }
            _ => {}
        }

        if let Some(arg) = arg {
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
        match &*arg {
            RExpr::Member(RMemberExpr {
                obj: box RExpr::This(..),
                prop: RMemberProp::PrivateName(..),
                ..
            }) => Err(Error::CannotDeletePrivateProperty { span }),

            RExpr::Paren(RParenExpr {
                expr: box RExpr::Member(expr),
                ..
            })
            | RExpr::OptChain(ROptChainExpr {
                expr: box RExpr::Member(expr),
                ..
            })
            | RExpr::Member(expr) => {
                if self.rule().strict_null_checks {
                    let ty = self.type_of_member_expr(expr, TypeOfMode::RValue).convert_err(|err| match &err {
                        Error::ObjectIsPossiblyNull { span, .. }
                        | Error::ObjectIsPossiblyUndefined { span, .. }
                        | Error::ObjectIsPossiblyNullOrUndefined { span, .. } => Error::DeleteOperandMustBeOptional { span: *span },
                        _ => err,
                    })?;
                    if !self.can_be_undefined(span, &ty)? {
                        return Err(Error::DeleteOperandMustBeOptional { span });
                    }
                }
                return Ok(());
            }

            //
            // delete (o4.b?.c.d);
            // delete (o4.b?.c.d)?.e;
            RExpr::Paren(RParenExpr { expr, .. }) | RExpr::OptChain(ROptChainExpr { expr, .. }) => {
                return self.validate_delete_operand(expr);
            }

            RExpr::Await(..) => Err(Error::InvalidDeleteOperand { span }),

            _ => Err(Error::InvalidDeleteOperand { span }),
        }
    }

    fn validate_unary_expr_inner(&mut self, span: Span, op: UnaryOp, arg: &Type) {
        let mut errors = Errors::default();

        match op {
            op!("typeof") | op!("delete") | op!("void") => match arg.normalize() {
                Type::EnumVariant(..) if op == op!("delete") => errors.push(Error::TS2704 { span: arg.span() }),

                _ => {}
            },

            op!("~") | op!(unary, "-") | op!(unary, "+") => match arg.normalize() {
                Type::Keyword(KeywordType {
                    kind: TsKeywordTypeKind::TsNumberKeyword,
                    ..
                }) => {}

                Type::Keyword(KeywordType {
                    kind: TsKeywordTypeKind::TsNullKeyword,
                    ..
                }) => errors.push(Error::TS2531 { span: arg.span() }),

                Type::Keyword(KeywordType {
                    kind: TsKeywordTypeKind::TsUndefinedKeyword,
                    ..
                }) => errors.push(Error::ObjectIsPossiblyUndefined { span: arg.span() }),

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
    match ty {
        Type::Lit(LitType { ref lit, span, metadata }) => match *lit {
            RTsLit::Bool(ref v) => {
                return Type::Lit(LitType {
                    lit: RTsLit::Bool(RBool {
                        value: !v.value,
                        ..v.clone()
                    }),
                    span,
                    metadata,
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
                });
            }
            RTsLit::Tpl(ref v) => {
                return Type::Lit(LitType {
                    lit: RTsLit::Bool(RBool {
                        value: !v.quasis.iter().next().as_ref().unwrap().raw.is_empty(),
                        span: v.span,
                    }),
                    span,
                    metadata,
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
                });
            }
        },

        _ => {}
    }

    KeywordType {
        span: ty.span(),
        kind: TsKeywordTypeKind::TsBooleanKeyword,
        metadata: KeywordTypeMetadata {
            common: ty.metadata(),
            ..Default::default()
        },
    }
    .into()
}
