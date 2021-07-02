use crate::{
    analyzer::{expr::TypeOfMode, util::ResultExt, Analyzer, ScopeKind},
    ty::Type,
    validator,
    validator::ValidateWith,
    ValidationResult,
};
use rnode::NodeId;
use stc_ts_ast_rnode::{
    RBigInt, RBool, RExpr, RExprOrSuper, RMemberExpr, RNumber, RStr, RTsKeywordType, RTsLit, RTsLitType, RUnaryExpr,
};
use stc_ts_errors::{Error, Errors};
use stc_ts_types::Union;
use swc_atoms::js_word;
use swc_common::{Span, Spanned};
use swc_ecma_ast::*;

#[validator]
impl Analyzer<'_, '_> {
    fn validate(&mut self, e: &RUnaryExpr) -> ValidationResult {
        let RUnaryExpr { span, op, arg, .. } = e;
        let span = *span;

        if let op!("delete") = op {
            // `delete foo` returns bool

            self.validate_with(|a| a.validate_delete_operand(&arg));
        }

        // TODO: Check for `self.ctx.in_cond` to improve performance.
        let arg: Option<Type> = match op {
            op!("!") => {
                let orig_facts = self.cur_facts.take();
                let arg_ty = self
                    .with_child(
                        ScopeKind::Flow,
                        orig_facts.true_facts.clone(),
                        |child: &mut Analyzer| arg.validate_with_args(child, (TypeOfMode::RValue, None, None)),
                    )
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
                        self.storage.report(Error::NumericUnaryOpToSymbol {
                            span: arg.span(),
                            op: *op,
                        })
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
                        .map(|value| RTsLitType {
                            node_id: NodeId::invalid(),
                            span,
                            lit: RTsLit::Str(RStr {
                                span,
                                value,
                                has_escape: false,
                                kind: Default::default(),
                            }),
                        })
                        .map(Type::Lit)
                        .collect(),
                    }));
                }
                return Ok(Type::Keyword(RTsKeywordType {
                    span,
                    kind: TsKeywordTypeKind::TsStringKeyword,
                }));
            }

            op!("void") => return Ok(Type::undefined(span)),

            op!(unary, "-") | op!(unary, "+") => {
                if let Some(arg) = &arg {
                    match arg.normalize() {
                        Type::Lit(RTsLitType {
                            lit: RTsLit::Number(RNumber { span, value }),
                            ..
                        }) => {
                            let span = *span;

                            return Ok(Type::Lit(RTsLitType {
                                node_id: NodeId::invalid(),
                                span,
                                lit: RTsLit::Number(RNumber {
                                    span,
                                    value: if *op == op!(unary, "-") { -(*value) } else { *value },
                                }),
                            }));
                        }
                        _ => {}
                    }
                }

                return Ok(Type::Keyword(RTsKeywordType {
                    span,
                    kind: TsKeywordTypeKind::TsNumberKeyword,
                }));
            }

            op!("~") => {
                return Ok(Type::Keyword(RTsKeywordType {
                    span,
                    kind: TsKeywordTypeKind::TsNumberKeyword,
                }));
            }
            _ => {}
        }

        match arg {
            Some(Type::Keyword(RTsKeywordType {
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
                return Ok(Type::Keyword(RTsKeywordType {
                    span,
                    kind: TsKeywordTypeKind::TsBooleanKeyword,
                }))
            }

            _ => {}
        }

        unimplemented!("validate(UnaryExpr)\n{:?}", e)
    }
}

impl Analyzer<'_, '_> {
    fn validate_delete_operand(&mut self, arg: &RExpr) -> ValidationResult<()> {
        let span = arg.span();

        match &*arg {
            RExpr::Member(RMemberExpr {
                obj: RExprOrSuper::Expr(box RExpr::This(..)),
                computed: false,
                prop: box RExpr::PrivateName(..),
                ..
            }) => Err(Error::CannotDeletePrivateProperty { span }),

            RExpr::Member(..) => return Ok(()),

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
                Type::Keyword(RTsKeywordType {
                    kind: TsKeywordTypeKind::TsNumberKeyword,
                    ..
                }) => {}

                Type::Keyword(RTsKeywordType {
                    kind: TsKeywordTypeKind::TsNullKeyword,
                    ..
                }) => errors.push(Error::TS2531 { span: arg.span() }),

                Type::Keyword(RTsKeywordType {
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
        Type::Lit(RTsLitType { ref lit, span, node_id }) => match *lit {
            RTsLit::Bool(ref v) => {
                return Type::Lit(RTsLitType {
                    node_id,
                    lit: RTsLit::Bool(RBool {
                        value: !v.value,
                        ..v.clone()
                    }),
                    span,
                });
            }
            RTsLit::Number(ref v) => {
                return Type::Lit(RTsLitType {
                    node_id: NodeId::invalid(),
                    lit: RTsLit::Bool(RBool {
                        value: v.value != 0.0,
                        span: v.span,
                    }),
                    span,
                });
            }
            RTsLit::Str(ref v) => {
                return Type::Lit(RTsLitType {
                    node_id: NodeId::invalid(),
                    lit: RTsLit::Bool(RBool {
                        value: v.value != js_word!(""),
                        span: v.span,
                    }),
                    span,
                });
            }
            RTsLit::Tpl(ref v) => {
                return Type::Lit(RTsLitType {
                    node_id: NodeId::invalid(),
                    lit: RTsLit::Bool(RBool {
                        value: v.quasis.iter().next().as_ref().unwrap().raw.value != js_word!(""),
                        span: v.span,
                    }),
                    span,
                });
            }
            RTsLit::BigInt(ref v) => {
                return Type::Lit(RTsLitType {
                    node_id: NodeId::invalid(),
                    lit: RTsLit::BigInt(RBigInt {
                        value: -v.value.clone(),
                        span: v.span,
                    }),
                    span,
                });
            }
        },

        _ => {}
    }

    RTsKeywordType {
        span: ty.span(),
        kind: TsKeywordTypeKind::TsBooleanKeyword,
    }
    .into()
}
