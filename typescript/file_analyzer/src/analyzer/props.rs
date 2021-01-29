use super::{marks::MarkExt, scope::ScopeKind, Analyzer};
use crate::{
    analyzer::{expr::TypeOfMode, util::ResultExt, Ctx},
    ty::{MethodSignature, Operator, PropertySignature, Type, TypeElement, TypeExt},
    validator,
    validator::ValidateWith,
    ValidationResult,
};
use rnode::Visit;
use rnode::VisitWith;
use stc_ts_ast_rnode::RAssignProp;
use stc_ts_ast_rnode::RComputedPropName;
use stc_ts_ast_rnode::RExpr;
use stc_ts_ast_rnode::RExprOrSuper;
use stc_ts_ast_rnode::RGetterProp;
use stc_ts_ast_rnode::RIdent;
use stc_ts_ast_rnode::RKeyValueProp;
use stc_ts_ast_rnode::RLit;
use stc_ts_ast_rnode::RMemberExpr;
use stc_ts_ast_rnode::RMethodProp;
use stc_ts_ast_rnode::RNumber;
use stc_ts_ast_rnode::RProp;
use stc_ts_ast_rnode::RPropName;
use stc_ts_ast_rnode::RSetterProp;
use stc_ts_ast_rnode::RStr;
use stc_ts_ast_rnode::RTsKeywordType;
use stc_ts_errors::Error;
use stc_ts_errors::Errors;
use stc_ts_types::ComputedKey;
use stc_ts_types::Key;
use swc_atoms::js_word;
use swc_common::Spanned;
use swc_ecma_ast::*;

#[derive(Debug, Clone, Copy)]
pub(super) enum ComputedPropMode {
    Class {
        has_body: bool,
    },
    /// Object literal
    Object,

    Interface,
}

#[validator]
impl Analyzer<'_, '_> {
    fn validate(&mut self, node: &RPropName) -> ValidationResult<Key> {
        self.record(node);

        match node {
            RPropName::Computed(c) => c.validate_with(self),
            RPropName::Ident(i) => Ok(Key::Normal {
                span: i.span,
                sym: i.sym.clone(),
            }),
            RPropName::Str(s) => Ok(Key::Normal {
                span: s.span,
                sym: s.value.clone(),
            }),
            RPropName::Num(v) => Ok(Key::Num(v.clone())),
            RPropName::BigInt(v) => Ok(Key::BigInt(v.clone())),
        }
    }
}

#[validator]
impl Analyzer<'_, '_> {
    fn validate(&mut self, node: &RComputedPropName) -> ValidationResult<Key> {
        self.record(node);
        let span = node.span;

        let mode = self.ctx.computed_prop_mode;
        let span = node.span;

        let is_symbol_access = match *node.expr {
            RExpr::Member(RMemberExpr {
                obj:
                    RExprOrSuper::Expr(box RExpr::Ident(RIdent {
                        sym: js_word!("Symbol"),
                        ..
                    })),
                ..
            }) => true,
            _ => false,
        };
        let mut check_for_symbol_form = true;

        let mut errors = Errors::default();

        let ty = match node.expr.validate_with_default(self) {
            Ok(ty) => ty,
            Err(err) => {
                check_for_symbol_form = false;
                match err {
                    box Error::TS2585 { span } => Err(box Error::TS2585 { span })?,
                    _ => {}
                }

                errors.push(err);
                // TODO: Change this to something else (maybe any)
                Type::unknown(span)
            }
        };

        if check_for_symbol_form && is_symbol_access {
            match ty.normalize() {
                Type::Keyword(RTsKeywordType {
                    kind: TsKeywordTypeKind::TsSymbolKeyword,
                    ..
                }) => {}
                Type::Operator(Operator {
                    op: TsTypeOperatorOp::Unique,
                    ty,
                    ..
                }) if ty.is_kwd(TsKeywordTypeKind::TsSymbolKeyword) => {}
                _ => {
                    //
                    self.storage
                        .report(box Error::NonSymbolComputedPropInFormOfSymbol { span });
                }
            }
        }

        match mode {
            ComputedPropMode::Class { .. } | ComputedPropMode::Interface => {
                let is_valid_key = is_valid_computed_key(&node.expr);

                let ty = self.expand(node.span, ty.clone()).report(&mut self.storage);

                if let Some(ref ty) = ty {
                    // TODO: Add support for expressions like '' + ''.
                    match **ty {
                        _ if is_valid_key => {}
                        Type::Lit(..) => {}
                        Type::EnumVariant(..) => {}
                        _ if ty.is_kwd(TsKeywordTypeKind::TsSymbolKeyword) || ty.is_unique_symbol() => {}
                        _ => match mode {
                            ComputedPropMode::Interface => errors.push(box Error::TS1169 { span: node.span }),
                            _ => {}
                        },
                    }
                }
            }

            _ => {}
        }

        if match mode {
            ComputedPropMode::Class { has_body } => !has_body,
            ComputedPropMode::Object => errors.is_empty(),
            // TODO:
            ComputedPropMode::Interface => errors.is_empty(),
        } {
            let ty = ty.clone().generalize_lit();
            match *ty.normalize() {
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
                    kind: TsKeywordTypeKind::TsSymbolKeyword,
                    ..
                })
                | Type::Operator(Operator {
                    op: TsTypeOperatorOp::Unique,
                    ty:
                        box Type::Keyword(RTsKeywordType {
                            kind: TsKeywordTypeKind::TsSymbolKeyword,
                            ..
                        }),
                    ..
                }) => {}
                _ if is_symbol_access => {}
                _ => errors.push(box Error::TS2464 { span }),
            }
        }
        if !errors.is_empty() {
            self.storage.report_all(errors);
        }

        // match *ty {
        //     Type::Lit(RTsLitType {
        //         lit: RTsLit::Number(n), ..
        //     }) => return Ok(Key::Num(n)),
        //     Type::Lit(RTsLitType {
        //         lit: RTsLit::Str(s), ..
        //     }) => {
        //         return Ok(Key::Normal {
        //             span: s.span,
        //             sym: s.value,
        //         })
        //     }
        //     _ => {}
        // }

        Ok(Key::Computed(ComputedKey {
            span,
            expr: node.expr.clone(),
            ty,
        }))
    }
}

#[validator]
impl Analyzer<'_, '_> {
    fn validate(&mut self, prop: &RProp) -> ValidationResult<TypeElement> {
        self.record(prop);

        let ctx = Ctx {
            computed_prop_mode: ComputedPropMode::Object,
            ..self.ctx
        };

        let old_this = self.scope.this.take();
        let res = self.with_ctx(ctx).validate_prop_inner(prop);
        self.scope.this = old_this;

        res
    }
}

impl Analyzer<'_, '_> {
    fn validate_prop_inner(&mut self, prop: &RProp) -> ValidationResult<TypeElement> {
        let computed = match prop {
            RProp::KeyValue(ref kv) => match &kv.key {
                RPropName::Computed(c) => {
                    c.visit_with(self);

                    true
                }
                _ => false,
            },
            _ => false,
        };

        let span = prop.span();
        // TODO: Validate prop key

        let shorthand_type_ann = match prop {
            RProp::Shorthand(ref i) => {
                // TODO: Check if RValue is correct
                self.type_of_var(&i, TypeOfMode::RValue, None).report(&mut self.storage)
            }
            _ => None,
        };

        let span = prop.span();

        Ok(match prop {
            RProp::Shorthand(i) => {
                let key = Key::Normal {
                    span,
                    sym: i.sym.clone(),
                };
                PropertySignature {
                    span: prop.span(),
                    key,
                    params: Default::default(),
                    optional: false,
                    readonly: false,
                    type_ann: shorthand_type_ann,
                    type_params: Default::default(),
                }
                .into()
            }

            RProp::KeyValue(ref kv) => {
                let key = kv.key.validate_with(self)?;
                let computed = match kv.key {
                    RPropName::Computed(_) => true,
                    _ => false,
                };
                let ty = kv.value.validate_with_default(self)?;

                PropertySignature {
                    span,
                    key,
                    params: Default::default(),
                    optional: false,
                    readonly: false,
                    type_ann: Some(ty),
                    type_params: Default::default(),
                }
                .into()
            }

            RProp::Assign(ref p) => unimplemented!("validate_key(AssignProperty): {:?}", p),
            RProp::Getter(ref p) => p.validate_with(self)?,
            RProp::Setter(ref p) => {
                let key = p.key.validate_with(self)?;
                let computed = match p.key {
                    RPropName::Computed(_) => true,
                    _ => false,
                };
                let parma_span = p.param.span();
                let mut param = &p.param;

                self.with_child(ScopeKind::Method, Default::default(), {
                    |child| -> ValidationResult<_> {
                        Ok(PropertySignature {
                            span,
                            key,
                            params: vec![param.validate_with(child)?],
                            optional: false,
                            readonly: false,
                            type_ann: Some(Type::any(parma_span)),
                            type_params: Default::default(),
                        }
                        .into())
                    }
                })?
            }

            RProp::Method(ref p) => {
                let key = p.key.validate_with(self)?;
                let computed = match p.key {
                    RPropName::Computed(..) => true,
                    _ => false,
                };

                self.with_child(ScopeKind::Method, Default::default(), {
                    |child: &mut Analyzer| -> ValidationResult<_> {
                        // We mark as wip
                        if !computed {
                            match &p.key {
                                RPropName::Ident(i) => {
                                    child.scope.declaring_prop = Some(i.into());
                                }
                                _ => {}
                            };
                        }

                        let type_params = try_opt!(p.function.type_params.validate_with(child));
                        let params = p.function.params.validate_with(child)?;
                        let mut inferred = None;

                        if let Some(body) = &p.function.body {
                            let mut inferred_ret_ty = child
                                .visit_stmts_for_return(
                                    p.function.span,
                                    p.function.is_async,
                                    p.function.is_generator,
                                    &body.stmts,
                                )?
                                .unwrap_or_else(|| {
                                    box Type::Keyword(RTsKeywordType {
                                        span: body.span,
                                        kind: TsKeywordTypeKind::TsVoidKeyword,
                                    })
                                });

                            // Preserve return type if `this` is not involved in return type.
                            if p.function.return_type.is_none() {
                                inferred_ret_ty = if child
                                    .marks()
                                    .infected_by_this_in_object_literal
                                    .is_marked(&inferred_ret_ty)
                                {
                                    Type::any(span)
                                } else {
                                    inferred_ret_ty
                                };

                                if let Some(m) = &mut child.mutations {
                                    m.for_fns.entry(p.function.node_id).or_default().ret_ty =
                                        Some(inferred_ret_ty.clone());
                                }
                            }

                            inferred = Some(inferred_ret_ty)

                            // TODO: Assign
                        }

                        let ret_ty = try_opt!(p.function.return_type.validate_with(child));
                        let ret_ty = ret_ty.or(inferred);

                        Ok(MethodSignature {
                            span,
                            readonly: false,
                            key,
                            optional: false,
                            params,
                            ret_ty,
                            type_params,
                        }
                        .into())
                    }
                })?
            }
        })
    }
}

#[validator]
impl Analyzer<'_, '_> {
    fn validate(&mut self, n: &RGetterProp) -> ValidationResult<TypeElement> {
        self.record(n);

        let key = n.key.validate_with(self)?;
        let computed = key.is_computed();

        let type_ann = self
            .with_child(ScopeKind::Method, Default::default(), |child| {
                n.key.visit_with(child);

                if let Some(body) = &n.body {
                    let ret_ty = child.visit_stmts_for_return(n.span, false, false, &body.stmts)?;
                    if let None = ret_ty {
                        // getter property must have return statements.
                        child.storage.report(box Error::TS2378 { span: n.key.span() });
                    }

                    return Ok(ret_ty);
                }

                Ok(None)
            })
            .report(&mut self.storage)
            .flatten();

        Ok(PropertySignature {
            span: n.span(),
            key,
            params: Default::default(),
            optional: false,
            readonly: true,
            type_ann: if computed { type_ann } else { Some(Type::any(n.span)) },
            type_params: Default::default(),
        }
        .into())
    }
}

fn prop_key_to_expr(p: &RProp) -> Box<RExpr> {
    match *p {
        RProp::Shorthand(ref i) => box RExpr::Ident(i.clone()),
        RProp::Assign(RAssignProp { ref key, .. }) => box RExpr::Ident(key.clone()),
        RProp::Getter(RGetterProp { ref key, .. })
        | RProp::KeyValue(RKeyValueProp { ref key, .. })
        | RProp::Method(RMethodProp { ref key, .. })
        | RProp::Setter(RSetterProp { ref key, .. }) => prop_name_to_expr(key),
    }
}

pub(super) fn prop_name_to_expr(key: &RPropName) -> Box<RExpr> {
    match key {
        RPropName::Computed(ref p) => p.expr.clone(),
        RPropName::Ident(ref ident) => box RExpr::Ident(ident.clone()),
        RPropName::Str(ref s) => box RExpr::Lit(RLit::Str(RStr { ..s.clone() })),
        RPropName::Num(ref s) => box RExpr::Lit(RLit::Num(RNumber { ..s.clone() })),
        RPropName::BigInt(n) => box RExpr::Lit(RLit::BigInt(n.clone())),
    }
}

fn is_valid_computed_key(key: &RExpr) -> bool {
    let mut v = ValidKeyChecker { valid: true };
    key.visit_with(&mut v);
    v.valid
}

#[derive(Debug)]
struct ValidKeyChecker {
    valid: bool,
}

impl Visit<RIdent> for ValidKeyChecker {
    fn visit(&mut self, _: &RIdent) {
        self.valid = false;
    }
}
