use crate::{
    analyzer::{
        expr::{IdCtx, TypeOfMode},
        pat::PatMode,
        scope::ScopeKind,
        util::ResultExt,
        Analyzer, Ctx,
    },
    ty::{MethodSignature, Operator, PropertySignature, Type, TypeElement, TypeExt},
    validator,
    validator::ValidateWith,
    ValidationResult,
};
use itertools::{EitherOrBoth, Itertools};
use rnode::{Visit, VisitWith};
use stc_ts_ast_rnode::{
    RAssignProp, RComputedPropName, RExpr, RExprOrSuper, RGetterProp, RIdent, RKeyValueProp, RLit, RMemberExpr,
    RMethodProp, RNumber, RPrivateName, RProp, RPropName, RSetterProp, RStr,
};
use stc_ts_errors::{Error, Errors};
use stc_ts_file_analyzer_macros::extra_validator;
use stc_ts_types::{Accessor, ComputedKey, Key, KeywordType, PrivateName, TypeParam};
use stc_ts_utils::PatExt;
use std::borrow::Cow;
use swc_atoms::js_word;
use swc_common::{Span, Spanned, SyntaxContext};
use swc_ecma_ast::*;
use tracing::instrument;

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
    fn validate(&mut self, n: &RPrivateName) -> ValidationResult<PrivateName> {
        Ok(PrivateName {
            span: n.span,
            id: n.id.clone().into(),
        })
    }
}

#[validator]
impl Analyzer<'_, '_> {
    fn validate(&mut self, node: &RComputedPropName) -> ValidationResult<Key> {
        self.record(node);
        let ctx = Ctx {
            in_computed_prop_name: true,
            ..self.ctx
        };

        let span = node.span;
        let mode = self.ctx.computed_prop_mode;

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

        self.with_ctx(ctx).with(|analyzer: &mut Analyzer| {
            let mut check_for_validity = true;
            let mut check_for_symbol_form = true;

            let mut errors = Errors::default();

            let ty = match node.expr.validate_with_default(analyzer) {
                Ok(ty) => ty,
                Err(err) => {
                    check_for_symbol_form = false;
                    match err {
                        Error::TS2585 { span } => Err(Error::TS2585 { span })?,
                        _ => {}
                    }

                    errors.push(err);
                    // TODO: Change this to something else (maybe any)
                    Type::unknown(span, Default::default())
                }
            };

            if match mode {
                ComputedPropMode::Class { has_body } => errors.is_empty(),
                ComputedPropMode::Object => errors.is_empty(),
                // TODO:
                ComputedPropMode::Interface => errors.is_empty(),
            } {
                if !analyzer.is_type_valid_for_computed_key(span, &ty) {
                    check_for_validity = false;

                    analyzer.storage.report(Error::InvalidTypeForComputedProperty {
                        span,
                        ty: box ty.clone(),
                    });
                }
            }

            if check_for_validity {
                match mode {
                    ComputedPropMode::Class { .. } | ComputedPropMode::Interface => {
                        let is_valid_key = is_valid_computed_key(&node.expr);

                        let ty = analyzer
                            .expand(node.span, ty.clone(), Default::default())
                            .report(&mut analyzer.storage);

                        if let Some(ref ty) = ty {
                            // TODO: Add support for expressions like '' + ''.
                            match ty.normalize() {
                                _ if is_valid_key => {}
                                Type::Lit(..) => {}
                                Type::EnumVariant(..) => {}
                                _ if ty.is_kwd(TsKeywordTypeKind::TsSymbolKeyword)
                                    || ty.is_unique_symbol()
                                    || ty.normalize().is_symbol() => {}
                                _ => match mode {
                                    ComputedPropMode::Interface => {
                                        errors.push(Error::TS1169 { span: node.span });
                                        check_for_symbol_form = false;
                                    }
                                    _ => {}
                                },
                            }
                        }
                    }

                    _ => {}
                }
            }

            if check_for_validity && check_for_symbol_form && is_symbol_access {
                match ty.normalize() {
                    Type::Keyword(KeywordType {
                        kind: TsKeywordTypeKind::TsSymbolKeyword,
                        ..
                    })
                    | Type::Symbol(..) => {}
                    Type::Operator(Operator {
                        op: TsTypeOperatorOp::Unique,
                        ty,
                        ..
                    }) if ty.is_kwd(TsKeywordTypeKind::TsSymbolKeyword) => {}
                    _ => {
                        //
                        analyzer
                            .storage
                            .report(Error::NonSymbolComputedPropInFormOfSymbol { span });
                    }
                }
            }

            if !errors.is_empty() {
                analyzer.storage.report_all(errors);
            }

            // match *ty {
            //     Type::Lit(LitType {
            //         lit: RTsLit::Number(n), ..
            //     }) => return Ok(Key::Num(n)),
            //     Type::Lit(LitType {
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
                ty: box ty,
            }))
        })
    }
}

#[validator]
impl Analyzer<'_, '_> {
    fn validate(&mut self, prop: &RProp, object_type: Option<&Type>) -> ValidationResult<TypeElement> {
        self.record(prop);

        let ctx = Ctx {
            computed_prop_mode: ComputedPropMode::Object,
            in_shorthand: match prop {
                RProp::Shorthand(..) => true,
                _ => false,
            },
            ..self.ctx
        };

        let old_this = self.scope.this.take();
        let mut res = self.with_ctx(ctx).validate_prop_inner(prop, object_type);
        self.scope.this = old_this;

        res
    }
}

impl Analyzer<'_, '_> {
    /// Computed properties should not use type parameters defined by the
    /// declaring class.
    ///
    /// See: `computedPropertyNames32_ES5.ts`
    #[extra_validator]
    #[instrument(skip(self, used_type_params, span))]
    pub(crate) fn report_error_for_usage_of_type_param_of_declaring_class(
        &mut self,
        used_type_params: &[TypeParam],
        span: Span,
    ) {
        debug_assert!(self.ctx.in_computed_prop_name);

        for used in used_type_params {
            let scope = self.scope.first_kind(|kind| match kind {
                ScopeKind::Fn
                | ScopeKind::Method { .. }
                | ScopeKind::Module
                | ScopeKind::Constructor
                | ScopeKind::ArrowFn
                | ScopeKind::Class
                | ScopeKind::ObjectLit => true,
                ScopeKind::LoopBody { .. }
                | ScopeKind::Block
                | ScopeKind::Flow
                | ScopeKind::TypeParams
                | ScopeKind::Call => false,
            });
            if let Some(scope) = scope {
                match scope.kind() {
                    ScopeKind::Class => {
                        if scope.declaring_type_params.contains(&used.name) {
                            self.storage
                                .report(Error::DeclaringTypeParamReferencedByComputedPropName { span });
                        }
                    }
                    _ => {
                        dbg!(scope.kind());
                    }
                }
            }
        }
    }

    #[instrument(skip(self, span, ty))]
    fn is_type_valid_for_computed_key(&mut self, span: Span, ty: &Type) -> bool {
        let marks = self.marks();

        let ty = ty.clone().generalize_lit(marks);

        match ty.normalize() {
            Type::Function(..) => return false,
            _ => {}
        }
        let ty = self.normalize(None, Cow::Owned(ty), Default::default());
        let ty = match ty {
            Ok(v) => v,
            Err(err) => {
                self.storage.report(err);
                // Don't report more errors.
                return true;
            }
        };
        match ty.normalize() {
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
                kind: TsKeywordTypeKind::TsSymbolKeyword,
                ..
            })
            | Type::Operator(Operator {
                op: TsTypeOperatorOp::Unique,
                ty:
                    box Type::Keyword(KeywordType {
                        kind: TsKeywordTypeKind::TsSymbolKeyword,
                        ..
                    }),
                ..
            })
            | Type::EnumVariant(..)
            | Type::Symbol(..) => true,

            Type::Param(TypeParam {
                constraint: Some(ty), ..
            }) => {
                if self.is_type_valid_for_computed_key(span, ty) {
                    return true;
                }

                match ty.normalize() {
                    Type::Operator(Operator {
                        op: TsTypeOperatorOp::KeyOf,
                        ..
                    }) => return true,
                    _ => {}
                }

                false
            }

            Type::Union(u) => u.types.iter().all(|ty| {
                ty.is_kwd(TsKeywordTypeKind::TsNullKeyword)
                    || ty.is_kwd(TsKeywordTypeKind::TsUndefinedKeyword)
                    || self.is_type_valid_for_computed_key(span, ty)
            }),

            _ => false,
        }
    }

    #[instrument(skip(self, prop, object_type))]
    fn validate_prop_inner(&mut self, prop: &RProp, object_type: Option<&Type>) -> ValidationResult<TypeElement> {
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
                self.type_of_var(&i, TypeOfMode::RValue, None)
                    .report(&mut self.storage)
                    .map(Box::new)
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
                    span: prop.span().with_ctxt(SyntaxContext::empty()),
                    accessibility: None,
                    readonly: false,
                    key,
                    optional: false,
                    params: Default::default(),
                    type_ann: shorthand_type_ann,
                    type_params: Default::default(),
                    metadata: Default::default(),
                    accessor: Default::default(),
                }
                .into()
            }

            RProp::KeyValue(ref kv) => {
                let key = kv.key.validate_with(self)?;
                let computed = match kv.key {
                    RPropName::Computed(_) => true,
                    _ => false,
                };

                let type_ann = object_type.and_then(|obj| {
                    self.access_property(span, &obj, &key, TypeOfMode::RValue, IdCtx::Var, Default::default())
                        .ok()
                });

                let ty = kv
                    .value
                    .validate_with_args(self, (TypeOfMode::RValue, None, type_ann.as_ref()))?;

                PropertySignature {
                    span,
                    accessibility: None,
                    readonly: false,
                    key,
                    optional: false,
                    params: Default::default(),
                    type_ann: Some(box ty),
                    type_params: Default::default(),
                    metadata: Default::default(),
                    accessor: Default::default(),
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
                let param_span = p.param.span();
                let mut param = &p.param;

                self.with_child(ScopeKind::Method { is_static: false }, Default::default(), {
                    |child: &mut Analyzer| -> ValidationResult<_> {
                        child.ctx.pat_mode = PatMode::Decl;
                        let param = param.validate_with(child)?;

                        p.body.visit_with(child);

                        Ok(PropertySignature {
                            span,
                            accessibility: None,
                            readonly: false,
                            key,
                            optional: false,
                            params: vec![param],
                            type_ann: Some(box Type::any(param_span, Default::default())),
                            type_params: Default::default(),
                            metadata: Default::default(),
                            accessor: Accessor {
                                getter: false,
                                setter: true,
                            },
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
                let method_type_ann = object_type.and_then(|obj| {
                    self.access_property(span, &obj, &key, TypeOfMode::RValue, IdCtx::Var, Default::default())
                        .ok()
                });

                self.with_child(ScopeKind::Method { is_static: false }, Default::default(), {
                    |child: &mut Analyzer| -> ValidationResult<_> {
                        child.ctx.in_async = p.function.is_async;
                        child.ctx.in_generator = p.function.is_generator;

                        match method_type_ann.as_ref().map(|ty| ty.normalize()) {
                            Some(Type::Function(ty)) => {
                                for p in p.function.params.iter().zip_longest(ty.params.iter()) {
                                    match p {
                                        EitherOrBoth::Both(param, ty) => {
                                            // Store type infomations, so the pattern validator can use correct type.
                                            if let Some(pat_node_id) = param.pat.node_id() {
                                                if let Some(m) = &mut child.mutations {
                                                    m.for_pats
                                                        .entry(pat_node_id)
                                                        .or_default()
                                                        .ty
                                                        .get_or_insert_with(|| *ty.ty.clone());
                                                }
                                            }
                                        }
                                        _ => {}
                                    }
                                }
                            }
                            _ => {}
                        }

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

                        let ret_ty = try_opt!(p.function.return_type.validate_with(child));
                        let ret_ty = ret_ty.map(|ty| ty.cheap());
                        child.scope.declared_return_type = ret_ty.clone();

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
                                    Type::Keyword(KeywordType {
                                        span: body.span,
                                        kind: TsKeywordTypeKind::TsVoidKeyword,
                                        metadata: Default::default(),
                                    })
                                });

                            // Preserve return type if `this` is not involved in return type.
                            if p.function.return_type.is_none() {
                                inferred_ret_ty = if inferred_ret_ty.metadata().infected_by_this_in_object_literal {
                                    Type::any(span, Default::default())
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
                        let ret_ty = ret_ty.or(inferred).map(Box::new);

                        Ok(MethodSignature {
                            span,
                            accessibility: None,
                            readonly: false,
                            key,
                            optional: false,
                            params,
                            ret_ty,
                            type_params,
                            metadata: Default::default(),
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
            .with_child(
                ScopeKind::Method { is_static: false },
                Default::default(),
                |child: &mut Analyzer| {
                    if let Some(body) = &n.body {
                        let ret_ty = child.visit_stmts_for_return(n.span, false, false, &body.stmts)?;
                        if let None = ret_ty {
                            // getter property must have return statements.
                            child.storage.report(Error::TS2378 { span: n.key.span() });
                        }

                        return Ok(ret_ty);
                    }

                    Ok(None)
                },
            )
            .report(&mut self.storage)
            .flatten();

        Ok(PropertySignature {
            span: n.span(),
            accessibility: None,
            readonly: true,
            key,
            optional: false,
            params: Default::default(),
            type_ann: if computed {
                type_ann.map(Box::new)
            } else {
                Some(box Type::any(n.span, Default::default()))
            },
            type_params: Default::default(),
            metadata: Default::default(),
            accessor: Accessor {
                getter: true,
                setter: false,
            },
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
