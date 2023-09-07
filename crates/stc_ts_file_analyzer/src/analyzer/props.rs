use std::borrow::Cow;

use rnode::{Visit, VisitWith};
use stc_ts_ast_rnode::{RComputedPropName, RExpr, RGetterProp, RIdent, RMemberExpr, RPrivateName, RProp, RPropName};
use stc_ts_errors::{ErrorKind, Errors};
use stc_ts_file_analyzer_macros::extra_validator;
use stc_ts_types::{Accessor, ComputedKey, Index, Key, KeywordType, PrivateName, TypeParam, Unique};
use stc_utils::{cache::Freeze, dev_span};
use swc_atoms::js_word;
use swc_common::{Span, Spanned, SyntaxContext};
use swc_ecma_ast::*;

use crate::{
    analyzer::{
        expr::{IdCtx, TypeOfMode},
        pat::PatMode,
        scope::ScopeKind,
        util::ResultExt,
        Analyzer, Ctx,
    },
    ty::{MethodSignature, PropertySignature, Type, TypeElement, TypeExt},
    type_facts::TypeFacts,
    validator,
    validator::ValidateWith,
    VResult,
};

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
    fn validate(&mut self, node: &RPropName) -> VResult<Key> {
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
    fn validate(&mut self, n: &RPrivateName) -> VResult<PrivateName> {
        Ok(PrivateName {
            span: n.span,
            id: n.id.clone().into(),
        })
    }
}

#[validator]
impl Analyzer<'_, '_> {
    fn validate(&mut self, node: &RComputedPropName) -> VResult<Key> {
        let ctx = Ctx {
            in_computed_prop_name: true,
            ..self.ctx
        };

        let span = node.span;
        let mode = self.ctx.computed_prop_mode;

        let is_symbol_access = matches!(
            *node.expr,
            RExpr::Member(RMemberExpr {
                obj: box RExpr::Ident(RIdent {
                    sym: js_word!("Symbol"), ..
                }),
                ..
            })
        );

        self.with_ctx(ctx).with(|analyzer: &mut Analyzer| {
            let mut check_for_validity = true;

            let mut errors = Errors::default();

            let mut ty = match node.expr.validate_with_default(analyzer) {
                Ok(ty) => ty,
                Err(err) => {
                    if let ErrorKind::TS2585 { span } = *err {
                        Err(ErrorKind::TS2585 { span })?
                    }

                    errors.push(err);
                    // TODO(kdy1): Change this to something else (maybe any)
                    Type::unknown(span, Default::default())
                }
            };
            ty.freeze();

            if match mode {
                ComputedPropMode::Class { has_body } => errors.is_empty(),
                ComputedPropMode::Object => errors.is_empty(),
                // TODO(kdy1):
                ComputedPropMode::Interface => errors.is_empty(),
            } {
                if !analyzer.is_type_valid_for_computed_key(span, &ty) {
                    check_for_validity = false;

                    analyzer.storage.report(
                        ErrorKind::InvalidTypeForComputedProperty {
                            span,
                            ty: Box::new(ty.clone()),
                        }
                        .into(),
                    );
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
                            // TODO(kdy1): Add support for expressions like '' + ''.
                            match ty.normalize() {
                                _ if is_valid_key => {}
                                Type::Lit(..) => {}
                                Type::EnumVariant(..) => {}
                                _ if ty.is_kwd(TsKeywordTypeKind::TsSymbolKeyword) || ty.is_unique_symbol() || ty.is_symbol() => {}
                                _ => {
                                    if let ComputedPropMode::Interface = mode {
                                        errors.push(ErrorKind::TS1169 { span: node.span }.into());
                                    }
                                }
                            }
                        }
                    }

                    _ => {}
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
                ty: Box::new(ty),
            }))
        })
    }
}

#[validator]
impl Analyzer<'_, '_> {
    fn validate(&mut self, prop: &RProp, object_type: Option<&Type>) -> VResult<TypeElement> {
        let ctx = Ctx {
            computed_prop_mode: ComputedPropMode::Object,
            in_shorthand: matches!(prop, RProp::Shorthand(..)),
            ..self.ctx
        };

        let old_this = self.scope.this.take();
        let res = self.with_ctx(ctx).validate_prop_inner(prop, object_type);
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
    pub(crate) fn report_error_for_usage_of_type_param_of_declaring_class(&mut self, used_type_params: &[TypeParam], span: Span) {
        let _tracing = dev_span!("report_error_for_usage_of_type_param_of_declaring_class");

        debug_assert!(self.ctx.in_computed_prop_name);

        for used in used_type_params {
            let scope = self.scope.first_kind(|kind| match kind {
                ScopeKind::Fn
                | ScopeKind::Method { .. }
                | ScopeKind::Module
                | ScopeKind::Constructor
                | ScopeKind::ArrowFn
                | ScopeKind::Class
                | ScopeKind::ClassStaticBlock
                | ScopeKind::ObjectLit => true,
                ScopeKind::LoopBody { .. } | ScopeKind::Block | ScopeKind::Flow | ScopeKind::TypeParams | ScopeKind::Call => false,
            });
            if let Some(scope) = scope {
                match scope.kind() {
                    ScopeKind::Class => {
                        if scope.declaring_type_params.contains(&used.name) {
                            self.storage
                                .report(ErrorKind::DeclaringTypeParamReferencedByComputedPropName { span }.into());
                        }
                    }
                    _ => {
                        dbg!(scope.kind());
                    }
                }
            }
        }
    }

    fn is_type_valid_for_computed_key(&mut self, span: Span, ty: &Type) -> bool {
        let _tracing = dev_span!("is_type_valid_for_computed_key");

        if (ty.metadata().resolved_from_var || ty.metadata().prevent_generalization) && (ty.is_str_lit() || ty.is_num_lit()) {
            return true;
        }

        let ty = ty.clone().generalize_lit();

        if let Type::Function(..) = ty.normalize() {
            return false;
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
            | Type::Unique(Unique {
                ty:
                    box Type::Keyword(KeywordType {
                        kind: TsKeywordTypeKind::TsSymbolKeyword,
                        ..
                    }),
                ..
            })
            | Type::EnumVariant(..)
            | Type::Symbol(..)
            | Type::Tpl(..) => true,

            Type::Param(TypeParam { constraint: Some(ty), .. }) => {
                if self.is_type_valid_for_computed_key(span, ty) {
                    return true;
                }

                if let Type::Index(Index { .. }) = ty.normalize() {
                    return true;
                }

                false
            }

            Type::Union(u) => u
                .types
                .iter()
                .all(|ty| ty.is_null_or_undefined() || self.is_type_valid_for_computed_key(span, ty)),

            _ => false,
        }
    }

    fn validate_prop_inner(&mut self, prop: &RProp, object_type: Option<&Type>) -> VResult<TypeElement> {
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
        // TODO(kdy1): Validate prop key

        let shorthand_type_ann = match prop {
            RProp::Shorthand(i) => {
                // TODO(kdy1): Check if RValue is correct
                self.type_of_var(i, TypeOfMode::RValue, None)
                    .report(&mut self.storage)
                    .map(Box::new)
            }
            _ => None,
        };

        let span = prop.span();

        Ok(match prop {
            RProp::Shorthand(i) => {
                let key = Key::Normal { span, sym: i.sym.clone() };
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
                let computed = matches!(kv.key, RPropName::Computed(_));

                let type_ann = object_type.and_then(|obj| {
                    self.access_property(span, obj, &key, TypeOfMode::RValue, IdCtx::Var, Default::default())
                        .ok()
                        .freezed()
                });

                let ty = kv.value.validate_with_args(self, (TypeOfMode::RValue, None, type_ann.as_ref()))?;

                PropertySignature {
                    span,
                    accessibility: None,
                    readonly: false,
                    key,
                    optional: false,
                    params: Default::default(),
                    type_ann: Some(Box::new(ty)),
                    type_params: Default::default(),
                    metadata: Default::default(),
                    accessor: Default::default(),
                }
                .into()
            }

            RProp::Assign(ref p) => unimplemented!("validate_key(AssignProperty): {:?}", p),
            RProp::Getter(ref p) => {
                self.ctx.get_accessor_prop = true;
                p.validate_with(self)?
            }
            RProp::Setter(ref p) => {
                let key = p.key.validate_with(self)?;
                let computed = matches!(p.key, RPropName::Computed(_));
                let param_span = p.param.span();
                let param = &p.param;

                self.with_child(ScopeKind::Method { is_static: false }, Default::default(), {
                    |child: &mut Analyzer| -> VResult<_> {
                        child.ctx.pat_mode = PatMode::Decl;
                        child.ctx.set_accessor_prop = true;
                        let param = param.validate_with(child)?;

                        p.body.visit_with(child);

                        Ok(PropertySignature {
                            span,
                            accessibility: None,
                            readonly: false,
                            key,
                            optional: false,
                            params: vec![param],
                            type_ann: Some(Box::new(Type::any(param_span, Default::default()))),
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
                let computed = matches!(p.key, RPropName::Computed(..));
                let method_type_ann = object_type
                    .cloned()
                    .map(|ty| self.apply_type_facts_to_type(TypeFacts::NEUndefinedOrNull, ty))
                    .and_then(|obj| {
                        self.access_property(span, &obj, &key, TypeOfMode::RValue, IdCtx::Var, Default::default())
                            .ok()
                    });

                self.with_child(ScopeKind::Method { is_static: false }, Default::default(), {
                    |child: &mut Analyzer| -> VResult<_> {
                        child.ctx.in_async = p.function.is_async;
                        child.ctx.in_generator = p.function.is_generator;

                        child.apply_fn_type_ann(
                            p.function.span,
                            p.function.node_id,
                            p.function.params.iter().map(|v| &v.pat),
                            method_type_ann.as_ref(),
                        );

                        // We mark as wip
                        if !computed {
                            if let RPropName::Ident(i) = &p.key {
                                child.scope.declaring_prop = Some(i.into());
                            };
                        }

                        let type_params = try_opt!(p.function.type_params.validate_with(child));
                        let params = p.function.params.validate_with(child)?;

                        let ret_ty = try_opt!(p.function.return_type.validate_with(child));
                        let ret_ty = ret_ty.map(|ty| ty.freezed());
                        child.scope.declared_return_type = ret_ty.clone();

                        let mut inferred = None;

                        if let Some(body) = &p.function.body {
                            let mut inferred_ret_ty = child
                                .visit_stmts_for_return(
                                    p.function.span.with_ctxt(SyntaxContext::empty()),
                                    p.function.is_async,
                                    p.function.is_generator,
                                    &body.stmts,
                                )?
                                .unwrap_or_else(|| {
                                    Type::Keyword(KeywordType {
                                        span: body.span,
                                        kind: TsKeywordTypeKind::TsVoidKeyword,
                                        metadata: Default::default(),
                                        tracker: Default::default(),
                                    })
                                });
                            inferred_ret_ty.freeze();

                            // Preserve return type if `this` is not involved in return type.
                            if p.function.return_type.is_none() {
                                inferred_ret_ty = if inferred_ret_ty.metadata().infected_by_this_in_object_literal {
                                    Type::any(span, Default::default())
                                } else {
                                    inferred_ret_ty
                                };

                                if let Some(m) = &mut child.mutations {
                                    m.for_fns.entry(p.function.node_id).or_default().ret_ty = Some(inferred_ret_ty.clone());
                                }
                            }

                            inferred = Some(inferred_ret_ty)

                            // TODO(kdy1): Assign
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
    fn validate(&mut self, n: &RGetterProp) -> VResult<TypeElement> {
        let key: Key = n.key.validate_with(self)?;

        let type_ann = self
            .with_child(
                ScopeKind::Method { is_static: false },
                Default::default(),
                |child: &mut Analyzer| {
                    if let Some(body) = &n.body {
                        let ret_ty = child.visit_stmts_for_return(n.span, false, false, &body.stmts)?;
                        if ret_ty.is_none() {
                            // getter property must have return statements.
                            child.storage.report(ErrorKind::TS2378 { span: n.key.span() }.into());
                        }

                        return Ok(ret_ty);
                    }

                    Ok(None)
                },
            )
            .report(&mut self.storage)
            .flatten();

        let computed = key.is_computed() | (matches!(type_ann, Some(Type::Lit(..))) && key.is_normal());

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
                Some(Box::new(Type::any(n.span, Default::default())))
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
