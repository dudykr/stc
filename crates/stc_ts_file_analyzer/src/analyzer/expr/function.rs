use std::borrow::Cow;

use rnode::NodeId;
use stc_ts_ast_rnode::{RArrowExpr, RBlockStmtOrExpr, RNumber, RPat};
use stc_ts_errors::DebugExt;
use stc_ts_types::{
    type_id::DestructureId, Class, ClassMetadata, Function, Key, KeywordType, RestType, Tuple, TupleElement, Type, TypeParam, Union,
};
use stc_ts_utils::PatExt;
use stc_utils::cache::Freeze;
use swc_common::{Span, Spanned};
use swc_ecma_ast::TsKeywordTypeKind;

use super::call_new::ExtractKind;
use crate::{
    analyzer::{assign::AssignOpts, expr::TypeOfMode, pat::PatMode, util::ResultExt, Analyzer, Ctx, ScopeKind},
    ty::TypeExt,
    validator,
    validator::ValidateWith,
    VResult,
};

#[validator]
impl Analyzer<'_, '_> {
    fn validate(&mut self, f: &RArrowExpr, type_ann: Option<&Type>) -> VResult<Function> {
        let marks = self.marks();

        let type_ann = self.expand_type_ann(f.span, type_ann)?;

        self.with_child(ScopeKind::ArrowFn, Default::default(), |child: &mut Analyzer| {
            let mut type_params = try_opt!(f.type_params.validate_with(child));

            if type_params.is_none() {
                if let Some(mutations) = &child.mutations {
                    if let Some(ann) = mutations.for_callable.get(&f.node_id) {
                        type_params = ann.type_params.clone();
                    }
                }
            }

            let params = {
                let ctx = Ctx {
                    pat_mode: PatMode::Decl,
                    allow_ref_declaring: false,
                    is_fn_param: true,
                    ..child.ctx
                };

                child.apply_fn_type_ann(f.span, f.node_id, f.params.iter(), type_ann.as_deref());

                for p in &f.params {
                    child.default_any_pat(p);
                }
                f.params.validate_with(&mut *child.with_ctx(ctx))?
            };

            let declared_ret_ty = match f.return_type.validate_with(child) {
                Some(Ok(ty)) => Some(ty),
                Some(Err(err)) => {
                    child.storage.report(err);
                    Some(Type::any(f.span, Default::default()))
                }
                None => None,
            };
            let declared_ret_ty = match declared_ret_ty {
                Some(ty) => {
                    let span = ty.span();
                    Some(match ty {
                        Type::ClassDef(def) => Type::Class(Class {
                            span,
                            metadata: ClassMetadata {
                                common: def.metadata.common,
                                ..Default::default()
                            },
                            def,
                            tracker: Default::default(),
                        }),
                        _ => ty,
                    })
                }
                None => None,
            }
            .freezed();

            let inferred_return_type = {
                match *f.body {
                    RBlockStmtOrExpr::Expr(ref e) => Some({
                        let ty = e.validate_with_args(child, (TypeOfMode::RValue, None, declared_ret_ty.as_ref()))?;
                        if !child.ctx.in_argument && f.return_type.is_none() && type_ann.is_none() && child.may_generalize(&ty) {
                            ty.generalize_lit()
                        } else {
                            ty
                        }
                    }),
                    RBlockStmtOrExpr::BlockStmt(ref s) => child.visit_stmts_for_return(f.span, f.is_async, f.is_generator, &s.stmts)?,
                }
            }
            .freezed();

            // Remove void from inferred return type.
            let inferred_return_type = inferred_return_type.map(|mut ty| {
                if let Type::Union(ty) = &mut ty {
                    ty.types.retain(|ty| {
                        !matches!(
                            ty.normalize(),
                            Type::Keyword(KeywordType {
                                kind: TsKeywordTypeKind::TsVoidKeyword,
                                ..
                            })
                        )
                    });
                }

                ty
            });

            if let Some(ref declared) = declared_ret_ty {
                let span = inferred_return_type.span();
                if let Some(ref inferred) = inferred_return_type {
                    child
                        .assign_with_opts(
                            &mut Default::default(),
                            declared,
                            inferred,
                            AssignOpts {
                                span,
                                allow_assignment_of_void: Some(true),
                                may_unwrap_promise: f.is_async,
                                ..Default::default()
                            },
                        )
                        .context("tried to assign inferred return type to declared return type of an arrow")
                        .report(&mut child.storage);
                }
            }

            Ok(Function {
                span: f.span,
                params,
                type_params,
                ret_ty: Box::new(
                    declared_ret_ty.unwrap_or_else(|| inferred_return_type.unwrap_or_else(|| Type::void(f.span, Default::default()))),
                ),
                metadata: Default::default(),
                tracker: Default::default(),
            })
        })
    }
}

impl Analyzer<'_, '_> {
    /// `fn_node_id` should be [NodeId] of [stc_ts_ast_rnode::RFunction] or
    /// [stc_ts_ast_rnode::RArrowExpr]
    pub(crate) fn apply_fn_type_ann<'a>(
        &mut self,
        span: Span,
        fn_node_id: NodeId,
        params: impl Iterator<Item = &'a RPat> + Clone,
        type_ann: Option<&Type>,
    ) {
        if let Some(ty) = &type_ann {
            // See functionExpressionContextualTyping1.ts
            //
            // If a type annotation of function is union and there are two or more
            // function types, the type becomes any implicitly.
            let candidates = self.extract_callee_candidates(span, ExtractKind::Call, ty);
            let candidates = match candidates {
                Ok(candidates) => candidates,
                _ => return,
            };
            if candidates.len() != 1 {
                return;
            }

            // We handle type parameters using mutations
            if let Some(mutations) = &mut self.mutations {
                mutations.for_callable.entry(fn_node_id).or_default().type_params = candidates[0].type_params.clone();
            }

            // Handle rest in `ty.params`.
            // If a rest parameter is present, we should adjust offset

            // We do it by creating a tuple and calling access_property
            // TODO(kdy1): This is not efficient.
            let mut params_tuple_els = vec![];
            let mut temp_els = vec![];
            for param in candidates[0].params.iter() {
                match param.pat {
                    RPat::Rest(..) => {
                        params_tuple_els.push(TupleElement {
                            span: param.span,
                            label: None,
                            ty: Box::new(Type::Rest(RestType {
                                span: param.span,
                                ty: param.ty.clone(),
                                metadata: Default::default(),
                                tracker: Default::default(),
                            })),
                            tracker: Default::default(),
                        });
                    }
                    _ => {
                        params_tuple_els.push(TupleElement {
                            span: param.span,
                            label: None,
                            ty: param.ty.clone(),
                            tracker: Default::default(),
                        });
                    }
                }
                match param.ty.normalize() {
                    ty @ Type::Union(..) => {
                        temp_els.push(TupleElement {
                            span: param.span,
                            label: None,
                            ty: Box::new(ty.clone().freezed()),
                            tracker: Default::default(),
                        });
                    }
                    Type::Param(TypeParam {
                        constraint: Some(box ty), ..
                    }) => {
                        if let ty @ Type::Union(..) = ty.normalize() {
                            temp_els.push(TupleElement {
                                span: param.span,
                                label: None,
                                ty: Box::new(ty.clone().freezed()),
                                tracker: Default::default(),
                            });
                        }
                    }
                    ty @ Type::Ref(..) => {
                        let ty = self.normalize(Some(span), Cow::Borrowed(ty), Default::default());
                        if let Ok(ty) = ty {
                            if let ty @ Type::Union(..) = ty.normalize() {
                                temp_els.push(TupleElement {
                                    span: param.span,
                                    label: None,
                                    ty: Box::new(ty.clone().freezed()),
                                    tracker: Default::default(),
                                });
                            }
                        }
                    }
                    _ => {}
                }
            }

            let mut params_tuple = Type::Tuple(Tuple {
                span,
                elems: params_tuple_els,
                metadata: Default::default(),
                tracker: Default::default(),
            });
            params_tuple.freeze();

            let destructure_key = self.get_destructor_unique_key();

            for (idx, param) in params.enumerate() {
                if temp_els.len() == 1 {
                    if let Some(TupleElement { ty: box ty, .. }) = temp_els.first_mut() {
                        if let Some(Union { types, .. }) = ty.as_union_type_mut() {
                            for ty in types {
                                if let Some(Tuple { elems, .. }) = ty.as_tuple_mut() {
                                    if idx < elems.len() {
                                        elems[idx].label = Some(param.clone());
                                    }
                                }
                            }
                        }
                    }
                }

                if param.get_ty().is_some() {
                    continue;
                }

                if let RPat::Rest(..) = param {
                    if let Ok(mut ty) = self.get_rest_elements(Some(param.span()), Cow::Borrowed(&params_tuple), idx) {
                        ty.freeze();

                        if let Some(pat_node_id) = param.node_id() {
                            if let Some(m) = &mut self.mutations {
                                m.for_pats.entry(pat_node_id).or_default().ty.get_or_insert_with(|| ty.into_owned());
                            }
                        }
                    }
                    continue;
                }

                if let Ok(mut ty) = self.access_property(
                    param.span(),
                    &params_tuple,
                    &Key::Num(RNumber {
                        span: param.span(),
                        value: idx as f64,
                        raw: None,
                    }),
                    TypeOfMode::RValue,
                    stc_ts_types::IdCtx::Var,
                    Default::default(),
                ) {
                    // Store type information, so the pattern
                    // validator can use a correct
                    // type.
                    add_destructure_sign(&mut ty, destructure_key);
                    if let Some(pat_node_id) = param.node_id() {
                        if let Some(m) = &mut self.mutations {
                            m.for_pats.entry(pat_node_id).or_default().ty.get_or_insert_with(|| ty.clone());
                        }
                    }
                }
            }

            self.regist_destructure(
                span,
                Some(
                    Type::Tuple(Tuple {
                        span,
                        elems: temp_els,
                        metadata: Default::default(),
                        tracker: Default::default(),
                    })
                    .freezed(),
                ),
                Some(destructure_key),
            );
        }
    }
}

fn add_destructure_sign(ty: &mut Type, key: DestructureId) {
    ty.metadata_mut().destructure_key = key;
    ty.freeze();
}
