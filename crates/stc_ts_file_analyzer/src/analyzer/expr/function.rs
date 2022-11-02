use stc_ts_ast_rnode::{RArrowExpr, RBlockStmtOrExpr, RNumber, RPat};
use stc_ts_types::{Class, ClassMetadata, Function, Key, KeywordType, RestType, Tuple, TupleElement, Type};
use stc_ts_utils::PatExt;
use stc_utils::cache::Freeze;
use swc_common::Spanned;
use swc_ecma_ast::TsKeywordTypeKind;

use crate::{
    analyzer::{assign::AssignOpts, expr::TypeOfMode, pat::PatMode, Analyzer, Ctx, ScopeKind},
    ty::TypeExt,
    validator,
    validator::ValidateWith,
    VResult,
};

#[validator]
impl Analyzer<'_, '_> {
    fn validate(&mut self, f: &RArrowExpr, type_ann: Option<&Type>) -> VResult<Function> {
        self.record(f);

        let marks = self.marks();

        let type_ann = self.expand_type_ann(f.span, type_ann)?;

        self.with_child(ScopeKind::ArrowFn, Default::default(), |child: &mut Analyzer| {
            let type_params = try_opt!(f.type_params.validate_with(child));

            let params = {
                let ctx = Ctx {
                    pat_mode: PatMode::Decl,
                    allow_ref_declaring: false,
                    ..child.ctx
                };

                if let Some(ty) = &type_ann {
                    // See functionExpressionContextualTyping1.ts
                    //
                    // If a type annotation of function is union and there are two or more
                    // function types, the type becomes any implicitly.
                    if ty.iter_union().filter(|ty| ty.is_fn_type()).count() == 1 {
                        for ty in ty.iter_union() {
                            match ty.normalize() {
                                Type::Function(ty) => {
                                    // Handle rest in `ty.params`.
                                    // If a rest parameter is present, we should adjust offset

                                    // We do it by creating a tuple and calling access_property
                                    // TODO(kdy1): This is not efficient.
                                    let mut params_tuple_els = vec![];

                                    for param in ty.params.iter() {
                                        match param.pat {
                                            RPat::Rest(..) => {
                                                params_tuple_els.push(TupleElement {
                                                    span: param.span,
                                                    label: None,
                                                    ty: box Type::Rest(RestType {
                                                        span: param.span,
                                                        ty: param.ty.clone(),
                                                        metadata: Default::default(),
                                                    }),
                                                });
                                            }
                                            _ => {
                                                params_tuple_els.push(TupleElement {
                                                    span: param.span,
                                                    label: None,
                                                    ty: param.ty.clone(),
                                                });
                                            }
                                        }
                                    }

                                    let params_tuple = Type::Tuple(Tuple {
                                        span: ty.span,
                                        elems: params_tuple_els,
                                        metadata: Default::default(),
                                    });

                                    for (idx, param) in f.params.iter().enumerate() {
                                        if let Ok(ty) = child.access_property(
                                            param.span(),
                                            &params_tuple,
                                            &Key::Num(RNumber {
                                                span: param.span(),
                                                value: idx as f64,
                                            }),
                                            TypeOfMode::RValue,
                                            stc_ts_types::IdCtx::Var,
                                            Default::default(),
                                        ) {
                                            // Store type information, so the pattern
                                            // validator can use a correct
                                            // type.
                                            if let Some(pat_node_id) = param.node_id() {
                                                if let Some(m) = &mut child.mutations {
                                                    m.for_pats.entry(pat_node_id).or_default().ty.get_or_insert_with(|| ty.clone());
                                                }
                                            }
                                        }
                                    }
                                }
                                _ => {}
                            }
                        }
                    }
                }

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
                            def: box def,
                        }),
                        _ => ty,
                    })
                }
                None => None,
            }
            .freezed();

            let inferred_return_type = {
                match f.body {
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
                match &mut ty {
                    Type::Union(ty) => {
                        ty.types.retain(|ty| match ty.normalize() {
                            Type::Keyword(KeywordType {
                                kind: TsKeywordTypeKind::TsVoidKeyword,
                                ..
                            }) => false,
                            _ => true,
                        });
                    }
                    _ => {}
                }

                ty
            });

            if let Some(ref declared) = declared_ret_ty {
                let span = inferred_return_type.span();
                if let Some(ref inferred) = inferred_return_type {
                    child.assign_with_opts(
                        &mut Default::default(),
                        AssignOpts {
                            span,
                            allow_assignment_of_void: Some(true),
                            ..Default::default()
                        },
                        declared,
                        inferred,
                    )?;
                }
            }

            Ok(Function {
                span: f.span,
                params,
                type_params,
                ret_ty: box declared_ret_ty
                    .unwrap_or_else(|| inferred_return_type.unwrap_or_else(|| Type::void(f.span, Default::default()))),
                metadata: Default::default(),
            })
        })
    }
}
