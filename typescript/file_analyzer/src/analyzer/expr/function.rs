use crate::{
    analyzer::{pat::PatMode, Analyzer, Ctx, ScopeKind},
    ty::TypeExt,
    validator,
    validator::ValidateWith,
    ValidationResult,
};
use itertools::{EitherOrBoth, Itertools};
use stc_ts_ast_rnode::{KeywordType, RArrowExpr, RBlockStmtOrExpr};
use stc_ts_types::{Class, Function, Type};
use stc_ts_utils::{OptionExt, PatExt};
use swc_common::Spanned;
use swc_ecma_ast::TsKeywordTypeKind;

#[validator]
impl Analyzer<'_, '_> {
    fn validate(&mut self, f: &RArrowExpr, type_ann: Option<&Type>) -> ValidationResult<Function> {
        self.record(f);

        let marks = self.marks();

        let type_ann = self.expand_type_ann(type_ann)?;

        self.with_child(ScopeKind::ArrowFn, Default::default(), |child: &mut Analyzer| {
            let type_params = try_opt!(f.type_params.validate_with(child));

            let params = {
                let ctx = Ctx {
                    pat_mode: PatMode::Decl,
                    allow_ref_declaring: false,
                    ..child.ctx
                };

                match type_ann.as_ref().map(|ty| ty.normalize()) {
                    Some(Type::Function(ty)) => {
                        for p in f.params.iter().zip_longest(ty.params.iter()) {
                            match p {
                                EitherOrBoth::Both(param, ty) => {
                                    // Store type infomations, so the pattern validator can use correct type.
                                    if let Some(pat_node_id) = param.node_id() {
                                        if let Some(m) = &mut child.mutations {
                                            m.for_pats
                                                .entry(pat_node_id)
                                                .or_default()
                                                .ty
                                                .fill_with(|| *ty.ty.clone());
                                        }
                                    }
                                }
                                _ => {}
                            }
                        }
                    }
                    _ => {}
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
                    Some(Type::any(f.span))
                }
                None => None,
            };
            let declared_ret_ty = match declared_ret_ty {
                Some(ty) => {
                    let span = ty.span();
                    Some(match ty {
                        Type::ClassDef(def) => Type::Class(Class { span, def: box def }),
                        _ => ty,
                    })
                }
                None => None,
            };

            let inferred_return_type = {
                match f.body {
                    RBlockStmtOrExpr::Expr(ref e) => Some({
                        let ty = e.validate_with_default(child)?;
                        if !child.ctx.in_argument
                            && f.return_type.is_none()
                            && type_ann.is_none()
                            && child.may_generalize(&ty)
                        {
                            ty.generalize_lit(marks)
                        } else {
                            ty
                        }
                    }),
                    RBlockStmtOrExpr::BlockStmt(ref s) => {
                        child.visit_stmts_for_return(f.span, f.is_async, f.is_generator, &s.stmts)?
                    }
                }
            };

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
                    child.assign(span, &mut Default::default(), declared, inferred)?;
                }
            }

            Ok(Function {
                span: f.span,
                params,
                type_params,
                ret_ty: box declared_ret_ty
                    .unwrap_or_else(|| inferred_return_type.unwrap_or_else(|| Type::void(f.span))),
            })
        })
    }
}
