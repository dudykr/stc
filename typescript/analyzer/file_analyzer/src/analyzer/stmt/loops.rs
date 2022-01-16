use crate::{
    analyzer::{types::NormalizeTypeOpts, util::ResultExt, Analyzer, Ctx, ScopeKind},
    ty::Type,
    util::is_str_or_union,
    validator,
    validator::ValidateWith,
    ValidationResult,
};
use rnode::VisitWith;
use stc_ts_ast_rnode::{
    RDoWhileStmt, RExpr, RForInStmt, RForOfStmt, RIdent, RPat, RStmt, RTsEntityName, RVarDecl, RVarDeclOrPat,
    RWhileStmt,
};
use stc_ts_errors::{DebugExt, Error};
use stc_ts_file_analyzer_macros::extra_validator;
use stc_ts_types::{
    Id, KeywordType, KeywordTypeMetadata, ModuleId, Operator, Ref, RefMetadata, TypeParamInstantiation,
};
use stc_ts_utils::{find_ids_in_pat, PatExt};
use stc_utils::cache::Freeze;
use std::borrow::Cow;
use swc_common::{Span, Spanned, DUMMY_SP};
use swc_ecma_ast::{EsVersion, TsKeywordTypeKind, TsTypeOperatorOp, VarDeclKind};

#[derive(Clone, Copy)]
enum ForHeadKind {
    In,
    Of { is_awaited: bool },
}

impl Analyzer<'_, '_> {
    #[extra_validator]
    fn validate_lhs_of_for_loop(&mut self, e: &RVarDeclOrPat, elem_ty: &Type, kind: ForHeadKind) {
        let span = e.span();

        match self.validate_lhs_of_for_in_of_loop(&e, kind) {
            Ok(()) => {}
            Err(err) => {
                self.storage.report(err);
            }
        }

        match *e {
            RVarDeclOrPat::VarDecl(ref v) => {
                // It is a parsing error if there are multiple variable declarators.
                // So we only handle the case where there's only one variable declarator.
                if v.decls.len() == 1 {
                    if let Some(m) = &mut self.mutations {
                        // We use node id of a variable declarator with `for_pats`.
                        let node_id = v.decls.first().unwrap().node_id;
                        m.for_pats
                            .entry(node_id)
                            .or_default()
                            .ty
                            .get_or_insert_with(|| elem_ty.clone());
                    }
                    // Add types.
                }

                // Store variables
                v.visit_with(self);
            }
            RVarDeclOrPat::Pat(ref pat) => {
                self.try_assign_pat(span, &pat, elem_ty)
                    .context("tried to assign to the pattern of a for-of/for-in loop")
                    .convert_err(|err| {
                        match kind {
                            ForHeadKind::In => {
                                if err.is_assign_failure() {
                                    return Error::WrongTypeForLhsOfForInLoop { span: err.span() };
                                }
                            }
                            _ => {}
                        }

                        err
                    })
                    .report(&mut self.storage);
            }
        }
    }

    fn validate_lhs_of_for_in_of_loop(&mut self, e: &RVarDeclOrPat, kind: ForHeadKind) -> ValidationResult<()> {
        match e {
            RVarDeclOrPat::VarDecl(v) => {
                if v.decls.len() >= 1 {
                    self.validate_lhs_of_for_in_of_loop_pat(&v.decls[0].name, kind)
                } else {
                    Ok(())
                }
            }
            RVarDeclOrPat::Pat(p) => self.validate_lhs_of_for_in_of_loop_pat(p, kind),
        }
    }

    fn validate_lhs_of_for_in_of_loop_pat(&mut self, p: &RPat, kind: ForHeadKind) -> ValidationResult<()> {
        match p {
            RPat::Object(..) | RPat::Array(..) => match kind {
                ForHeadKind::In => Err(Error::DestructuringBindingNotAllowedInLhsOfForIn { span: p.span() }),
                ForHeadKind::Of { .. } => Ok(()),
            },
            RPat::Expr(e) => self.validate_lhs_of_for_in_of_loop_expr(e, kind),
            _ => Ok(()),
        }
    }

    fn validate_lhs_of_for_in_of_loop_expr(&mut self, e: &RExpr, kind: ForHeadKind) -> ValidationResult<()> {
        match e {
            RExpr::Ident(..) | RExpr::This(..) | RExpr::Member(..) => Ok(()),
            // We use different error code for this.
            RExpr::Assign(..) => Ok(()),
            _ => match kind {
                ForHeadKind::In => Err(Error::InvalidExprOfLhsOfForIn { span: e.span() }),
                ForHeadKind::Of { .. } => Err(Error::InvalidExprOfLhsOfForOf { span: e.span() }),
            },
        }
    }

    fn get_element_type_of_for_in(&mut self, rhs: &Type) -> ValidationResult {
        let rhs = self
            .normalize(
                None,
                Cow::Borrowed(rhs),
                NormalizeTypeOpts {
                    preserve_mapped: true,
                    ..Default::default()
                },
            )
            .context("tried to normalize a type to handle a for-in loop")?;
        let rhs = rhs.normalize();

        if rhs.is_kwd(TsKeywordTypeKind::TsObjectKeyword) || rhs.is_array() || rhs.is_tuple() {
            return Ok(Type::Keyword(KeywordType {
                span: rhs.span(),
                kind: TsKeywordTypeKind::TsStringKeyword,
                metadata: KeywordTypeMetadata {
                    common: rhs.metadata(),
                    ..Default::default()
                },
            }));
        }

        match rhs.normalize() {
            Type::Mapped(m) => {
                // { [P in keyof K]: T[P]; }
                // =>
                // Extract<keyof K, string>
                if let Some(
                    contraint @ Type::Operator(Operator {
                        op: TsTypeOperatorOp::KeyOf,
                        ..
                    }),
                ) = m.type_param.constraint.as_deref().map(|ty| ty.normalize())
                {
                    // Extract<keyof T
                    return Ok(Type::Ref(Ref {
                        span: m.span,
                        ctxt: ModuleId::builtin(),
                        type_name: RTsEntityName::Ident(RIdent::new("Extract".into(), DUMMY_SP)),
                        type_args: Some(box TypeParamInstantiation {
                            span: DUMMY_SP,
                            params: vec![
                                contraint.clone(),
                                Type::Keyword(KeywordType {
                                    span: rhs.span(),
                                    kind: TsKeywordTypeKind::TsStringKeyword,
                                    metadata: KeywordTypeMetadata {
                                        common: rhs.metadata(),
                                        ..Default::default()
                                    },
                                }),
                            ],
                        }),
                        metadata: RefMetadata {
                            common: m.metadata.common,
                            ..Default::default()
                        },
                    }));
                }

                // { [P in K]: T[P]; }
                if let Some(..) = m.type_param.constraint.as_deref() {
                    return Ok(Type::Param(m.type_param.clone()));
                }
            }
            _ => {}
        }

        let s = Type::Keyword(KeywordType {
            span: rhs.span(),
            kind: TsKeywordTypeKind::TsStringKeyword,
            metadata: KeywordTypeMetadata {
                common: rhs.metadata(),
                ..Default::default()
            },
        });
        let n = Type::Keyword(KeywordType {
            span: rhs.span(),
            kind: TsKeywordTypeKind::TsNumberKeyword,
            metadata: KeywordTypeMetadata {
                common: rhs.metadata(),
                ..Default::default()
            },
        });
        Ok(Type::union(vec![s, n]))
    }

    #[extra_validator]
    fn check_for_of_in_loop(&mut self, span: Span, left: &RVarDeclOrPat, rhs: &RExpr, kind: ForHeadKind, body: &RStmt) {
        self.with_child(
            ScopeKind::Flow,
            Default::default(),
            |child: &mut Analyzer| -> ValidationResult<()> {
                // Error should not be `no such var` if it's used in rhs.
                let created_vars: Vec<Id> = match left {
                    RVarDeclOrPat::VarDecl(v) => find_ids_in_pat(&v.decls),
                    RVarDeclOrPat::Pat(_) => {
                        vec![]
                    }
                };
                debug_assert_eq!(child.scope.declaring, Vec::<Id>::new());
                child.scope.declaring.extend(created_vars);

                child.ctx.allow_ref_declaring = match left {
                    RVarDeclOrPat::VarDecl(RVarDecl {
                        kind: VarDeclKind::Var, ..
                    }) => true,
                    _ => false,
                };

                // Type annotation on lhs of for in/of loops is invalid.
                match left {
                    RVarDeclOrPat::VarDecl(RVarDecl { decls, .. }) => {
                        if decls.len() >= 1 {
                            if decls[0].name.get_ty().is_some() {
                                match kind {
                                    ForHeadKind::In => {
                                        child
                                            .storage
                                            .report(Error::TypeAnnOnLhsOfForInLoops { span: decls[0].span });
                                    }
                                    ForHeadKind::Of { .. } => {
                                        child
                                            .storage
                                            .report(Error::TypeAnnOnLhsOfForOfLoops { span: decls[0].span });
                                    }
                                }
                            }
                        }
                    }
                    _ => {}
                }

                let rhs_ctx = Ctx {
                    cannot_be_tuple: true,
                    // use_undefined_for_empty_tuple: true,
                    ..child.ctx
                };

                let rty = rhs
                    .validate_with_default(&mut *child.with_ctx(rhs_ctx))
                    .context("tried to validate rhs of a for in/of loop");
                let rty = rty
                    .report(&mut child.storage)
                    .unwrap_or_else(|| Type::any(span, Default::default()));

                match kind {
                    ForHeadKind::Of { is_awaited: false } => {
                        if child.env.target() < EsVersion::Es5 {
                            if rty
                                .iter_union()
                                .flat_map(|ty| ty.iter_union())
                                .flat_map(|ty| ty.iter_union())
                                .any(|ty| is_str_or_union(&ty))
                            {
                                child.storage.report(Error::ForOfStringUsedInEs3 { span })
                            }
                        }
                    }
                    _ => {}
                }

                let mut elem_ty = match kind {
                    ForHeadKind::Of { is_awaited: false } => child
                        .get_iterator_element_type(rhs.span(), Cow::Owned(rty), false)
                        .convert_err(|err| match err {
                            Error::NotArrayType { span }
                                if match rhs {
                                    RExpr::Lit(..) => true,
                                    _ => false,
                                } =>
                            {
                                Error::NotArrayTypeNorStringType { span }
                            }
                            _ => err,
                        })
                        .context("tried to get the element type of an iterator to calculate type for a for-of loop")
                        .report(&mut child.storage)
                        .unwrap_or_else(|| Cow::Owned(Type::any(span, Default::default()))),

                    ForHeadKind::Of { is_awaited: true } => child
                        .get_async_iterator_elem_type(rhs.span(), Cow::Owned(rty))
                        .context("tried to get element type of an async iteratror")
                        .report(&mut child.storage)
                        .unwrap_or_else(|| Cow::Owned(Type::any(span, Default::default()))),

                    ForHeadKind::In => Cow::Owned(
                        child
                            .get_element_type_of_for_in(&rty)
                            .context("tried to calculate the element type for a for-in loop")
                            .report(&mut child.storage)
                            .unwrap_or_else(|| Type::any(span, Default::default())),
                    ),
                };
                elem_ty.make_clone_cheap();

                child.scope.declaring.clear();

                child.validate_lhs_of_for_loop(left, &elem_ty, kind);

                body.visit_with(child);

                Ok(())
            },
        )?;
    }
}

#[validator]
impl Analyzer<'_, '_> {
    fn validate(&mut self, s: &RForInStmt) {
        self.check_for_of_in_loop(s.span, &s.left, &s.right, ForHeadKind::In, &s.body);

        Ok(())
    }
}

#[validator]
impl Analyzer<'_, '_> {
    fn validate(&mut self, s: &RForOfStmt) {
        self.check_for_of_in_loop(
            s.span,
            &s.left,
            &s.right,
            ForHeadKind::Of {
                is_awaited: s.await_token.is_some(),
            },
            &s.body,
        );

        Ok(())
    }
}

#[validator]
impl Analyzer<'_, '_> {
    fn validate(&mut self, node: &RWhileStmt) {
        node.test.visit_with(self);
        node.body.visit_with(self);

        Ok(())
    }
}

#[validator]
impl Analyzer<'_, '_> {
    fn validate(&mut self, node: &RDoWhileStmt) {
        node.body.visit_with(self);
        node.test.visit_with(self);

        Ok(())
    }
}
