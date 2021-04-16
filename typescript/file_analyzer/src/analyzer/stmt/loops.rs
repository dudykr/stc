use super::super::Analyzer;
use crate::analyzer::control_flow::CondFacts;
use crate::analyzer::types::NormalizeTypeOpts;
use crate::analyzer::util::ResultExt;
use crate::analyzer::Ctx;
use crate::validator::ValidateWith;
use crate::{analyzer::ScopeKind, ty::Type, validator, ValidationResult};
use rnode::VisitWith;
use stc_ts_ast_rnode::RDoWhileStmt;
use stc_ts_ast_rnode::RExpr;
use stc_ts_ast_rnode::RForInStmt;
use stc_ts_ast_rnode::RForOfStmt;
use stc_ts_ast_rnode::RIdent;
use stc_ts_ast_rnode::RStmt;
use stc_ts_ast_rnode::RTsEntityName;
use stc_ts_ast_rnode::RTsKeywordType;
use stc_ts_ast_rnode::RVarDeclOrPat;
use stc_ts_ast_rnode::RWhileStmt;
use stc_ts_errors::DebugExt;
use stc_ts_errors::Error;
use stc_ts_file_analyzer_macros::extra_validator;
use stc_ts_types::Id;
use stc_ts_types::ModuleId;
use stc_ts_types::Operator;
use stc_ts_types::Ref;
use stc_ts_types::TypeParamInstantiation;
use stc_ts_utils::find_ids_in_pat;
use std::borrow::Cow;
use swc_common::Span;
use swc_common::Spanned;
use swc_common::DUMMY_SP;
use swc_ecma_ast::TsKeywordTypeKind;
use swc_ecma_ast::TsTypeOperatorOp;

#[derive(Clone, Copy)]
enum ForHeadKind {
    In,
    Of,
}

impl Analyzer<'_, '_> {
    fn validate_loop_body_with_scope(&mut self, body: &RStmt) -> ValidationResult<()> {
        let orig_facts = self.cur_facts.take();

        let mut prev_facts = orig_facts.true_facts.clone();

        // TODO: Loop again if required.
        for _ in 0..2 {
            let facts_from_body: CondFacts =
                self.with_child(ScopeKind::LoopBody, prev_facts.clone(), |child: &mut Analyzer| {
                    body.visit_with(child);

                    Ok(child.cur_facts.true_facts.take())
                })?;

            // We copy `actual` types and type facts from the child scope.

            prev_facts += facts_from_body;
        }

        self.cur_facts.true_facts += prev_facts;

        Ok(())
    }

    #[extra_validator]
    fn check_lhs_of_for_loop(&mut self, e: &RVarDeclOrPat, elem_ty: &Type, kind: ForHeadKind) {
        let span = e.span();

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
                    .report(&mut self.storage);
            }
        }
    }

    fn check_rhs_of_for_loop(&mut self, e: &RExpr) -> ValidationResult {
        // Check iterable
        e.validate_with_default(self)
    }

    fn get_element_type_of_for_in(&mut self, rhs: &Type) -> ValidationResult {
        let rhs = self
            .normalize(
                rhs,
                NormalizeTypeOpts {
                    preserve_mapped: true,
                    ..Default::default()
                },
            )
            .context("tried to normalize a type to handle a for-in loop")?;
        let rhs = rhs.normalize();

        if rhs.is_kwd(TsKeywordTypeKind::TsObjectKeyword) || rhs.is_array() || rhs.is_tuple() {
            return Ok(Type::Keyword(RTsKeywordType {
                span: rhs.span(),
                kind: TsKeywordTypeKind::TsStringKeyword,
            }));
        }

        match rhs.normalize() {
            Type::Mapped(m) => {
                // { [P in keyof K]: T[P]; }
                // =>
                // Extract<keyof K, string>
                if let Some(
                    contraint
                    @
                    Type::Operator(Operator {
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
                                Type::Keyword(RTsKeywordType {
                                    span: rhs.span(),
                                    kind: TsKeywordTypeKind::TsStringKeyword,
                                }),
                            ],
                        }),
                    }));
                }

                // { [P in K]: T[P]; }
                if let Some(..) = m.type_param.constraint.as_deref() {
                    return Ok(Type::Param(m.type_param.clone()));
                }
            }
            _ => {}
        }

        let s = Type::Keyword(RTsKeywordType {
            span: rhs.span(),
            kind: TsKeywordTypeKind::TsStringKeyword,
        });
        let n = Type::Keyword(RTsKeywordType {
            span: rhs.span(),
            kind: TsKeywordTypeKind::TsNumberKeyword,
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

                let rty = rhs
                    .validate_with_default(child)
                    .context("tried to validate rhs of a for in/of loop")
                    .report(&mut child.storage)
                    .unwrap_or_else(|| Type::any(span));

                let elem_ty = match kind {
                    ForHeadKind::Of => child
                        .get_iterator_element_type(rhs.span(), Cow::Owned(rty))
                        .context("tried to get the element type of an iterator to calculate type for a for-of loop")?,
                    ForHeadKind::In => Cow::Owned(
                        child
                            .get_element_type_of_for_in(&rty)
                            .context("tried to calculate the element type for a for-in loop")?,
                    ),
                };

                child.scope.declaring.clear();

                child.check_lhs_of_for_loop(left, &elem_ty, kind);

                child.validate_loop_body_with_scope(&body).report(&mut child.storage);

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
        self.check_for_of_in_loop(s.span, &s.left, &s.right, ForHeadKind::Of, &s.body);

        Ok(())
    }
}

#[validator]
impl Analyzer<'_, '_> {
    fn validate(&mut self, node: &RWhileStmt) {
        let test = {
            let ctx = Ctx {
                in_cond: true,
                ..self.ctx
            };
            node.test.validate_with_default(&mut *self.with_ctx(ctx))?
        };
        self.check_for_inifinite_loop(&test, &node.body);

        self.validate_loop_body_with_scope(&node.body).report(&mut self.storage);

        Ok(())
    }
}

#[validator]
impl Analyzer<'_, '_> {
    fn validate(&mut self, node: &RDoWhileStmt) {
        node.body.visit_with(self);

        let test = node.test.validate_with_default(self)?;
        self.check_for_inifinite_loop(&test, &node.body);

        self.validate_loop_body_with_scope(&node.body).report(&mut self.storage);

        Ok(())
    }
}
