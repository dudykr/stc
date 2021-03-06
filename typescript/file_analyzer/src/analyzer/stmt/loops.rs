use super::super::Analyzer;
use crate::analyzer::types::NormalizeTypeOpts;
use crate::analyzer::util::ResultExt;
use crate::validator::ValidateWith;
use crate::{analyzer::ScopeKind, ty::Type, validator, ValidationResult};
use rnode::VisitWith;
use stc_ts_ast_rnode::RExpr;
use stc_ts_ast_rnode::RForInStmt;
use stc_ts_ast_rnode::RForOfStmt;
use stc_ts_ast_rnode::RStmt;
use stc_ts_ast_rnode::RTsKeywordType;
use stc_ts_ast_rnode::RVarDeclOrPat;
use stc_ts_errors::DebugExt;
use stc_ts_errors::Error;
use stc_ts_file_analyzer_macros::extra_validator;
use stc_ts_types::Id;
use stc_ts_utils::find_ids_in_pat;
use std::borrow::Cow;
use swc_common::Span;
use swc_common::Spanned;
use swc_ecma_ast::TsKeywordTypeKind;

#[derive(Clone, Copy)]
enum ForHeadKind {
    In,
    Of,
}

impl Analyzer<'_, '_> {
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

        if rhs.is_kwd(TsKeywordTypeKind::TsObjectKeyword) {
            return Ok(Type::Keyword(RTsKeywordType {
                span: rhs.span(),
                kind: TsKeywordTypeKind::TsStringKeyword,
            }));
        }

        match rhs.normalize() {
            Type::Mapped(m) => {
                // { [P in K]: T[P]; }
                if let Some(constraint) = m.type_param.constraint.as_deref() {
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
        self.check_for_of_in_loop(s.span, &s.left, &s.right, ForHeadKind::Of, &s.body);

        Ok(())
    }
}
