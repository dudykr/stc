use super::super::Analyzer;
use crate::validator::ValidateWith;
use crate::{
    analyzer::{expr::TypeOfMode, ScopeKind},
    ty::Type,
    validator, ValidationResult,
};
use rnode::VisitWith;
use stc_ts_ast_rnode::RExpr;
use stc_ts_ast_rnode::RForInStmt;
use stc_ts_ast_rnode::RForOfStmt;
use stc_ts_ast_rnode::RPat;
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
        match *e {
            RVarDeclOrPat::VarDecl(ref v) => {
                // Store variables
                v.visit_with(self);
            }
            RVarDeclOrPat::Pat(ref pat) => {
                match pat {
                    RPat::Expr(ref e) => {
                        e.validate_with_args(self, (TypeOfMode::LValue, None, None))?;
                    }
                    RPat::Ident(ref i) => {
                        // TODO: verify
                        self.type_of_var(&i.id, TypeOfMode::LValue, None)?;
                    }
                    _ => {}
                }
            }
        }
    }

    fn check_rhs_of_for_loop(&mut self, e: &RExpr) -> ValidationResult {
        // Check iterable
        e.validate_with_default(self)
    }

    #[extra_validator]
    fn check_for_of_in_loop(&mut self, span: Span, left: &RVarDeclOrPat, rhs: &RExpr, kind: ForHeadKind) {
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
                    .context("tried to validate rhs of a for in/of loop")?;

                let elem_ty = match kind {
                    ForHeadKind::Of => child
                        .get_iterator_element_type(rhs.span(), Cow::Owned(rty))
                        .context("tried to get the element type of an iterator to calculate type for a for-of loop")?,
                    ForHeadKind::In => {
                        let s = Type::Keyword(RTsKeywordType {
                            span: rhs.span(),
                            kind: TsKeywordTypeKind::TsStringKeyword,
                        });
                        let n = Type::Keyword(RTsKeywordType {
                            span: rhs.span(),
                            kind: TsKeywordTypeKind::TsNumberKeyword,
                        });
                        Cow::Owned(Type::union(vec![s, n]))
                    }
                };

                child.scope.declaring.clear();

                child.check_lhs_of_for_loop(left, &elem_ty, kind);

                Ok(())
            },
        )?;
    }
}

#[validator]
impl Analyzer<'_, '_> {
    fn validate(&mut self, s: &RForInStmt) {
        self.check_for_of_in_loop(s.span, &s.left, &s.right, ForHeadKind::In);

        s.body.visit_with(self);

        Ok(())
    }
}

#[validator]
impl Analyzer<'_, '_> {
    fn validate(&mut self, s: &RForOfStmt) {
        self.check_for_of_in_loop(s.span, &s.left, &s.right, ForHeadKind::Of);

        s.body.visit_with(self);

        Ok(())
    }
}
