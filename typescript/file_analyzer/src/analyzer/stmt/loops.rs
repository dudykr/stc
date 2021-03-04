use super::super::Analyzer;
use crate::{
    analyzer::{expr::TypeOfMode, ScopeKind},
    ty::{Array, Type},
    validator,
    validator::ValidateWith,
    ValidationResult,
};
use rnode::VisitWith;
use stc_ts_ast_rnode::RExpr;
use stc_ts_ast_rnode::RForInStmt;
use stc_ts_ast_rnode::RForOfStmt;
use stc_ts_ast_rnode::RPat;
use stc_ts_ast_rnode::RVarDecl;
use stc_ts_ast_rnode::RVarDeclOrPat;
use stc_ts_errors::Error;
use stc_ts_file_analyzer_macros::extra_validator;
use swc_common::{Span, Spanned};

impl Analyzer<'_, '_> {
    #[extra_validator]
    fn check_lhs_of_for_loop(&mut self, e: &RVarDeclOrPat) {
        match *e {
            RVarDeclOrPat::VarDecl(ref v) => {
                // Store variables
                v.visit_with(self);
            }
            RVarDeclOrPat::Pat(ref pat) => match pat {
                RPat::Expr(ref e) => {
                    e.validate_with_args(self, (TypeOfMode::LValue, None, None))?;
                }
                RPat::Ident(ref i) => {
                    // TODO: verify
                    self.type_of_var(&i.id, TypeOfMode::LValue, None)?;
                }
                _ => {}
            },
        }
    }

    fn check_rhs_of_for_loop(&mut self, e: &RExpr) -> ValidationResult {
        // Check iterable
        e.validate_with_default(self)
    }

    fn validate_for_loop(&mut self, span: Span, lhs: &RVarDeclOrPat, rty: Type) {
        match lhs {
            RVarDeclOrPat::Pat(RPat::Expr(l)) => {
                let lty = match l.validate_with_args(self, (TypeOfMode::LValue, None, None)) {
                    Ok(ty) => ty,
                    Err(..) => return,
                };

                match self.assign(
                    &Type::Array(Array {
                        span,
                        elem_type: box lty,
                    }),
                    &rty,
                    lhs.span(),
                ) {
                    Ok(..) => {}
                    Err(err) => self.storage.report(err),
                }
            }
            _ => {}
        }
    }

    #[extra_validator]
    fn check_for_of_in_loop(&mut self, span: Span, left: &RVarDeclOrPat, rhs: &RExpr) {
        self.with_child(ScopeKind::Flow, Default::default(), |child| -> ValidationResult<()> {
            child.check_lhs_of_for_loop(left);
            let rty = if match left {
                RVarDeclOrPat::VarDecl(RVarDecl { ref decls, .. }) => !decls.is_empty(),
                _ => true,
            } {
                child.check_rhs_of_for_loop(rhs)?
            } else {
                return Ok(());
            };

            child.validate_for_loop(span, &left, rty);

            Ok(())
        })?;
    }
}

#[validator]
impl Analyzer<'_, '_> {
    fn validate(&mut self, s: &RForInStmt) {
        s.left.visit_with(self);
        self.check_for_of_in_loop(s.span, &s.left, &s.right);

        s.body.visit_with(self);

        Ok(())
    }
}

#[validator]
impl Analyzer<'_, '_> {
    fn validate(&mut self, s: &RForOfStmt) {
        s.left.visit_with(self);

        self.check_for_of_in_loop(s.span, &s.left, &s.right);

        s.body.visit_with(self);

        Ok(())
    }
}
