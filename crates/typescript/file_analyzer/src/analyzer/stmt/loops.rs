use super::super::Analyzer;
use crate::{
    analyzer::{expr::TypeOfMode, ScopeKind},
    errors::Error,
    ty::{Array, Type},
    validator,
    validator::ValidateWith,
    ValidationResult,
};
use rnode::VisitMutWith;
use stc_ts_ast_rnode::RExpr;
use stc_ts_ast_rnode::RForInStmt;
use stc_ts_ast_rnode::RForOfStmt;
use stc_ts_ast_rnode::RPat;
use stc_ts_ast_rnode::RVarDecl;
use stc_ts_ast_rnode::RVarDeclOrPat;
use stc_ts_file_analyzer_macros::extra_validator;
use swc_common::{Span, Spanned};

impl Analyzer<'_, '_> {
    #[extra_validator]
    fn check_lhs_of_for_loop(&mut self, e: &mut RVarDeclOrPat) {
        match *e {
            RVarDeclOrPat::VarDecl(ref mut v) => {
                // Store variables
                v.visit_mut_with(self);
            }
            RVarDeclOrPat::Pat(ref mut pat) => match *pat {
                RPat::Expr(ref mut e) => {
                    e.validate_with_args(self, (TypeOfMode::LValue, None, None))?;
                }
                RPat::Ident(ref mut i) => {
                    // TODO: verify
                    self.type_of_var(i, TypeOfMode::LValue, None)?;
                }
                _ => {}
            },
        }
    }

    fn check_rhs_of_for_loop(&mut self, e: &mut RExpr) -> ValidationResult {
        // Check iterable
        e.validate_with_default(self)
    }

    fn validate_for_loop(&mut self, span: Span, lhs: &mut RVarDeclOrPat, rty: Box<Type>) {
        match lhs {
            RVarDeclOrPat::Pat(RPat::Expr(l)) => {
                let lty = match l.validate_with_args(self, (TypeOfMode::LValue, None, None)) {
                    Ok(ty) => ty,
                    Err(..) => return,
                };

                println!("FOO\nL: {:?}", lty);
                match self.assign(
                    &Type::Array(Array {
                        span,
                        elem_type: lty,
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
    fn check_for_of_in_loop(&mut self, span: Span, mut left: &mut RVarDeclOrPat, rhs: &mut RExpr) {
        self.with_child(
            ScopeKind::Flow,
            Default::default(),
            |child| -> ValidationResult<()> {
                child.check_lhs_of_for_loop(left);
                let rty = if match left {
                    RVarDeclOrPat::VarDecl(RVarDecl { ref decls, .. }) => !decls.is_empty(),
                    _ => true,
                } {
                    child.check_rhs_of_for_loop(rhs)?
                } else {
                    return Ok(());
                };

                child.validate_for_loop(span, &mut left, rty);

                Ok(())
            },
        )?;
    }
}

#[validator]
impl Analyzer<'_, '_> {
    fn validate(&mut self, s: &mut RForInStmt) {
        self.check_for_of_in_loop(s.span, &mut s.left, &mut s.right);

        Ok(())
    }
}

#[validator]
impl Analyzer<'_, '_> {
    fn validate(&mut self, s: &mut RForOfStmt) {
        self.check_for_of_in_loop(s.span, &mut s.left, &mut s.right);

        Ok(())
    }
}
