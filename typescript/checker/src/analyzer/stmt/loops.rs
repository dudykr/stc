use super::super::Analyzer;
use crate::{
    analyzer::{expr::TypeOfMode, ScopeKind},
    errors::Error,
    ty::{Array, Type},
    validator,
    validator::{Validate, ValidateWith},
    ValidationResult,
};
use stc_checker_macros::extra_validator;
use swc_common::{Span, Spanned};
use swc_ecma_ast::*;
use swc_ecma_visit::VisitMutWith;

impl Analyzer<'_, '_> {
    #[extra_validator]
    fn check_lhs_of_for_loop(&mut self, e: &mut VarDeclOrPat) {
        match *e {
            VarDeclOrPat::VarDecl(ref mut v) => {
                // Store variables
                v.visit_mut_with(self);
            }
            VarDeclOrPat::Pat(ref mut pat) => match *pat {
                Pat::Expr(ref mut e) => {
                    e.validate_with_args(self, (TypeOfMode::LValue, None, None))?;
                }
                Pat::Ident(ref mut i) => {
                    // TODO: verify
                    self.type_of_var(i, TypeOfMode::LValue, None)?;
                }
                _ => {}
            },
        }
    }

    fn check_rhs_of_for_loop(&mut self, e: &mut Expr) -> ValidationResult {
        // Check iterable
        e.validate_with_default(self)
    }

    fn validate_for_loop(&mut self, span: Span, lhs: &mut VarDeclOrPat, rty: Box<Type>) {
        match lhs {
            VarDeclOrPat::Pat(Pat::Expr(l)) => {
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
    fn check_for_of_in_loop(&mut self, span: Span, mut left: &mut VarDeclOrPat, rhs: &mut Expr) {
        self.with_child(
            ScopeKind::Flow,
            Default::default(),
            |child| -> ValidationResult<()> {
                child.check_lhs_of_for_loop(left);
                let rty = if match left {
                    VarDeclOrPat::VarDecl(VarDecl { ref decls, .. }) => !decls.is_empty(),
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
    fn validate(&mut self, s: &mut ForInStmt) {
        self.check_for_of_in_loop(s.span, &mut s.left, &mut s.right);

        Ok(())
    }
}

#[validator]
impl Analyzer<'_, '_> {
    fn validate(&mut self, s: &mut ForOfStmt) {
        self.check_for_of_in_loop(s.span, &mut s.left, &mut s.right);

        Ok(())
    }
}
