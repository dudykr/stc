pub(super) use self::ambient_decl::AmbientFunctionHandler;
use self::return_type::LoopBreakerFinder;
use super::Analyzer;
use crate::{
    analyzer::{scope::ScopeKind, util::ResultExt},
    validator,
    validator::{Validate, ValidateWith},
    ValidationResult,
};
use stc_types::Type;
use swc_common::DUMMY_SP;
use swc_ecma_ast::*;
use swc_ecma_utils::Value::Known;
use swc_ecma_visit::{VisitMutWith, VisitWith};

mod ambient_decl;
mod decl;
mod loops;
pub(crate) mod return_type;
mod try_catch;

#[validator]
impl Analyzer<'_, '_> {
    fn validate(&mut self, s: &mut Stmt) {
        let old_in_conditional = self.scope.return_values.in_conditional;
        self.scope.return_values.in_conditional |= match s {
            Stmt::If(_) => true,
            Stmt::Switch(_) => true,
            _ => false,
        };

        s.visit_mut_children_with(self);

        self.scope.return_values.in_conditional = old_in_conditional;

        Ok(())
    }
}

impl Analyzer<'_, '_> {
    fn check_for_inifinite_loop(&mut self, test: &Type, body: &Stmt) {
        slog::trace!(self.logger, "Checking for infinite loop");

        // Of `s` is always executed and we enter infinite loop, return type should be
        // never
        if !self.scope.return_values.in_conditional {
            let mut v = LoopBreakerFinder { found: false };
            body.visit_with(&Invalid { span: DUMMY_SP }, &mut v);
            let has_break = v.found;
            if !has_break {
                if let Known(v) = test.as_bool() {
                    self.scope.return_values.forced_never = true;
                    return;
                }
            }
        }
    }
}

#[validator]
impl Analyzer<'_, '_> {
    fn validate(&mut self, node: &mut WhileStmt) {
        let test = node.test.validate_with_default(self)?;
        self.check_for_inifinite_loop(&test, &node.body);

        node.body.visit_mut_with(self);

        Ok(())
    }
}

#[validator]
impl Analyzer<'_, '_> {
    fn validate(&mut self, node: &mut DoWhileStmt) {
        let test = node.test.validate_with_default(self)?;
        self.check_for_inifinite_loop(&test, &node.body);

        node.body.visit_mut_with(self);

        Ok(())
    }
}

#[validator]
impl Analyzer<'_, '_> {
    fn validate(&mut self, node: &mut ForStmt) {
        node.init.visit_mut_with(self);

        let test = try_opt!(node.test.validate_with_default(self));
        let always_true = Type::Lit(TsLitType {
            span: node.span,
            lit: TsLit::Bool(Bool {
                span: DUMMY_SP,
                value: true,
            }),
        });
        self.check_for_inifinite_loop(
            test.as_ref().map(|v| &**v).unwrap_or(&always_true),
            &node.body,
        );

        node.update.visit_mut_with(self);
        node.body.validate_with(self)?;

        Ok(())
    }
}

/// NOTE: We does **not** dig into with statements.
#[validator]
impl Analyzer<'_, '_> {
    fn validate(&mut self, s: &mut WithStmt) {
        s.obj.visit_mut_with(self);

        Ok(())
    }
}

#[validator]
impl Analyzer<'_, '_> {
    fn validate(&mut self, s: &mut BlockStmt) {
        self.with_child(ScopeKind::Block, Default::default(), |analyzer| {
            s.stmts.visit_mut_with(analyzer);
            Ok(())
        })?;

        Ok(())
    }
}

impl Analyzer<'_, '_> {
    /// Validate that parent interfaces are all resolved.
    pub fn resolve_parent_interfaces(&mut self, parents: &mut [TsExprWithTypeArgs]) {
        for parent in parents {
            // Verify parent interface
            let res: Result<_, _> = try {
                let type_args = try_opt!(parent.type_args.validate_with(self));
                self.type_of_ts_entity_name(
                    parent.span,
                    self.ctx.module_id,
                    &parent.expr,
                    type_args,
                )?;
            };

            res.report(&mut self.storage);
        }
    }
}
