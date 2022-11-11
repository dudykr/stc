use std::time::Instant;

use rnode::VisitWith;
use stc_ts_ast_rnode::{RBlockStmt, RBool, RExprStmt, RForStmt, RModuleItem, RStmt, RTsExprWithTypeArgs, RTsLit, RWithStmt};
use stc_ts_errors::Error;
use stc_ts_types::{LitType, Type};
use stc_utils::stack;
use swc_common::{Spanned, DUMMY_SP};
use swc_ecma_utils::Value::Known;
use tracing::{instrument, span, trace, warn, Level};

use self::return_type::LoopBreakerFinder;
use crate::{
    analyzer::{scope::ScopeKind, util::ResultExt, Analyzer, Ctx},
    validator,
    validator::ValidateWith,
};

mod ambient_decl;
mod loops;
pub(crate) mod return_type;
mod try_catch;
mod var_decl;

#[validator]
impl Analyzer<'_, '_> {
    fn validate(&mut self, i: &RModuleItem) {
        let _stack = stack::start(100);

        i.visit_children_with(self);

        Ok(())
    }
}

#[validator]
impl Analyzer<'_, '_> {
    fn validate(&mut self, s: &RStmt) {
        let span = s.span();
        let line_col = self.line_col(span);

        let tracing_span = span!(Level::TRACE, "Statement", line_col = &*line_col);
        let _tracing_guard = tracing_span.enter();

        warn!("Statement start");
        let start = Instant::now();

        if self.rule().always_strict && !self.rule().allow_unreachable_code && self.ctx.in_unreachable {
            self.storage.report(Error::UnreachableCode { span: s.span() });
        }

        let old_in_conditional = self.scope.return_values.in_conditional;
        self.scope.return_values.in_conditional |= match s {
            RStmt::If(_) => true,
            RStmt::Switch(_) => true,
            RStmt::While(..) | RStmt::DoWhile(..) | RStmt::For(..) | RStmt::ForIn(..) | RStmt::ForOf(..) => true,
            _ => false,
        };

        s.visit_children_with(&mut *self.with_ctx(Ctx {
            ignore_facts: false,
        s.visit_children_with(&mut self.with_ctx(Ctx {
            do_not_create_cond_facts: false,
            ..self.ctx
        }));

        self.scope.return_values.in_conditional = old_in_conditional;

        let end = Instant::now();

        warn!(
            kind = "perf",
            op = "validate (Stmt)",
            "({}): Statement validation done. (time = {:?}",
            line_col,
            end - start
        );

        Ok(())
    }
}

#[validator]
impl Analyzer<'_, '_> {
    fn validate(&mut self, s: &RExprStmt) {
        s.visit_children_with(&mut *self.with_ctx(Ctx {
            do_not_create_cond_facts: true,
            ..self.ctx
        }));

        Ok(())
    }
}

impl Analyzer<'_, '_> {
    fn check_for_inifinite_loop(&mut self, test: &Type, body: &RStmt) {
        trace!("Checking for infinite loop");

        // Of `s` is always executed and we enter infinite loop, return type should be
        // never
        if !self.scope.return_values.in_conditional {
            let mut v = LoopBreakerFinder { found: false };
            body.visit_with(&mut v);
            let has_break = v.found;
            if !has_break {
                if let Known(v) = test.as_bool() {
                    self.ctx.in_unreachable = true;
                    return;
                }
            }
        }
    }
}

#[validator]
impl Analyzer<'_, '_> {
    fn validate(&mut self, node: &RForStmt) {
        node.init.visit_with(self);

        let test = try_opt!(node.test.validate_with_default(self));
        let always_true = Type::Lit(LitType {
            span: node.span,
            lit: RTsLit::Bool(RBool {
                span: DUMMY_SP,
                value: true,
            }),
            metadata: Default::default(),
        });
        self.check_for_inifinite_loop(test.as_ref().unwrap_or(&always_true), &node.body);

        node.update.visit_with(self);
        node.body.validate_with(self)?;

        Ok(())
    }
}

/// NOTE: We does **not** dig into with statements.
#[validator]
impl Analyzer<'_, '_> {
    fn validate(&mut self, s: &RWithStmt) {
        self.storage.report(Error::WithStmtNotSupported { span: s.span });

        s.obj.visit_with(self);

        Ok(())
    }
}

#[validator]
impl Analyzer<'_, '_> {
    fn validate(&mut self, s: &RBlockStmt) {
        self.with_child(ScopeKind::Block, Default::default(), |analyzer| {
            s.stmts.visit_with(analyzer);
            Ok(())
        })?;

        Ok(())
    }
}

impl Analyzer<'_, '_> {
    /// Validate that parent interfaces are all resolved.
    #[instrument(skip(self, parents))]
    pub(super) fn resolve_parent_interfaces(&mut self, parents: &[RTsExprWithTypeArgs]) {
        if self.is_builtin {
            return;
        }

        for parent in parents {
            // Verify parent interface
            let res: Result<_, _> = try {
                let type_args = try_opt!(parent.type_args.validate_with(self));
                let span = parent.span;

                self.report_error_for_unresolve_type(span, &parent.expr, type_args.as_ref())?;
            };

            res.report(&mut self.storage);
        }
    }
}
