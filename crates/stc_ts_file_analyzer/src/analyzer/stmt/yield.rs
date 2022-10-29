use rnode::VisitWith;
use stc_ts_ast_rnode::RModuleItem;
use stc_ts_simple_ast_validations::yield_check::YieldCheck;

use crate::analyzer::Analyzer;

impl Analyzer<'_, '_> {
    pub(crate) fn report_error_for_wrong_yield(&mut self, nodes: &[RModuleItem]) {
        if self.is_builtin {
            return;
        }

        let mut visitor = YieldCheck {
            in_generator: self.ctx.in_generator,
            errors: &mut self.storage,
        };

        nodes.visit_with(&mut visitor);
    }
}
