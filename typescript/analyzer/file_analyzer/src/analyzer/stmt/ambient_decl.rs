use crate::analyzer::Analyzer;
use rnode::VisitWith;
use stc_ts_ast_rnode::RModuleItem;
use stc_ts_simple_ast_validations::ambient_fn::AmbientFunctionHandler;

impl Analyzer<'_, '_> {
    pub(crate) fn report_error_for_wrong_top_level_ambient_fns(&mut self, nodes: &[RModuleItem]) {
        if self.is_builtin {
            return;
        }

        let mut visitor = AmbientFunctionHandler {
            last_ambient_name: None,
            errors: &mut self.storage,
        };

        nodes.visit_with(&mut visitor);

        visitor.handle_missing_impl();
    }
}
