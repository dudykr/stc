use crate::{analyzer::Analyzer, ValidationResult};
use stc_ts_ast_rnode::RMetaPropExpr;
use stc_ts_file_analyzer_macros::validator;

#[validator]
impl Analyzer<'_, '_> {
    fn validate(&mut self, e: &RMetaPropExpr) -> ValidationResult {
        match (&e.meta, &e.prop) {
            _ => {
                todo!("Unsupported meta property {:?}", e)
            }
        }
    }
}
