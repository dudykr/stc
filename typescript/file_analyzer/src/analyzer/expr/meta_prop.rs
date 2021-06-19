use crate::{analyzer::Analyzer, ValidationResult};
use stc_ts_ast_rnode::{RIdent, RMetaPropExpr};
use stc_ts_file_analyzer_macros::validator;
use stc_ts_types::Type;
use swc_atoms::js_word;

#[validator]
impl Analyzer<'_, '_> {
    fn validate(&mut self, e: &RMetaPropExpr) -> ValidationResult {
        match (&e.meta, &e.prop) {
            (
                RIdent {
                    sym: js_word!("new"), ..
                },
                RIdent {
                    sym: js_word!("target"),
                    ..
                },
            ) => return Ok(Type::any(e.meta.span)),

            _ => {
                todo!("Unsupported meta property {:?}", e)
            }
        }
    }
}
