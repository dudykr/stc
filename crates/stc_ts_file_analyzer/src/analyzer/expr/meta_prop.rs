use stc_ts_ast_rnode::{RIdent, RMetaPropExpr};
use stc_ts_errors::Error;
use stc_ts_file_analyzer_macros::validator;
use stc_ts_types::Type;
use swc_atoms::js_word;
use swc_common::Spanned;

use crate::{analyzer::Analyzer, VResult};

#[validator]
impl Analyzer<'_, '_> {
    fn validate(&mut self, e: &RMetaPropExpr) -> VResult<Type> {
        match (&e.meta, &e.prop) {
            (
                RIdent {
                    sym: js_word!("new"),
                    ..
                },
                RIdent {
                    sym: js_word!("target"),
                    ..
                },
            ) => {
                if !self.ctx.allow_new_target {
                    self.storage
                        .report(Error::InvalidUsageOfNewTarget { span: e.span() })
                }

                return Ok(Type::any(e.span, Default::default()));
            }

            _ => {
                todo!("Unsupported meta property {:?}", e)
            }
        }
    }
}
