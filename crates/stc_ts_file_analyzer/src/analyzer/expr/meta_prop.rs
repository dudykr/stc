use stc_ts_ast_rnode::RMetaPropExpr;
use stc_ts_errors::ErrorKind;
use stc_ts_file_analyzer_macros::validator;
use stc_ts_types::Type;
use swc_common::Spanned;
use swc_ecma_ast::MetaPropKind;

use crate::{analyzer::Analyzer, VResult};

#[validator]
impl Analyzer<'_, '_> {
    fn validate(&mut self, e: &RMetaPropExpr) -> VResult<Type> {
        match e.kind {
            MetaPropKind::NewTarget => {
                if !self.ctx.allow_new_target {
                    self.storage.report(ErrorKind::InvalidUsageOfNewTarget { span: e.span() }.into())
                }

                Ok(Type::any(e.span, Default::default()))
            }

            MetaPropKind::ImportMeta => {
                if let Ok(mut ty) = self.env.get_global_type(e.span(), &"ImportMeta".into()) {
                    ty.reposition(e.span());
                    return Ok(ty);
                }

                todo!("Unsupported meta property {:?}", e)
            }
        }
    }
}
