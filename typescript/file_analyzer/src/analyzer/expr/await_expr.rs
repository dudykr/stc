use crate::analyzer::Analyzer;
use crate::validator::ValidateWith;
use crate::ValidationResult;
use stc_ts_ast_rnode::RAwaitExpr;
use stc_ts_file_analyzer_macros::validator;

#[validator]
impl Analyzer<'_, '_> {
    fn validate(&mut self, e: &RAwaitExpr) -> ValidationResult {
        let arg_ty = e.arg.validate_with_default(self)?;
        dbg!(&arg_ty);

        Ok(arg_ty)
    }
}
