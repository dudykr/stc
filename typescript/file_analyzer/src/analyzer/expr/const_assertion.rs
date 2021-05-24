use super::TypeOfMode;
use crate::{analyzer::Analyzer, validator::ValidateWith, ValidationResult};
use stc_ts_ast_rnode::RTsConstAssertion;
use stc_ts_errors::Error;
use stc_ts_file_analyzer_macros::validator;
use stc_ts_types::{Type, TypeParamInstantiation};

#[validator]
impl Analyzer<'_, '_> {
    fn validate(
        &mut self,
        expr: &RTsConstAssertion,
        mode: TypeOfMode,
        type_args: Option<&TypeParamInstantiation>,
        type_ann: Option<&Type>,
    ) -> ValidationResult {
        let span = expr.span;

        if mode == TypeOfMode::RValue {
            return expr.expr.validate_with_args(self, (mode, None, type_ann));
        } else {
            return Err(Error::Unimplemented {
                span,
                msg: format!(
                    "Proper error reporting for using const assertion expression in left hand side of an assignment \
                     expression"
                ),
            });
        }
    }
}
