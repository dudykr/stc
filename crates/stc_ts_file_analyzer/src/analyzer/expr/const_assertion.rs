use stc_ts_ast_rnode::RTsConstAssertion;
use stc_ts_errors::{DebugExt, Error};
use stc_ts_file_analyzer_macros::validator;
use stc_ts_type_ops::{generalization::prevent_generalize, tuple_to_array::prevent_tuple_to_array};
use stc_ts_types::{Type, TypeParamInstantiation};

use crate::{
    analyzer::{expr::TypeOfMode, Analyzer, Ctx},
    validator::ValidateWith,
    VResult,
};

#[validator]
impl Analyzer<'_, '_> {
    fn validate(
        &mut self,
        expr: &RTsConstAssertion,
        mode: TypeOfMode,
        type_args: Option<&TypeParamInstantiation>,
        type_ann: Option<&Type>,
    ) -> VResult {
        let span = expr.span;

        if mode == TypeOfMode::RValue {
            let ctx = Ctx {
                in_const_assertion: true,
                ..self.ctx
            };
            let mut a = self.with_ctx(ctx);

            let mut ty = expr
                .expr
                .validate_with_args(&mut *a, (mode, None, type_ann))
                .context("tried to valid expression of a const assertion")?;

            prevent_generalize(&mut ty);
            prevent_tuple_to_array(&mut ty);

            Ok(ty)
        } else {
            return Err(Error::Unimplemented {
                span,
                msg: format!("Proper error reporting for using const assertion expression in left hand side of an assignment expression"),
            });
        }
    }
}
