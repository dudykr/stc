use super::TypeOfMode;
use crate::analyzer::Analyzer;
use crate::validator::ValidateWith;
use crate::ValidationResult;
use stc_ts_ast_rnode::RAwaitExpr;
use stc_ts_errors::DebugExt;
use stc_ts_file_analyzer_macros::validator;
use stc_ts_types::IdCtx;
use stc_ts_types::Key;
use stc_ts_types::Type;

#[validator]
impl Analyzer<'_, '_> {
    fn validate(&mut self, e: &RAwaitExpr) -> ValidationResult {
        let span = e.span;
        self.with(|a: &mut Analyzer| -> ValidationResult<_> {
            let arg_ty = e
                .arg
                .validate_with_default(a)
                .context("tried to validate the argument of an await expr")?;

            let then_ty = match a.access_property(
                span,
                &arg_ty,
                &Key::Normal {
                    span,
                    sym: "then".into(),
                },
                TypeOfMode::RValue,
                IdCtx::Var,
            ) {
                Ok(v) => v,
                Err(..) => {
                    // If `then` does not exists, the type itself is used
                    return Ok(arg_ty);
                }
            };

            match then_ty.normalize() {
                Type::Function(f) => {
                    // Default type of the first type parameter is awaited type.
                    if let Some(type_params) = &f.type_params {
                        if let Some(ty) = type_params.params.first() {
                            if let Some(ty) = &ty.default {
                                return Ok(*ty.clone());
                            }
                        }
                    }
                }
                _ => {}
            }

            Ok(arg_ty)
        })
        .map(|mut ty| {
            ty.reposition(e.span);
            ty
        })
    }
}
