use super::TypeOfMode;
use crate::analyzer::Analyzer;
use crate::validator::ValidateWith;
use crate::ValidationResult;
use stc_ts_ast_rnode::{RAwaitExpr, RIdent, RTsEntityName};
use stc_ts_errors::DebugExt;
use stc_ts_file_analyzer_macros::validator;
use stc_ts_types::{IdCtx, Ref};
use stc_ts_types::{Key, ModuleId};
use stc_ts_types::{Type, TypeParamInstantiation};
use swc_common::SyntaxContext;

#[validator]
impl Analyzer<'_, '_> {
    fn validate(&mut self, e: &RAwaitExpr, type_ann: Option<&Type>) -> ValidationResult {
        let span = e.span;

        let arg_type_ann = type_ann.map(|item| {
            let spane = span.with_ctxt(SyntaxContext::empty());
            Type::Ref(Ref {
                ctxt: ModuleId::builtin(),
                span,
                type_name: RTsEntityName::Ident(RIdent::new("PromiseLike".into(), span)),
                type_args: Some(box TypeParamInstantiation {
                    span,
                    params: vec![item.clone()],
                }),
            })
        });

        self.with(|a: &mut Analyzer| -> ValidationResult<_> {
            let arg_ty = e
                .arg
                .validate_with_args(a, (TypeOfMode::RValue, None, arg_type_ann.as_ref()))
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
