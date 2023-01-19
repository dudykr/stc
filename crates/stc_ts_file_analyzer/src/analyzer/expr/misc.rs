use stc_ts_ast_rnode::{RTsInstantiation, RTsSatisfiesExpr};
use stc_ts_errors::DebugExt;
use stc_ts_file_analyzer_macros::validator;
use stc_ts_types::{Type, TypeParamInstantiation};
use stc_utils::cache::Freeze;

use crate::{
    analyzer::{expr::TypeOfMode, Analyzer},
    validator::ValidateWith,
    VResult,
};

#[validator]
impl Analyzer<'_, '_> {
    fn validate(
        &mut self,
        e: &RTsSatisfiesExpr,
        mode: TypeOfMode,
        type_args: Option<&TypeParamInstantiation>,
        type_ann: Option<&Type>,
    ) -> VResult<Type> {
        let type_ann = e.type_ann.validate_with(self)?.freezed();

        let ty = e
            .expr
            .validate_with_args(self, (mode, type_args, Some(&type_ann)))
            .context("tried to verify expr of ts satisfies expression")?;

        // TODO: verify

        Ok(ty)
    }
}

#[validator]
impl Analyzer<'_, '_> {
    fn validate(
        &mut self,
        e: &RTsInstantiation,
        mode: TypeOfMode,
        type_args: Option<&TypeParamInstantiation>,
        type_ann: Option<&Type>,
    ) -> VResult<Type> {
        let type_args = e.type_args.validate_with(self)?;

        let ty = e
            .expr
            .validate_with_args(self, (mode, Some(&type_args), type_ann))
            .context("tried to verify expr of ts instantiation expression")?;

        let new = self.expand_generics_with_type_args(e.span, ty, &type_args)?;

        Ok(new)
    }
}
