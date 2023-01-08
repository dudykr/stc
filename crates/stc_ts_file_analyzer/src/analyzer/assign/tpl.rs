#![allow(clippy::if_same_then_else)]

use stc_ts_ast_rnode::RTsLit;
use stc_ts_errors::{ctx, debug::dump_type_as_string, ErrorKind};
use stc_ts_types::{LitType, TplType, Type};

use crate::{
    analyzer::{
        assign::{AssignData, AssignOpts},
        Analyzer,
    },
    VResult,
};

impl Analyzer<'_, '_> {
    /// # Implementation notes
    ///
    /// We split string based on the literals.
    ///
    ///
    /// `:${string}:::${string}:` = ":1:sss:s:s:s:s::s:s:"
    ///
    /// For the code above, we try to find `:`, `:::`, `:`, while preserving
    /// orders.
    ///
    /// After splitting, we can check if each element is assignable.
    pub(crate) fn assign_to_tpl(&mut self, data: &mut AssignData, l: &TplType, r_ty: &Type, opts: AssignOpts) -> VResult<()> {
        let span = opts.span;
        let r_ty = r_ty.normalize();

        let types = self.infer_types_from_tpl_lit_type(span, r_ty, l)?;

        let types = match types {
            Some(types) => types,
            None => return Err(ErrorKind::SimpleAssignFailed { span, cause: None }.context("tried to infer")),
        };

        for (i, ty) in types.iter().enumerate() {
            if !self.is_valid_type_for_tpl_lit_placeholder(ty, &l.types[i])? {
                return Err(ErrorKind::SimpleAssignFailed { span, cause: None }.context("verified types"));
            }
        }

        Ok(())
    }

    /// Ported from `isValidTypeForTemplateLiteralPlaceholder` of `tsc`
    pub(crate) fn is_valid_type_for_tpl_lit_placeholder(&mut self, source: &Type, target: &Type) -> VResult<bool> {}
}
