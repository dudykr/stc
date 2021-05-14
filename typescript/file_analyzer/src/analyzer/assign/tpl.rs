use super::AssignOpts;
use crate::analyzer::Analyzer;
use crate::ValidationResult;
use stc_ts_ast_rnode::RTsLit;
use stc_ts_ast_rnode::RTsLitType;
use stc_ts_errors::Error;
use stc_ts_types::TplType;
use stc_ts_types::Type;

impl Analyzer<'_, '_> {
    pub(crate) fn assign_to_tpl(&mut self, l: &TplType, r: &Type, opts: AssignOpts) -> ValidationResult<()> {
        let span = opts.span;
        let r = r.normalize();

        match r {
            Type::Lit(RTsLitType {
                lit: RTsLit::Str(r), ..
            }) => {
                //

                Ok(())
            }

            _ => Err(Error::SimpleAssignFailed { span }),
        }
    }
}
