use super::AssignOpts;
use crate::analyzer::Analyzer;
use crate::ValidationResult;
use stc_ts_ast_rnode::RTsLit;
use stc_ts_ast_rnode::RTsLitType;
use stc_ts_errors::Error;
use stc_ts_types::TplType;
use stc_ts_types::Type;

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
    pub(crate) fn assign_to_tpl(&mut self, l: &TplType, r: &Type, opts: AssignOpts) -> ValidationResult<()> {
        let span = opts.span;
        let r = r.normalize();

        match r {
            Type::Lit(RTsLitType {
                lit: RTsLit::Str(r), ..
            }) => {
                let mut start = 0;
                let mut positions = vec![];

                for item in &l.quasis {
                    let q = &item.cooked.as_ref().unwrap().value;
                    match r.value[start..].find(&**q) {
                        Some(pos) => {
                            positions.push(pos);
                            start += pos + 1;
                        }
                        None => return Err(Error::SimpleAssignFailed { span }),
                    }
                }

                Ok(())
            }

            _ => Err(Error::SimpleAssignFailed { span }),
        }
    }
}
