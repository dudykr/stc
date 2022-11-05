use stc_ts_ast_rnode::RTsLit;
use stc_ts_errors::Error;
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
    pub(crate) fn assign_to_tpl(&mut self, data: &mut AssignData, l: &TplType, r: &Type, opts: AssignOpts) -> VResult<()> {
        let span = opts.span;
        let r = r.normalize();

        match r {
            Type::Tpl(r) => {
                if r.quasis.len() != l.quasis.len() {
                    return Err(Error::SimpleAssignFailed { span, cause: None });
                }

                for index in 0..r.quasis.len() {
                    if r.quasis[index].raw != l.quasis[index].raw {
                        return Err(Error::SimpleAssignFailed { span, cause: None });
                    }
                }

                if r.types.len() != l.types.len() {
                    return Err(Error::SimpleAssignFailed { span, cause: None });
                }

                for index in 0..r.types.len() {
                    self.assign_without_wrapping(data, &l.types[index], &r.types[index], opts)?;
                }

                Ok(())
            }
            Type::Lit(LitType { lit: RTsLit::Str(r), .. }) => {
                let mut start = 0;
                let mut positions = vec![];

                for item in &l.quasis {
                    let q = &item.cooked.as_ref().unwrap().value;
                    match r.value[start..].find(&**q) {
                        Some(pos) => {
                            positions.push(pos);
                            start += pos + 1;
                        }
                        None => return Err(Error::SimpleAssignFailed { span, cause: None }),
                    }
                }

                Ok(())
            }

            _ => Err(Error::SimpleAssignFailed { span, cause: None }),
        }
    }
}
