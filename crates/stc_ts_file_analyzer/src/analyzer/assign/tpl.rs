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
                // TOOD(kdy1): We should iterator over two types, and check if each element is
                // assignable.

                let mut li = 0;
                let mut ri = 0;

                while li <= l.quasis.len() + l.types.len() && ri <= r.quasis.len() + r.types.len() {
                    // 0: quasi, 1: type
                    match (li % 2, ri % 2) {
                        (0, 0) => {
                            //
                            if l.quasis[li % 2].cooked != r.quasis[ri % 2].cooked {
                                return Err(
                                    Error::SimpleAssignFailed { span, cause: None }.context("failed to assign a literal to literal")
                                );
                            }
                        }
                        (0, 1) => {
                            return Err(Error::SimpleAssignFailed { span, cause: None }.context("cannot assign expression to literal"));
                        }
                        (1, 0) => {}
                        (1, 1) => {}
                        _ => {
                            unreachable!()
                        }
                    }

                    // Bump
                    li += 1;
                    ri += 1;
                }

                Ok(())
            }
            Type::Lit(LitType { lit: RTsLit::Str(r), .. }) => {
                let mut start = 0;
                let mut positions = vec![];

                for item in &l.quasis {
                    let q = &item.cooked.as_ref().unwrap();
                    match r.value[start..].find(&***q) {
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
