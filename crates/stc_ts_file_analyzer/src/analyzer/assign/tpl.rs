use stc_ts_ast_rnode::RTsLit;
use stc_ts_errors::{DebugExt, Error};
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

                while li < l.quasis.len() + l.types.len() && ri < r.quasis.len() + r.types.len() {
                    // 0: quasi, 1: type

                    if li % 2 == 0 {
                        // LHS is literal
                        if ri % 2 == 0 {
                            // RHS is literal
                            if l.quasis[li / 2].cooked != r.quasis[ri / 2].cooked {
                                return Err(
                                    Error::SimpleAssignFailed { span, cause: None }.context("failed to assign a literal to literal")
                                );
                            }
                        } else {
                            // RHS is type
                            return Err(Error::SimpleAssignFailed { span, cause: None }.context("cannot assign expression to literal"));
                        }
                    } else {
                        // LHS is type

                        // We should eat as much text as possible.

                        if ri % 2 == 0 {
                            // RHS is literal
                        } else {
                            // RHS is type

                            let l = &l.types[li / 2];
                            let r = &r.types[ri / 2];

                            self.assign_inner(data, l, r, opts)
                                .context("tried to assign a type to a type for a template literal")?;
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
