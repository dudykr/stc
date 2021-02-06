use super::AssignOpts;
use crate::analyzer::Analyzer;
use crate::ValidationResult;
use stc_ts_errors::DebugExt;
use stc_ts_errors::Error;
use stc_ts_types::FnParam;
use stc_ts_types::Function;
use stc_ts_types::Type;

impl Analyzer<'_, '_> {
    /// ```ts
    /// class Base {}
    /// class Derived extends Base {
    ///
    /// }
    ///
    /// declare var a: (b: Base) => {};
    /// declare var b: (b: Derived) => { foo: string };
    ///
    /// a = b;
    /// b = a; // error
    /// ```
    pub(super) fn assign_to_function(&mut self, opts: AssignOpts, l: &Function, r: &Type) -> ValidationResult<()> {
        let span = opts.span;
        let r = r.normalize();

        match r {
            // var fnr2: () => any = fnReturn2();
            Type::Function(Function {
                params: r_params,
                ret_ty: right_ret_ty,
                ..
            }) => {
                self.assign_params(opts, &r_params, &l.params)
                    .context("tried to parameters of a function to parameters of another function")?;

                // TODO: Verify type parameters.
                self.assign_inner(&l.ret_ty, right_ret_ty, opts)
                    .context("tried to assign the return type of a function to the return type of another function")?;
                // TODO: Verify parameter counts

                return Ok(());
            }

            Type::Lit(..) => return Err(box Error::CannotAssignToNonVariable { span }),
            _ => {}
        }

        Ok(())
    }

    pub(crate) fn assign_params(&mut self, opts: AssignOpts, l: &[FnParam], r: &[FnParam]) -> ValidationResult<()> {
        let span = opts.span;

        if l.len() != r.len() {
            return Err(box Error::Unimplemented {
                span,
                msg: format!("l.params.len() = {}; r.params.len() = {};", l.len(), r.len()),
            });
        }

        for (lp, rp) in l.iter().zip(r.iter()) {
            self.assign_inner(&lp.ty, &rp.ty, opts)
                .context("tried to assign a method parameter to a method parameter")?;
        }

        Ok(())
    }
}
