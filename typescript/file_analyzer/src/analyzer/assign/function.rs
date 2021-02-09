use super::AssignOpts;
use crate::analyzer::Analyzer;
use crate::ValidationResult;
use fxhash::FxHashMap;
use stc_ts_ast_rnode::RIdent;
use stc_ts_ast_rnode::RPat;
use stc_ts_errors::DebugExt;
use stc_ts_errors::Error;
use stc_ts_types::FnParam;
use stc_ts_types::Function;
use stc_ts_types::Type;
use swc_atoms::js_word;

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
    pub(super) fn assign_to_function(
        &mut self,
        opts: AssignOpts,
        lt: &Type,
        l: &Function,
        r: &Type,
    ) -> ValidationResult<()> {
        let span = opts.span;
        let r = r.normalize();

        match r {
            // var fnr2: () => any = fnReturn2();
            Type::Function(Function {
                type_params: r_type_params,
                params: r_params,
                ret_ty: r_ret_ty,
                ..
            }) => {
                let new_r;
                let (r_params, r_ret_ty) = match (&l.type_params, r_type_params) {
                    (Some(lt), Some(rt)) => {
                        //
                        let map = lt
                            .params
                            .iter()
                            .zip(rt.params.iter())
                            .map(|(l, r)| (r.name.clone(), Type::Param(l.clone()).cheap()))
                            .collect::<FxHashMap<_, _>>();
                        let r = self
                            .expand_type_params(&map, box r.clone())
                            .context("tried to expand type parameters as a step of function assignemnt")?;
                        new_r = r.function().unwrap();
                        (&new_r.params, &new_r.ret_ty)
                    }

                    // Assigning `(a: 1) => string` to `<Z>(a: Z) => string` is valid.
                    (None, Some(rt)) => {
                        let map = self.infer_type_with_types(span, &*rt.params, r, lt)?;
                        let r = self
                            .expand_type_params(&map, box r.clone())
                            .context("tried to expand type parameters of rhs as a step of function assignemnt")?;
                        new_r = r.function().unwrap();
                        (&new_r.params, &new_r.ret_ty)
                    }

                    _ => (r_params, r_ret_ty),
                };

                // () => void
                //
                // is assignable to
                //
                // (t: unknown, t1: unknown) => void
                //
                // So we check for length first.
                if r_params.len() != 0 {
                    self.assign_params(opts, &l.params, &r_params)
                        .context("tried to assign parameters of a function to parameters of another function")?;
                }

                // TODO: Verify type parameters.
                self.assign_inner(&l.ret_ty, r_ret_ty, opts)
                    .context("tried to assign the return type of a function to the return type of another function")?;

                return Ok(());
            }

            Type::Lit(..) => return Err(box Error::CannotAssignToNonVariable { span }),
            _ => {}
        }

        Ok(())
    }

    pub(crate) fn assign_params(&mut self, opts: AssignOpts, l: &[FnParam], r: &[FnParam]) -> ValidationResult<()> {
        let span = opts.span;
        let li = l.iter().filter(|p| match p.pat {
            RPat::Ident(RIdent {
                sym: js_word!("this"), ..
            }) => false,
            _ => true,
        });
        let ri = r.iter().filter(|p| match p.pat {
            RPat::Ident(RIdent {
                sym: js_word!("this"), ..
            }) => false,
            _ => true,
        });

        // TODO: Consider optional parameters.
        if li.clone().count() < ri.clone().count() {
            return Err(box Error::SimpleAssignFailed { span });
        }

        for (lp, rp) in li.zip(ri) {
            self.assign_inner(
                &lp.ty,
                &rp.ty,
                AssignOpts {
                    allow_unknown_type: true,
                    ..opts
                },
            )
            .context("tried to assign a method parameter to a method parameter")?;
        }

        Ok(())
    }
}
