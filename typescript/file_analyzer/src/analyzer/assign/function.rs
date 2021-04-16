use super::AssignOpts;
use crate::analyzer::Analyzer;
use crate::ValidationResult;
use fxhash::FxHashMap;
use stc_ts_ast_rnode::RBindingIdent;
use stc_ts_ast_rnode::RIdent;
use stc_ts_ast_rnode::RPat;
use stc_ts_errors::DebugExt;
use stc_ts_errors::Error;
use stc_ts_types::ClassDef;
use stc_ts_types::Constructor;
use stc_ts_types::FnParam;
use stc_ts_types::Function;
use stc_ts_types::Type;
use stc_ts_types::TypeElement;
use std::borrow::Cow;
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
                            .expand_type_params(&map, r.clone())
                            .context("tried to expand type parameters as a step of function assignemnt")?;
                        new_r = r.function().unwrap();
                        (&new_r.params, &new_r.ret_ty)
                    }

                    // Assigning `(a: 1) => string` to `<Z>(a: Z) => string` is valid.
                    (None, Some(rt)) => {
                        let map = self.infer_type_with_types(span, &*rt.params, r, lt)?;
                        let r = self
                            .expand_type_params(&map, r.clone())
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
                    self.assign_params(opts, &l.params, &r_params, true)
                        .context("tried to assign parameters of a function to parameters of another function")?;
                }

                // TODO: Verify type parameters.
                self.assign_inner(&l.ret_ty, r_ret_ty, opts)
                    .context("tried to assign the return type of a function to the return type of another function")?;

                return Ok(());
            }

            Type::TypeLit(rt) => {
                for rm in &rt.members {
                    match rm {
                        TypeElement::Call(rm) => {
                            if self.assign_params(opts, &l.params, &rm.params, true).is_err() {
                                continue;
                            }

                            if let Some(r_ret_ty) = &rm.ret_ty {
                                if self.assign_with_opts(opts, &l.ret_ty, &r_ret_ty).is_err() {
                                    continue;
                                }
                            }

                            return Ok(());
                        }
                        _ => {}
                    }
                }
            }

            Type::Interface(..) => {
                let ty = self.type_to_type_lit(span, r)?.map(Cow::into_owned).map(Type::TypeLit);
                if let Some(ty) = ty {
                    return self
                        .assign_to_function(opts, lt, l, &ty)
                        .context("tried to assign an expanded type to a function");
                }
            }
            _ => {}
        }

        Err(Error::SimpleAssignFailed { span })
    }

    pub(super) fn assign_to_constructor(
        &mut self,
        opts: AssignOpts,
        lt: &Type,
        l: &Constructor,
        r: &Type,
    ) -> ValidationResult<()> {
        let span = opts.span;
        let r = r.normalize();

        match r {
            Type::Constructor(rc) => {
                let new_r;
                let (r_params, r_type_ann) = match (&l.type_params, &rc.type_params) {
                    (Some(lt), Some(rt)) => {
                        //
                        let map = lt
                            .params
                            .iter()
                            .zip(rt.params.iter())
                            .map(|(l, r)| (r.name.clone(), Type::Param(l.clone()).cheap()))
                            .collect::<FxHashMap<_, _>>();
                        let r = self
                            .expand_type_params(&map, r.clone())
                            .context("tried to expand type parameters as a step of constructor assignemnt")?;
                        new_r = r.constructor().unwrap();
                        (&new_r.params, &new_r.type_ann)
                    }

                    // Assigning `(a: 1) => string` to `<Z>(a: Z) => string` is valid.
                    (None, Some(rt)) => {
                        let map = self.infer_type_with_types(span, &*rt.params, r, lt)?;
                        let r = self
                            .expand_type_params(&map, r.clone())
                            .context("tried to expand type parameters of rhs as a step of constructor assignemnt")?;
                        new_r = r.constructor().unwrap();
                        (&new_r.params, &new_r.type_ann)
                    }

                    _ => (&rc.params, &rc.type_ann),
                };

                self.assign_params(opts, &l.params, &r_params, true).context(
                    "tried to assign the parameters of constructor to the parameters of another constructor",
                )?;

                self.assign_with_opts(opts, &l.type_ann, &r_type_ann).context(
                    "tried to assign the return type of constructor to the return type of another constructor",
                )?;

                return Ok(());
            }
            Type::Lit(..) | Type::ClassDef(ClassDef { is_abstract: true, .. }) | Type::Function(..) => {
                return Err(Error::SimpleAssignFailed { span })
            }

            Type::TypeLit(rt) => {
                let mut errors = vec![];
                for (idx, rm) in rt.members.iter().enumerate() {
                    match rm {
                        TypeElement::Constructor(rc) => {
                            if let Err(err) =
                                self.assign_params(opts, &l.params, &rc.params, false).with_context(|| {
                                    format!(
                                        "tried to assign parameters of a constructor to them of another constructor \
                                         ({}th element)",
                                        idx
                                    )
                                })
                            {
                                errors.push(err);
                                continue;
                            }

                            if let Some(r_ret_ty) = &rc.ret_ty {
                                if let Err(err) =
                                    self.assign_with_opts(opts, &l.type_ann, &r_ret_ty).with_context(|| {
                                        format!(
                                            "tried to  assign the return type of a constructor to it of another \
                                             constructor ({}th element)",
                                            idx,
                                        )
                                    })
                                {
                                    errors.push(err);
                                    continue;
                                }
                            }

                            return Ok(());
                        }
                        _ => {}
                    }
                }
                if !errors.is_empty() {
                    return Err(Error::SimpleAssignFailedWithCause { span, cause: errors });
                }
            }
            Type::Interface(..) => {
                let ty = self.type_to_type_lit(span, r)?.map(Cow::into_owned).map(Type::TypeLit);
                if let Some(ty) = ty {
                    return self
                        .assign_to_constructor(opts, lt, l, &ty)
                        .context("tried to assign an expanded type to a constructor type");
                }
            }
            _ => {}
        }

        Err(Error::SimpleAssignFailed { span })
    }

    /// ``ts
    /// declare let a: (parent: 'foo' | 'bar') => void
    /// declare let b: (parent: 'bar') => void
    ///
    /// a = b // error
    /// b = a // ok
    /// ```
    pub(crate) fn assign_params(
        &mut self,
        opts: AssignOpts,
        l: &[FnParam],
        r: &[FnParam],
        reverse: bool,
    ) -> ValidationResult<()> {
        let span = opts.span;
        let li = l.iter().filter(|p| match p.pat {
            RPat::Ident(RBindingIdent {
                id: RIdent {
                    sym: js_word!("this"), ..
                },
                ..
            }) => false,
            _ => true,
        });
        let ri = r.iter().filter(|p| match p.pat {
            RPat::Ident(RBindingIdent {
                id: RIdent {
                    sym: js_word!("this"), ..
                },
                ..
            }) => false,
            _ => true,
        });

        // TODO: Consider optional parameters.
        if li.clone().count() < ri.clone().count() {
            return Err(Error::SimpleAssignFailed { span });
        }

        for (lp, rp) in li.zip(ri) {
            // TODO: What should we do?
            if opts.allow_assignment_to_param {
                if let Ok(()) = self.assign_inner(
                    &rp.ty,
                    &lp.ty,
                    AssignOpts {
                        allow_unknown_type: true,
                        ..opts
                    },
                ) {
                    continue;
                }
            }

            if reverse {
                self.assign_inner(
                    &rp.ty,
                    &lp.ty,
                    AssignOpts {
                        allow_unknown_type: true,
                        ..opts
                    },
                )
            } else {
                self.assign_inner(
                    &lp.ty,
                    &rp.ty,
                    AssignOpts {
                        allow_unknown_type: true,
                        ..opts
                    },
                )
            }
            .context("tried to assign a method parameter to a method parameter")?;
        }

        Ok(())
    }
}
