use super::AssignData;
use super::AssignOpts;
use crate::analyzer::Analyzer;
use crate::ValidationResult;
use fxhash::FxHashMap;
use itertools::EitherOrBoth;
use itertools::Itertools;
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
use stc_ts_types::TypeParamDecl;
use std::borrow::Cow;
use swc_atoms::js_word;

impl Analyzer<'_, '_> {
    pub(super) fn assign_to_fn_like(
        &mut self,
        data: &mut AssignData,
        opts: AssignOpts,
        l_type_params: Option<&TypeParamDecl>,
        l_params: &[FnParam],
        l_ret_ty: Option<&Type>,
        r_type_params: Option<&TypeParamDecl>,
        r_params: &[FnParam],
        r_ret_ty: Option<&Type>,
    ) -> ValidationResult<()> {
        let span = opts.span;

        let new_r_params;
        let new_r_ret_ty;
        let (r_params, r_ret_ty) = match (&l_type_params, r_type_params) {
            (Some(lt), Some(rt)) => {
                //
                let map = lt
                    .params
                    .iter()
                    .zip(rt.params.iter())
                    .map(|(l, r)| (r.name.clone(), Type::Param(l.clone()).cheap()))
                    .collect::<FxHashMap<_, _>>();
                new_r_params = self
                    .expand_type_params(&map, r_params.to_vec())
                    .context("tried to expand type parameters as a step of function assignemnt")?;
                new_r_ret_ty = self
                    .expand_type_params(&map, r_ret_ty.cloned())
                    .context("tried to expand return type of rhs as a step of function assignemnt")?;
                (&*new_r_params, new_r_ret_ty.as_ref())
            }

            // Assigning `(a: 1) => string` to `<Z>(a: Z) => string` is valid.
            (None, Some(rt)) => {
                let lf = Type::Function(Function {
                    span,
                    type_params: None,
                    params: l_params.to_vec(),
                    ret_ty: box l_ret_ty.cloned().unwrap_or_else(|| Type::any(span)),
                });
                let rf = Type::Function(Function {
                    span,
                    type_params: None,
                    params: r_params.to_vec(),
                    ret_ty: box r_ret_ty.cloned().unwrap_or_else(|| Type::any(span)),
                });

                let map = self.infer_type_with_types(span, &*rt.params, &rf, &lf)?;
                new_r_params = self
                    .expand_type_params(&map, r_params.to_vec())
                    .context("tried to expand type parameters of rhs as a step of function assignemnt")?;
                new_r_ret_ty = self
                    .expand_type_params(&map, r_ret_ty.cloned())
                    .context("tried to expand return type of rhs as a step of function assignemnt")?;
                (&*new_r_params, new_r_ret_ty.as_ref())
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
            self.assign_params(data, opts, l_params, &r_params)
                .context("tried to assign parameters of a function to parameters of another function")?;
        }

        if let Some(l_ret_ty) = l_ret_ty {
            if let Some(r_ret_ty) = r_ret_ty {
                // TODO: Verify type parameters.
                self.assign_inner(
                    data,
                    l_ret_ty,
                    r_ret_ty,
                    AssignOpts {
                        allow_assignment_of_void: true,
                        allow_assignment_to_void: true,
                        ..opts
                    },
                )
                .context("tried to assign the return type of a function to the return type of another function")?;
            }
        }

        return Ok(());
    }

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
        data: &mut AssignData,
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
                self.assign_to_fn_like(
                    data,
                    opts,
                    l.type_params.as_ref(),
                    &l.params,
                    Some(&l.ret_ty),
                    r_type_params.as_ref(),
                    r_params,
                    Some(r_ret_ty),
                )
                .context("tried to assign to a function type")?;

                return Ok(());
            }

            Type::TypeLit(rt) => {
                for rm in &rt.members {
                    match rm {
                        TypeElement::Call(rm) => {
                            if self.assign_params(data, opts, &l.params, &rm.params).is_err() {
                                continue;
                            }

                            if let Some(r_ret_ty) = &rm.ret_ty {
                                if self.assign_with_opts(data, opts, &l.ret_ty, &r_ret_ty).is_err() {
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
                        .assign_to_function(data, opts, lt, l, &ty)
                        .context("tried to assign an expanded type to a function");
                }
            }
            _ => {}
        }

        Err(Error::SimpleAssignFailed { span })
    }

    pub(super) fn assign_to_constructor(
        &mut self,
        data: &mut AssignData,
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

                self.assign_params(data, opts, &l.params, &r_params).context(
                    "tried to assign the parameters of constructor to the parameters of another constructor",
                )?;

                self.assign_with_opts(data, opts, &l.type_ann, &r_type_ann).context(
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
                            if let Err(err) = self.assign_params(data, opts, &l.params, &rc.params).with_context(|| {
                                format!(
                                    "tried to assign parameters of a constructor to them of another constructor ({}th \
                                     element)",
                                    idx
                                )
                            }) {
                                errors.push(err);
                                continue;
                            }

                            if let Some(r_ret_ty) = &rc.ret_ty {
                                if let Err(err) = self
                                    .assign_with_opts(data, opts, &l.type_ann, &r_ret_ty)
                                    .with_context(|| {
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
                        .assign_to_constructor(data, opts, lt, l, &ty)
                        .context("tried to assign an expanded type to a constructor type");
                }
            }
            _ => {}
        }

        Err(Error::SimpleAssignFailed { span })
    }

    /// Assigns a parameter to another one.
    /// It may assign in reverse direction because of the rule 1.
    /// At the same time, it should not be reversed in some cases. (See rule 2)
    ///
    /// ## Rule 1
    ///
    /// ```ts
    /// declare let a: (parent: 'foo' | 'bar') => void
    /// declare let b: (parent: 'bar') => void
    ///
    /// a = b // error
    /// b = a // ok
    /// ```
    ///
    /// Valid assignment is `foo | bar` = `bar`, which is `a.param[0] =
    /// b.param[0]`, but it doesn't match `b = a`.
    ///
    /// ## Rule 2
    ///
    /// ```ts
    /// class Base {
    ///     private foo!: string
    /// }
    /// class Derived extends Base {
    ///     private bar!: string
    /// }
    ///
    /// declare var a: (y: Derived) => any;
    /// declare var b: (y: Base) => any
    ///
    /// a = b // ok
    /// b = a // error
    /// ```
    ///
    /// Valid assignment is `Derived = Base`, which is `a.params[0] =
    /// b.param[0]` and it matches `a = b`.
    ///
    /// # Notes
    ///
    ///  - `string` is assignable to `...args: any[]`.
    fn assign_param(
        &mut self,
        data: &mut AssignData,
        l: &FnParam,
        r: &FnParam,
        opts: AssignOpts,
    ) -> ValidationResult<()> {
        debug_assert!(
            !opts.span.is_dummy(),
            "Cannot assign function parameters with dummy span"
        );

        match l.pat {
            RPat::Rest(..) => match l.ty.normalize() {
                Type::Array(l_arr) => {
                    if let Ok(()) = self.assign_with_opts(data, opts, &l_arr.elem_type, &r.ty) {
                        return Ok(());
                    }
                }
                _ => {}
            },
            _ => {}
        }

        // TODO: Change this to extends call.

        let l_ty = self.normalize(Some(opts.span), Cow::Borrowed(&l.ty), Default::default())?;
        let r_ty = self.normalize(Some(opts.span), Cow::Borrowed(&r.ty), Default::default())?;

        l_ty.assert_valid();
        r_ty.assert_valid();

        let reverse = match (l_ty.normalize(), r_ty.normalize()) {
            (Type::Union(..), Type::Union(..)) => false,
            (_, Type::Union(..)) => true,
            _ => false,
        };

        if reverse {
            self.assign_with_opts(data, opts, &r.ty, &l.ty)
                .context("tried to assign the type of a parameter to another (reversed)")?;
        } else {
            self.assign_with_opts(data, opts, &l.ty, &r.ty)
                .context("tried to assign the type of a parameter to another")?;
        }

        Ok(())
    }

    /// # Validation of parameter count
    ///
    /// A parameter named `this` is excluded.
    ///
    ///
    /// ## Rule
    ///
    /// ```ts
    /// declare var a: (x: string) => any;
    /// declare var b: (x: string, y: number) => any
    ///
    /// a = b // error
    /// b = a // ok
    /// ```
    ///
    /// So, it's an error if `l.params.len() < r.params.len()`.
    pub(crate) fn assign_params(
        &mut self,
        data: &mut AssignData,
        opts: AssignOpts,
        l: &[FnParam],
        r: &[FnParam],
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

        let l_has_rest = l.iter().any(|p| match p.pat {
            RPat::Rest(..) => true,
            _ => false,
        });

        // TODO: Consider optional parameters.

        let required_li = li.clone().filter(|i| i.required);
        let required_ri = ri.clone().filter(|i| i.required);

        if opts.for_overload {
            if required_li.clone().count() > required_ri.clone().count() {
                return Err(Error::SimpleAssignFailed { span })
                    .context("l.params.required.len > r.params.required.len");
            }
        }

        // Don't ask why.
        if li.clone().count() < required_ri.clone().count() {
            if !l_has_rest && required_li.clone().count() < required_ri.clone().count() {
                // I don't know why, but overload signature does not need to match overloaded
                // signature.
                if opts.for_overload {
                    return Ok(());
                }

                return Err(Error::SimpleAssignFailed { span }
                    .context("!l_has_rest && l.params.required.len < r.params.required.len"));
            }
        }

        for pair in li.zip_longest(ri) {
            match pair {
                EitherOrBoth::Both(lp, rp) => {
                    // TODO: What should we do?
                    if opts.allow_assignment_to_param {
                        if let Ok(()) = self.assign_param(
                            data,
                            &rp,
                            &lp,
                            AssignOpts {
                                allow_unknown_type: true,
                                ..opts
                            },
                        ) {
                            continue;
                        }
                    }

                    self.assign_param(
                        data,
                        lp,
                        rp,
                        AssignOpts {
                            allow_unknown_type: true,
                            ..opts
                        },
                    )
                    .with_context(|| format!("tried to assign a method parameter to a method parameter",))?;
                }
                EitherOrBoth::Left(_) => {}
                EitherOrBoth::Right(_) => {}
            }
        }

        Ok(())
    }
}
