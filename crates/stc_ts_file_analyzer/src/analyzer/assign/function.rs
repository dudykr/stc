use std::borrow::Cow;

use fxhash::FxHashMap;
use itertools::{EitherOrBoth, Itertools};
use stc_ts_ast_rnode::{RBindingIdent, RIdent, RPat};
use stc_ts_errors::{
    debug::{dump_type_as_string, dump_type_map},
    DebugExt, Error,
};
use stc_ts_types::{ClassDef, Constructor, FnParam, Function, Type, TypeElement, TypeParamDecl};
use stc_utils::{cache::Freeze, debug_ctx};
use swc_atoms::js_word;
use swc_common::{Spanned, SyntaxContext, TypeEq};
use swc_ecma_ast::TsKeywordTypeKind;
use tracing::debug;

use crate::{
    analyzer::{
        assign::{AssignData, AssignOpts},
        generic::InferTypeOpts,
        Analyzer,
    },
    util::unwrap_ref_with_single_arg,
    VResult,
};

/// Methods to handle assignment to function types and constructor types.
impl Analyzer<'_, '_> {
    #[cfg_attr(debug_assertions, tracing::instrument(skip_all))]
    pub(crate) fn assign_to_fn_like(
        &mut self,
        data: &mut AssignData,
        opts: AssignOpts,
        is_call: bool,
        l_type_params: Option<&TypeParamDecl>,
        l_params: &[FnParam],
        l_ret_ty: Option<&Type>,
        r_type_params: Option<&TypeParamDecl>,
        r_params: &[FnParam],
        r_ret_ty: Option<&Type>,
    ) -> VResult<()> {
        let span = opts.span.with_ctxt(SyntaxContext::empty());

        if let Some(r_ret_ty) = r_ret_ty {
            // Fast path for
            //
            //
            // lhs: ((value: T#0#0) => (TResult1#0#0 | PromiseLike<TResult1>) |
            // undefined | null);
            //
            // rhs: (b: boolean) => Promise<boolean>;

            if cfg!(feature = "fastpath")
                && l_params.len() == 1
                && l_params[0].ty.is_type_param()
                && l_params[0].ty.span().is_dummy()
            {
                if let Some(l_ret_ty) = l_ret_ty {
                    if let Some(r_ret_ty) = unwrap_ref_with_single_arg(&r_ret_ty, "Promise") {
                        match l_ret_ty.normalize() {
                            Type::Union(l_ret_ty) => {
                                // Exact match
                                if l_ret_ty.types.len() == 4
                                    && l_ret_ty.types[0].is_type_param()
                                    && unwrap_ref_with_single_arg(&l_ret_ty.types[1], "PromiseLike")
                                        .type_eq(&Some(&l_ret_ty.types[0]))
                                    && l_ret_ty.types[2]
                                        .is_kwd(TsKeywordTypeKind::TsUndefinedKeyword)
                                    && l_ret_ty.types[3].is_kwd(TsKeywordTypeKind::TsNullKeyword)
                                {
                                    return Ok(());
                                }
                            }
                            _ => {}
                        }
                    }
                }
            }
        }

        if r_type_params.is_some() {
            // If a parameter of lhs is something like
            //
            // (x: {
            //     (a: number) : number;
            //     (a: string) : string;
            // }) : any[];
            //
            // and rhs is
            //
            // <T>(x: (a: T) => T) : T[];
            //
            // we need to infer two time.

            macro_rules! check {
                ($pat:ident) => {{
                    let l_pos = l_params.iter().position(|p| match p.ty.normalize() {
                        Type::TypeLit(ty) => {
                            ty.members
                                .iter()
                                .filter(|v| match v {
                                    TypeElement::$pat(..) => true,
                                    _ => false,
                                })
                                .count()
                                >= 2
                        }
                        _ => false,
                    });

                    if let Some(l_pos) = l_pos {
                        let count = match l_params[l_pos].ty.normalize() {
                            Type::TypeLit(ty) => ty
                                .members
                                .iter()
                                .filter(|v| match v {
                                    TypeElement::$pat(..) => true,
                                    _ => false,
                                })
                                .count(),
                            _ => {
                                unreachable!()
                            }
                        };

                        let mut vec = (0..count)
                            .into_iter()
                            .map(|_| l_params.to_vec())
                            .collect_vec();

                        for (el_idx, new_params) in vec.iter_mut().enumerate() {
                            match new_params[l_pos].ty.normalize_mut() {
                                Type::TypeLit(ty) => {
                                    let mut call_idx = 0;
                                    ty.members.retain(|el| match el {
                                        TypeElement::$pat(..) => {
                                            if el_idx == call_idx {
                                                call_idx += 1;

                                                return true;
                                            }

                                            call_idx += 1;

                                            false
                                        }
                                        _ => true,
                                    });
                                }
                                _ => {
                                    unreachable!()
                                }
                            }
                        }

                        vec.make_clone_cheap();

                        for new_l_params in vec {
                            return self
                                .assign_to_fn_like(
                                    data,
                                    opts,
                                    is_call,
                                    l_type_params,
                                    &new_l_params,
                                    l_ret_ty,
                                    r_type_params,
                                    r_params,
                                    r_ret_ty,
                                )
                                .context(
                                    "tried to assign by expanding overloads in a type literal",
                                );
                        }
                    }
                }};
            }

            check!(Call);
            check!(Constructor);
        }

        match (&l_type_params, r_type_params) {
            (Some(lt), Some(rt)) if lt.params.len() == rt.params.len() && lt.params.len() == 1 => {
                if lt.params[0].constraint.is_none() || rt.params[0].constraint.is_none() {
                    let map = lt
                        .params
                        .iter()
                        .zip(rt.params.iter())
                        .filter(|(l, r)| match (&l.constraint, &r.constraint) {
                            (None, Some(..)) => return false,
                            // TODO(kdy1): Use extends()
                            _ => true,
                        })
                        .map(|(l, r)| (r.name.clone(), Type::Param(l.clone()).cheap()))
                        .collect::<FxHashMap<_, _>>();
                    let mut new_r_params = self
                        .expand_type_params(&map, r_params.to_vec(), Default::default())
                        .context(
                            "tried to expand type parameters as a step of function assignment",
                        )?;
                    let mut new_r_ret_ty = self
                        .expand_type_params(&map, r_ret_ty.cloned(), Default::default())
                        .context(
                            "tried to expand return type of rhs as a step of function assignment",
                        )?;

                    new_r_params.make_clone_cheap();
                    new_r_ret_ty.make_clone_cheap();

                    return self
                        .assign_to_fn_like(
                            data,
                            AssignOpts {
                                allow_assignment_of_void: Some(false),
                                ..opts
                            },
                            is_call,
                            l_type_params,
                            l_params,
                            l_ret_ty,
                            None,
                            &new_r_params,
                            new_r_ret_ty.as_ref(),
                        )
                        .context("tried to assign to a mapped (wrong) function");
                }
            }

            _ => {}
        }

        let (r_params, r_ret_ty) = match (&l_type_params, r_type_params) {
            (Some(lt), None) if opts.infer_type_params_of_left => {
                let opts = AssignOpts {
                    infer_type_params_of_left: false,
                    ..opts
                };

                let lf = Type::Function(Function {
                    span,
                    type_params: None,
                    params: l_params.to_vec(),
                    ret_ty: box l_ret_ty
                        .cloned()
                        .unwrap_or_else(|| Type::any(span, Default::default())),
                    metadata: Default::default(),
                });
                let rf = Type::Function(Function {
                    span,
                    type_params: None,
                    params: r_params.to_vec(),
                    ret_ty: box r_ret_ty
                        .cloned()
                        .unwrap_or_else(|| Type::any(span, Default::default())),
                    metadata: Default::default(),
                });

                let map = self.infer_type_with_types(
                    span,
                    &*lt.params,
                    &lf,
                    &rf,
                    InferTypeOpts {
                        ..Default::default()
                    },
                )?;
                let mut new_l_params = self
                    .expand_type_params(&map, l_params.to_vec(), Default::default())
                    .context(
                        "tried to expand type parameters of lhs as a step of function assignment",
                    )?;
                let mut new_l_ret_ty = self
                    .expand_type_params(&map, l_ret_ty.cloned(), Default::default())
                    .context(
                        "tried to expand return type of lhs as a step of function assignment",
                    )?;

                new_l_params.make_clone_cheap();
                new_l_ret_ty.make_clone_cheap();

                return self
                    .assign_to_fn_like(
                        data,
                        opts,
                        is_call,
                        None,
                        &new_l_params,
                        new_l_ret_ty.as_ref(),
                        None,
                        r_params,
                        r_ret_ty,
                    )
                    .context("tried to assign to an instantiated fn-like stuff");
            }

            // Assigning `(a: 1) => string` to `<Z>(a: Z) => string` is valid.
            (_, Some(rt)) => {
                let lf = Type::Function(Function {
                    span,
                    type_params: None,
                    params: l_params.to_vec(),
                    ret_ty: box l_ret_ty
                        .cloned()
                        .unwrap_or_else(|| Type::any(span, Default::default())),
                    metadata: Default::default(),
                });
                let rf = Type::Function(Function {
                    span,
                    type_params: None,
                    params: r_params.to_vec(),
                    ret_ty: box r_ret_ty
                        .cloned()
                        .unwrap_or_else(|| Type::any(span, Default::default())),
                    metadata: Default::default(),
                });

                let map = self.infer_type_with_types(
                    span,
                    &*rt.params,
                    &rf,
                    &lf,
                    InferTypeOpts {
                        for_fn_assignment: true,
                        ..Default::default()
                    },
                )?;

                if cfg!(debug_assertions) {
                    debug!("Callable map:\n{}", dump_type_map(&self.cm, &map));
                }

                let new_l_params = self
                    .expand_type_params(&map, l_params.to_vec(), Default::default())
                    .context(
                        "tried to expand type parameters of lhs as a step of function assignment",
                    )?
                    .freezed();
                let new_l_ret_ty = self
                    .expand_type_params(&map, l_ret_ty.cloned(), Default::default())
                    .context("tried to expand return type of lhs as a step of function assignment")?
                    .freezed();

                let new_r_params = self
                    .expand_type_params(&map, r_params.to_vec(), Default::default())
                    .context(
                        "tried to expand type parameters of rhs as a step of function assignment",
                    )?
                    .freezed();
                let new_r_ret_ty = self
                    .expand_type_params(&map, r_ret_ty.cloned(), Default::default())
                    .context("tried to expand return type of rhs as a step of function assignment")?
                    .freezed();

                let _panic_ctx = debug_ctx!(format!("new_r_params = {:?}", new_r_params));
                let _panic_ctx = debug_ctx!(format!("new_r_ret_ty = {:?}", new_r_ret_ty));

                return self
                    .assign_to_fn_like(
                        data,
                        opts,
                        is_call,
                        l_type_params,
                        &new_l_params,
                        new_l_ret_ty.as_ref(),
                        None,
                        &new_r_params,
                        new_r_ret_ty.as_ref(),
                    )
                    .with_context(|| {
                        format!(
                            "tried to assign to an expanded callable\nMap:\n{}",
                            dump_type_map(&self.cm, &map)
                        )
                    });
            }

            _ => (r_params, r_ret_ty),
        };

        // TypeScript functions are bivariant if strict_function_types is false.
        if !self.env.rule().strict_function_types || opts.is_params_of_method_definition {
            if self
                .assign_params(
                    data,
                    AssignOpts {
                        is_params_of_method_definition: false,
                        ..opts
                    },
                    &r_params,
                    &l_params,
                )
                .is_ok()
            {
                return Ok(());
            }
        }

        // () => void
        //
        // is assignable to
        //
        // (t: unknown, t1: unknown) => void
        //
        // So we check for length first.
        if r_params.len() != 0 {
            self.assign_params(
                data,
                AssignOpts {
                    is_params_of_method_definition: false,
                    ..opts
                },
                l_params,
                r_params,
            )
            .context(
                "tried to assign parameters of a function to parameters of another function",
            )?;
        }

        if let Some(l_ret_ty) = l_ret_ty {
            if let Some(r_ret_ty) = r_ret_ty {
                // TODO(kdy1): Verify type parameters.

                let opts = AssignOpts {
                    // We are done with the overload context.
                    for_overload: false,
                    allow_assignment_of_void: Some(opts.allow_assignment_of_void.unwrap_or(true)),
                    allow_assignment_to_void: !opts.for_overload,
                    is_params_of_method_definition: false,
                    ..opts
                };

                self.assign_inner(data, l_ret_ty, r_ret_ty, opts).context(
                    "tried to assign the return type of a function to the return type of another \
                     function",
                )?;
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
    #[cfg_attr(debug_assertions, tracing::instrument(skip_all))]
    pub(super) fn assign_to_function(
        &mut self,
        data: &mut AssignData,
        mut opts: AssignOpts,
        lt: &Type,
        l: &Function,
        r: &Type,
    ) -> VResult<()> {
        opts.is_params_of_method_definition = false;

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
                    true,
                    l.type_params.as_ref(),
                    &l.params,
                    Some(&l.ret_ty),
                    r_type_params.as_ref(),
                    r_params,
                    Some(r_ret_ty),
                )
                .context("tried to assign a function to another one")?;

                return Ok(());
            }

            Type::TypeLit(rt) => {
                for rm in &rt.members {
                    match rm {
                        TypeElement::Call(rm) => {
                            return self
                                .assign_to_fn_like(
                                    data,
                                    AssignOpts {
                                        infer_type_params_of_left: true,
                                        ..opts
                                    },
                                    true,
                                    l.type_params.as_ref(),
                                    &l.params,
                                    Some(&l.ret_ty),
                                    rm.type_params.as_ref(),
                                    &rm.params,
                                    rm.ret_ty.as_deref(),
                                )
                                .context("tried to assign TypeElemeny::Call to a function");
                        }
                        _ => {}
                    }
                }
            }

            Type::Interface(..) => {
                let ty = self
                    .convert_type_to_type_lit(span, Cow::Borrowed(r))?
                    .map(Cow::into_owned)
                    .map(Type::TypeLit);
                if let Some(ty) = ty {
                    return self
                        .assign_to_function(data, opts, lt, l, &ty)
                        .context("tried to assign an expanded type to a function");
                }
            }
            _ => {}
        }

        Err(Error::SimpleAssignFailed { span, cause: None })
    }

    ///
    /// # Note
    ///
    /// We should distinguish assign failure due to type parameter instantiation
    /// with assign failure due to type element kind mismatch.
    ///
    /// ```ts
    /// declare var a16: {
    ///     new (x: {
    ///         new (a: number): number;
    ///         new (a?: number): number;
    ///     }): number[];
    ///     new (x: {
    ///         new (a: boolean): boolean;
    ///         new (a?: boolean): boolean;
    ///     }): boolean[];
    /// };
    /// declare var b16: new <T>(x: (a: T) => T) => T[];
    /// a16 = b16; // error
    /// b16 = a16; // error
    ///
    ///
    /// declare var a18: {
    ///     new (x: {
    ///         (a: number): number;
    ///         (a: string): string;
    ///     }): any[];
    ///     new (x: {
    ///         (a: boolean): boolean;
    ///         (a: Date): Date;
    ///     }): any[];
    /// }
    /// declare var b18: new <T>(x: (a: T) => T) => T[];
    /// a18 = b18; // ok
    /// b18 = a18; // ok
    /// ```
    #[cfg_attr(debug_assertions, tracing::instrument(skip_all))]
    pub(super) fn assign_to_constructor(
        &mut self,
        data: &mut AssignData,
        opts: AssignOpts,
        lt: &Type,
        l: &Constructor,
        r: &Type,
    ) -> VResult<()> {
        let span = opts.span;
        let r = r.normalize();

        match r {
            Type::Constructor(rc) => {
                if l.type_eq(rc) {
                    return Ok(());
                }

                self.assign_to_fn_like(
                    data,
                    opts,
                    false,
                    l.type_params.as_ref(),
                    &l.params,
                    Some(&l.type_ann),
                    rc.type_params.as_ref(),
                    &rc.params,
                    Some(&rc.type_ann),
                )
                .context("tried to assign a constructor to another one")?;

                return Ok(());
            }
            Type::Lit(..)
            | Type::ClassDef(ClassDef {
                is_abstract: true, ..
            })
            | Type::Function(..) => return Err(Error::SimpleAssignFailed { span, cause: None }),

            Type::TypeLit(rt) => {
                let r_el_cnt = rt
                    .members
                    .iter()
                    .filter(|m| matches!(m, TypeElement::Constructor(..)))
                    .count();

                let mut errors = vec![];
                for (idx, rm) in rt.members.iter().enumerate() {
                    match rm {
                        TypeElement::Constructor(rc) => {
                            if let Err(err) = self
                                .assign_to_fn_like(
                                    data,
                                    AssignOpts {
                                        allow_assignment_to_param: opts.allow_assignment_to_param
                                            || r_el_cnt > 1,
                                        ..opts
                                    },
                                    false,
                                    l.type_params.as_ref(),
                                    &l.params,
                                    Some(&l.type_ann),
                                    rc.type_params.as_ref(),
                                    &rc.params,
                                    rc.ret_ty.as_deref(),
                                )
                                .with_context(|| {
                                    format!(
                                        "tried to assign a constructor to another constructor \
                                         ({}th element)",
                                        idx
                                    )
                                })
                            {
                                errors.push(err);
                                continue;
                            }

                            return Ok(());
                        }
                        _ => {}
                    }
                }

                if !errors.is_empty() {
                    return Err(Error::SimpleAssignFailedWithCause {
                        span,
                        cause: errors,
                    });
                }
            }
            Type::Interface(..) => {
                let ty = self
                    .convert_type_to_type_lit(span, Cow::Borrowed(r))?
                    .map(Cow::into_owned)
                    .map(Type::TypeLit);
                if let Some(ty) = ty {
                    return self
                        .assign_to_constructor(data, opts, lt, l, &ty)
                        .context("tried to assign an expanded type to a constructor type");
                }
            }
            _ => {}
        }

        Err(Error::SimpleAssignFailed { span, cause: None })
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
    #[cfg_attr(debug_assertions, tracing::instrument(skip_all))]
    fn assign_param(
        &mut self,
        data: &mut AssignData,
        l: &FnParam,
        r: &FnParam,
        opts: AssignOpts,
    ) -> VResult<()> {
        let span = opts.span;
        debug_assert!(
            !opts.span.is_dummy(),
            "Cannot assign function parameters with dummy span"
        );

        let _panic = debug_ctx!(format!(
            "left = {}\n{:?}",
            dump_type_as_string(&self.cm, &l.ty),
            &l.ty
        ));
        let _panic = debug_ctx!(format!(
            "right = {}\n{:?}",
            dump_type_as_string(&self.cm, &r.ty),
            &r.ty
        ));

        match l.pat {
            RPat::Rest(..) => {
                let l_ty = self
                    .normalize(Some(span), Cow::Borrowed(&l.ty), Default::default())
                    .context("tried to normalize lhs")?;

                match l_ty.normalize() {
                    Type::Array(l_arr) => {
                        if let Ok(()) = self.assign_with_opts(data, opts, &l_arr.elem_type, &r.ty) {
                            return Ok(());
                        }
                    }
                    _ => {}
                }
            }
            _ => {}
        }

        // TODO(kdy1): Change this to extends call.

        let mut l_ty = self.normalize(Some(opts.span), Cow::Borrowed(&l.ty), Default::default())?;
        let mut r_ty = self.normalize(Some(opts.span), Cow::Borrowed(&r.ty), Default::default())?;

        l_ty.make_clone_cheap();
        r_ty.make_clone_cheap();

        let res = if opts.for_overload {
            self.assign_with_opts(data, AssignOpts { ..opts }, &l_ty, &r_ty)
                .context("tried to assign the type of a parameter to another")
        } else {
            self.assign_with_opts(data, AssignOpts { ..opts }, &r_ty, &l_ty)
                .context(
                    "tried to assign the type of a parameter to another (reversed due to variance)",
                )
        };

        res.convert_err(|err| match &err {
            Error::MissingFields { span, .. } => Error::SimpleAssignFailed {
                span: *span,
                cause: Some(box err),
            },

            Error::Errors { errors, .. } => {
                if errors.iter().all(|err| match err.actual() {
                    Error::MissingFields { .. } => true,
                    _ => false,
                }) {
                    Error::SimpleAssignFailed {
                        span,
                        cause: Some(box err),
                    }
                } else {
                    err
                }
            }
            _ => err,
        })?;

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
    #[cfg_attr(debug_assertions, tracing::instrument(skip_all))]
    pub(crate) fn assign_params(
        &mut self,
        data: &mut AssignData,
        opts: AssignOpts,
        l: &[FnParam],
        r: &[FnParam],
    ) -> VResult<()> {
        let span = opts.span;

        let li = l.iter().filter(|p| match p.pat {
            RPat::Ident(RBindingIdent {
                id:
                    RIdent {
                        sym: js_word!("this"),
                        ..
                    },
                ..
            }) => false,
            _ => true,
        });
        let ri = r.iter().filter(|p| match p.pat {
            RPat::Ident(RBindingIdent {
                id:
                    RIdent {
                        sym: js_word!("this"),
                        ..
                    },
                ..
            }) => false,
            _ => true,
        });

        let l_has_rest = l.iter().any(|p| match p.pat {
            RPat::Rest(..) => true,
            _ => false,
        });

        // TODO(kdy1): Consider optional parameters.

        let required_li = li.clone().filter(|i| i.required);
        let required_ri = ri.clone().filter(|i| i.required);

        let required_non_void_li = li
            .clone()
            .filter(|i| i.required && !i.ty.is_kwd(TsKeywordTypeKind::TsVoidKeyword));
        let required_non_void_ri = ri
            .clone()
            .filter(|i| i.required && !i.ty.is_kwd(TsKeywordTypeKind::TsVoidKeyword));

        if opts.for_overload {
            if required_li.clone().count() > required_ri.clone().count() {
                return Err(Error::SimpleAssignFailed { span, cause: None })
                    .context("l.params.required.len > r.params.required.len");
            }
        }

        // Don't ask why.
        if li.clone().count() < required_ri.clone().count() {
            if !l_has_rest
                && required_non_void_li.clone().count() < required_non_void_ri.clone().count()
            {
                // I don't know why, but overload signature does not need to match overloaded
                // signature.
                if opts.for_overload {
                    return Ok(());
                }

                return Err(Error::SimpleAssignFailed { span, cause: None }).with_context(|| {
                    format!(
                        "!l_has_rest && l.params.required.len < r.params.required.len\nLeft: \
                         {:?}\nRight: {:?}\n",
                        required_non_void_li.collect_vec(),
                        required_non_void_ri.collect_vec()
                    )
                });
            }
        }

        for pair in li.zip_longest(ri) {
            match pair {
                EitherOrBoth::Both(lp, rp) => {
                    // TODO(kdy1): What should we do?
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
                    .with_context(|| {
                        format!("tried to assign a parameter to another parameter",)
                    })?;
                }
                EitherOrBoth::Left(_) => {}
                EitherOrBoth::Right(_) => {}
            }
        }

        Ok(())
    }
}
