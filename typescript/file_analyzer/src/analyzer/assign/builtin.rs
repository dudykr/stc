use crate::{
    analyzer::{
        assign::{AssignData, AssignOpts},
        Analyzer,
    },
    util::unwrap_ref_with_single_arg,
    ValidationResult,
};
use stc_ts_ast_rnode::{RIdent, RTsEntityName};
use stc_ts_errors::{DebugExt, Error};
use stc_ts_types::{Array, ArrayMetadata, Ref, Type, TypeElement};
use swc_atoms::js_word;
use swc_common::{Spanned, TypeEq};

impl Analyzer<'_, '_> {
    /// This handles the assignment to builtin types.
    ///
    /// - Handles assignment of `Function` types.
    /// - Handles assignment of various array types.
    /// - Handles assignment of promise types.
    pub(super) fn assign_to_builtins(
        &mut self,
        data: &mut AssignData,
        opts: AssignOpts,
        l: &Type,
        r: &Type,
    ) -> Option<ValidationResult<()>> {
        let span = opts.span;
        let l = l.normalize();
        let r = r.normalize();

        match l {
            Type::Ref(Ref {
                type_name: RTsEntityName::Ident(RIdent { sym, .. }),
                ..
            }) if *sym == *"ThisType" => return Some(Ok(())),

            Type::Ref(Ref {
                type_name:
                    RTsEntityName::Ident(RIdent {
                        sym: js_word!("Array"), ..
                    }),
                type_args: Some(type_args),
                ..
            }) => match r {
                Type::Array(r) => {
                    if type_args.params.len() == 1 {
                        return Some(self.assign_inner(data, &type_args.params[0], &r.elem_type, opts));
                    }
                    return Some(Ok(()));
                }
                Type::Tuple(r) => {
                    if type_args.params.len() == 1 {
                        let mut errors = vec![];
                        for el in &r.elems {
                            errors.extend(self.assign_inner(data, &type_args.params[0], &el.ty, opts).err());
                        }
                        if !errors.is_empty() {
                            return Some(Err(Error::TupleAssignError { span, errors }));
                        }
                    }
                    return Some(Ok(()));
                }
                _ => {}
            },

            Type::Ref(Ref {
                type_name:
                    RTsEntityName::Ident(RIdent {
                        sym: js_word!("Function"),
                        ..
                    }),
                ..
            }) => match r {
                Type::Ref(Ref {
                    type_name:
                        RTsEntityName::Ident(RIdent {
                            sym: js_word!("Function"),
                            ..
                        }),
                    ..
                }) => return Some(Ok(())),

                Type::TypeLit(rt) => {
                    if rt.members.iter().any(|r| match r {
                        TypeElement::Call(..) | TypeElement::Constructor(..) => true,
                        _ => false,
                    }) {
                        return Some(Ok(()));
                    }

                    return Some(Err(Error::NoCallSignature {
                        span: opts.span,
                        callee: box r.clone(),
                    }));
                }
                Type::Interface(ri) => {
                    if *ri.name.sym() == js_word!("Function") {
                        return Some(Ok(()));
                    }

                    if ri.body.iter().any(|r| match r {
                        TypeElement::Call(..) | TypeElement::Constructor(..) => true,
                        _ => false,
                    }) {
                        return Some(Ok(()));
                    }

                    for parent in &ri.extends {
                        match parent.expr {
                            RTsEntityName::Ident(RIdent {
                                sym: js_word!("Function"),
                                ..
                            }) => return Some(Ok(())),
                            _ => {}
                        }

                        let parent = self.type_of_ts_entity_name(
                            opts.span,
                            self.ctx.module_id,
                            &parent.expr,
                            parent.type_args.as_deref(),
                        );
                        let parent = match parent {
                            Ok(ty) => ty,
                            Err(err) => return Some(Err(err)),
                        };

                        if let Some(Ok(())) = self.assign_to_builtins(data, opts, l, &parent) {
                            return Some(Ok(()));
                        }
                    }

                    return Some(Err(Error::NoCallSignature {
                        span: opts.span,
                        callee: box r.clone(),
                    }));
                }
                _ => {}
            },

            Type::Ref(Ref {
                type_name: RTsEntityName::Ident(type_name),
                type_args,
                ..
            }) if type_name.sym == *"ReadonlyArray" => match type_args {
                Some(type_args) => {
                    if type_args.params.len() == 1 {
                        match r {
                            Type::Array(Array { elem_type, .. }) => {
                                return Some(
                                    self.assign_inner(data, &type_args.params[0], elem_type, opts)
                                        .context("tried to assign an array to a readonly array (builtin)"),
                                );
                            }
                            _ => {}
                        }
                    }
                }
                None => {}
            },

            _ => {}
        }

        if cfg!(feature = "fastpath") {
            if let Type::Array(l) = l {
                if let Some(r_elem) = unwrap_ref_with_single_arg(r, "TemplateStringArray")
                    .or_else(|| unwrap_ref_with_single_arg(r, "Array"))
                    .or_else(|| unwrap_ref_with_single_arg(r, "ReadonlyArray"))
                {
                    return Some(
                        self.assign_with_opts(data, opts, &l.elem_type, &r)
                            .context("tried fast-path assignment to an array"),
                    );
                }
            }
        }

        if cfg!(feature = "fastpath") {
            if let Some(r_elem) = unwrap_ref_with_single_arg(r, "ReadonlyArray") {
                return Some(self.assign_with_opts(
                    data,
                    opts,
                    &l,
                    &Type::Array(Array {
                        span: r.span(),
                        elem_type: box r_elem.clone(),
                        metadata: ArrayMetadata { common: r.metadata() },
                    }),
                ));
            }
        }

        if cfg!(feature = "fastpath") && opts.allow_assignment_to_param {
            // Fast path for
            //
            // lhs: (TResult1#0#0 | PromiseLike<TResult1>);
            // rhs: Promise<boolean>
            match l.normalize() {
                Type::Union(l) => {
                    if l.types.len() == 2
                        && l.types[0].normalize().is_type_param()
                        && unwrap_ref_with_single_arg(&l.types[1], "PromiseLike").type_eq(&Some(&l.types[0]))
                    {
                        return Some(Ok(()));
                    }
                }
                _ => {}
            }
        }

        if cfg!(feature = "fastpath") {
            match l.normalize() {
                Type::Union(l) => {
                    if let Some(r) = unwrap_ref_with_single_arg(r, "Promise") {
                        // Fast path for
                        //
                        // (Promise<number> | Promise<string> | Promise<boolean> |
                        // PromiseLike<(Promise<number> | Promise<string> |
                        // Promise<boolean>)>); = Promise<boolean>;
                        let mut done = true;
                        for l in &l.types {
                            if let Some(l) = unwrap_ref_with_single_arg(l, "Promise") {
                                if let Ok(()) = self.assign_with_opts(data, opts, l, r) {
                                    return Some(Ok(()));
                                }
                            } else {
                                done = false;
                            }
                        }

                        if done {
                            return Some(Err(Error::SimpleAssignFailed { span, cause: None }
                                .context("tried optimized assignment of `Promise<T>` to union")));
                        }
                    }
                }
                _ => {}
            }
        }

        if opts.may_unwrap_promise {
            if let Some(l) = unwrap_ref_with_single_arg(l, "Promise") {
                // We are in return type of an async function.

                if let Ok(()) = self.assign_with_opts(
                    data,
                    AssignOpts {
                        may_unwrap_promise: false,
                        ..opts
                    },
                    l,
                    &r,
                ) {
                    return Some(Ok(()));
                }

                if let Some(r) = unwrap_ref_with_single_arg(r, "Promise") {
                    let r = self.normalize_promise_arg(r);

                    if let Ok(()) = self.assign_with_opts(
                        data,
                        AssignOpts {
                            may_unwrap_promise: false,
                            ..opts
                        },
                        l,
                        &r,
                    ) {
                        return Some(Ok(()));
                    }
                }
            }
        }

        if let Some(l) = unwrap_ref_with_single_arg(l, "Promise") {
            if let Some(r) = unwrap_ref_with_single_arg(r, "Promise") {
                return Some(
                    self.assign_with_opts(data, opts, l, r)
                        .context("tried to assign a promise to another using optimized algorithm"),
                );
            }
        }

        None
    }
}
