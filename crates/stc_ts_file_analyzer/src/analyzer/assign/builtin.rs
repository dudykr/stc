use std::borrow::Cow;

use stc_ts_ast_rnode::{RExpr, RIdent, RTsEntityName};
use stc_ts_errors::{DebugExt, ErrorKind};
use stc_ts_types::{Array, ArrayMetadata, Ref, Type, TypeElement};
use swc_atoms::js_word;
use swc_common::{Spanned, TypeEq};

use crate::{
    analyzer::{
        assign::{AssignData, AssignOpts},
        Analyzer,
    },
    util::unwrap_builtin_with_single_arg,
    VResult,
};

impl Analyzer<'_, '_> {
    /// This handles the assignment to builtin types.
    ///
    /// - Handles assignment of `Function` types.
    /// - Handles assignment of various array types.
    /// - Handles assignment of promise types.
    pub(super) fn assign_to_builtin(&self, data: &mut AssignData, l: &Type, r: &Type, opts: AssignOpts) -> Option<VResult<()>> {
        let span = opts.span;
        let l = l.normalize();
        let r = r.normalize();

        match l {
            Type::Ref(Ref {
                type_name: RTsEntityName::Ident(RIdent { sym, .. }),
                ..
            }) if *sym == *"ThisType" => return Some(Ok(())),

            Type::Ref(Ref {
                type_name: RTsEntityName::Ident(RIdent {
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
                            return Some(Err(ErrorKind::TupleAssignError { span, errors }.into()));
                        }
                    }
                    return Some(Ok(()));
                }
                _ => {}
            },

            Type::Ref(Ref {
                type_name: RTsEntityName::Ident(RIdent {
                    sym: js_word!("Function"), ..
                }),
                ..
            }) => match r {
                Type::Ref(Ref {
                    type_name: RTsEntityName::Ident(RIdent {
                        sym: js_word!("Function"), ..
                    }),
                    ..
                }) => return Some(Ok(())),

                Type::TypeLit(rt) => {
                    if rt
                        .members
                        .iter()
                        .any(|r| matches!(r, TypeElement::Call(..) | TypeElement::Constructor(..)))
                    {
                        return Some(Ok(()));
                    }

                    return Some(Err(ErrorKind::NoCallSignature {
                        span: opts.span,
                        callee: Box::new(r.clone()),
                    }
                    .into()));
                }
                Type::Interface(ri) => {
                    if *ri.name.sym() == js_word!("Function") {
                        return Some(Ok(()));
                    }

                    if ri
                        .body
                        .iter()
                        .any(|r| matches!(r, TypeElement::Call(..) | TypeElement::Constructor(..)))
                    {
                        return Some(Ok(()));
                    }

                    for parent in &ri.extends {
                        if let RExpr::Ident(RIdent {
                            sym: js_word!("Function"), ..
                        }) = &*parent.expr
                        {
                            return Some(Ok(()));
                        }

                        let parent = self.type_of_ts_entity_name(opts.span, &parent.expr, parent.type_args.as_deref());
                        let parent = match parent {
                            Ok(ty) => ty,
                            Err(err) => return Some(Err(err)),
                        };

                        if let Some(Ok(())) = self.assign_to_builtin(data, l, &parent, opts) {
                            return Some(Ok(()));
                        }
                    }

                    return Some(Err(ErrorKind::NoCallSignature {
                        span: opts.span,
                        callee: Box::new(r.clone()),
                    }
                    .into()));
                }
                _ => {}
            },

            Type::Ref(Ref {
                type_name: RTsEntityName::Ident(type_name),
                type_args: Some(type_args),
                ..
            }) if type_name.sym == *"ReadonlyArray" => {
                if type_args.params.len() == 1 {
                    if let Type::Array(Array { elem_type, .. }) = r.normalize() {
                        return Some(
                            self.assign_inner(data, &type_args.params[0], elem_type, opts)
                                .context("tried to assign an array to a readonly array (builtin)"),
                        );
                    }
                }
            }

            _ => {}
        }

        if cfg!(feature = "fastpath") {
            if let Type::Array(l) = l {
                if let Some(r_elem) = unwrap_builtin_with_single_arg(r, "TemplateStringArray")
                    .or_else(|| unwrap_builtin_with_single_arg(r, "Array"))
                    .or_else(|| unwrap_builtin_with_single_arg(r, "ReadonlyArray"))
                {
                    return Some(
                        self.assign_with_opts(data, &l.elem_type, r_elem, opts)
                            .context("tried fast-path assignment to an array"),
                    );
                }
            }
        }

        if cfg!(feature = "fastpath") {
            if let Some(r_elem) = unwrap_builtin_with_single_arg(r, "ReadonlyArray") {
                return Some(self.assign_with_opts(
                    data,
                    l,
                    &Type::Array(Array {
                        span: r.span(),
                        elem_type: Box::new(r_elem.clone()),
                        metadata: ArrayMetadata { common: r.metadata() },
                        tracker: Default::default(),
                    }),
                    opts,
                ));
            }
        }

        if cfg!(feature = "fastpath") && opts.allow_assignment_to_param {
            // Fast path for
            //
            // lhs: (TResult1#0#0 | PromiseLike<TResult1>);
            // rhs: Promise<boolean>
            if let Type::Union(l) = l.normalize() {
                if l.types.len() == 2 && unwrap_builtin_with_single_arg(&l.types[1], "PromiseLike").type_eq(&Some(&l.types[0])) {
                    return Some(Ok(()));
                }
            }
        }

        if cfg!(feature = "fastpath") {
            if let Type::Union(l) = l.normalize() {
                if let Some(r) = unwrap_builtin_with_single_arg(r, "Promise") {
                    // Fast path for
                    //
                    // (Promise<number> | Promise<string> | Promise<boolean> |
                    // PromiseLike<(Promise<number> | Promise<string> |
                    // Promise<boolean>)>); = Promise<boolean>;
                    let mut done = true;
                    for l in &l.types {
                        if let Some(l) = unwrap_builtin_with_single_arg(l, "Promise") {
                            if let Ok(()) = self.assign_with_opts(data, l, r, opts) {
                                return Some(Ok(()));
                            }
                        } else {
                            done = false;
                        }
                    }

                    if done {
                        return Some(Err(ErrorKind::SimpleAssignFailed { span, cause: None }
                            .context("tried optimized assignment of `Promise<T>` to union")));
                    }
                }
            }
        }

        if opts.may_unwrap_promise {
            if let Ok(l) = self.get_awaited_type(span, Cow::Borrowed(l), true) {
                // We are in return type of an async function.

                if let Ok(r) = self.get_awaited_type(span, Cow::Borrowed(r), true) {
                    return Some(
                        self.assign_with_opts(data, &l, &r, AssignOpts { ..opts })
                            .context("tried to assign an awaited type to an awaited type"),
                    );
                }

                return Some(
                    self.assign_with_opts(
                        data,
                        &l,
                        r,
                        AssignOpts {
                            may_unwrap_promise: false,
                            ..opts
                        },
                    )
                    .context("tried to assign an awaited type to a non-awaited type"),
                );
            } else {
                if let Ok(r) = self.get_awaited_type(span, Cow::Borrowed(r), true) {
                    return Some(
                        self.assign_with_opts(data, l, &r, AssignOpts { ..opts })
                            .context("tried to assign an non-waited type to an awaited type"),
                    );
                }
            }
        }

        if let Some(l) = unwrap_builtin_with_single_arg(l, "Promise") {
            if let Some(r) = unwrap_builtin_with_single_arg(r, "Promise") {
                return Some(
                    self.assign_with_opts(data, l, r, opts)
                        .context("tried to assign a promise to another using optimized algorithm"),
                );
            }
        }

        None
    }
}
