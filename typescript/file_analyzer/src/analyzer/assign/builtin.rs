use super::AssignData;
use super::AssignOpts;
use crate::analyzer::Analyzer;
use crate::ValidationResult;
use stc_ts_ast_rnode::RIdent;
use stc_ts_ast_rnode::RTsEntityName;
use stc_ts_errors::DebugExt;
use stc_ts_errors::Error;
use stc_ts_types::Array;
use stc_ts_types::Ref;
use stc_ts_types::Type;
use stc_ts_types::TypeElement;
use swc_atoms::js_word;

impl Analyzer<'_, '_> {
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

fn unwrap_ref_with_single_arg<'a>(ty: &'a Type, wanted_ref_name: &str) -> Option<&'a Type> {
    match ty.normalize() {
        Type::Ref(Ref {
            type_name: RTsEntityName::Ident(l_type_name),
            type_args: l_type_args,
            ..
        }) if l_type_name.sym == *wanted_ref_name => match l_type_args {
            Some(l_type_args) => {
                if l_type_args.params.len() == 1 {
                    return Some(&l_type_args.params[0]);
                }
            }
            None => {}
        },

        _ => {}
    }

    None
}
