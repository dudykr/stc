use crate::analyzer::expr::IdCtx;
use crate::analyzer::expr::TypeOfMode;
use crate::analyzer::util::ResultExt;
use crate::analyzer::Analyzer;
use crate::ty::TypeExt;
use crate::validator::ValidateWith;
use crate::ValidationResult;
use stc_ts_ast_rnode::RArrayPat;
use stc_ts_ast_rnode::RAssignPatProp;
use stc_ts_ast_rnode::RExpr;
use stc_ts_ast_rnode::RIdent;
use stc_ts_ast_rnode::RNumber;
use stc_ts_ast_rnode::RObjectPat;
use stc_ts_ast_rnode::RObjectPatProp;
use stc_ts_ast_rnode::RPat;
use stc_ts_ast_rnode::RRestPat;
use stc_ts_ast_rnode::RTsEntityName;
use stc_ts_ast_rnode::RTsKeywordType;
use stc_ts_errors::DebugExt;
use stc_ts_errors::Error;
use stc_ts_types::Id;
use stc_ts_types::Key;
use stc_ts_types::Ref;
use stc_ts_types::Type;
use stc_ts_types::TypeLit;
use stc_ts_types::TypeParamInstantiation;
use stc_ts_utils::OptionExt;
use stc_ts_utils::PatExt;
use swc_common::Spanned;
use swc_common::DUMMY_SP;
use swc_ecma_ast::TsKeywordTypeKind;
use swc_ecma_ast::VarDeclKind;

impl Analyzer<'_, '_> {
    /// Updates variable list.
    ///
    /// This method should be called for function parameters including error
    /// variable from a catch clause.
    pub(super) fn declare_vars_inner_with_ty(
        &mut self,
        kind: VarDeclKind,
        pat: &RPat,
        export: bool,
        ty: Option<Type>,
        actual_ty: Option<Type>,
    ) -> ValidationResult<()> {
        let span = ty
            .as_ref()
            .map(|v| v.span())
            .and_then(|span| if span.is_dummy() { None } else { Some(span) })
            .unwrap_or_else(|| pat.span());
        if !self.is_builtin {
            assert_ne!(span, DUMMY_SP);
        }

        match &*pat {
            RPat::Ident(i) => {
                let name: Id = Id::from(i.clone());
                if !self.is_builtin {
                    debug_assert_ne!(span, DUMMY_SP);
                }
                let ty = match ty {
                    None => try_opt!(i.type_ann.as_ref().map(|v| v.type_ann.validate_with(self))),
                    Some(ty) => Some(ty),
                };
                self.declare_var(
                    span,
                    kind,
                    name.clone(),
                    ty.clone(),
                    actual_ty,
                    // initialized
                    true,
                    // allow_multiple
                    kind == VarDeclKind::Var,
                )?;
                if export {
                    self.storage
                        .store_private_var(self.ctx.module_id, name.clone(), ty.unwrap_or(Type::any(i.span)));
                    self.storage.export_var(span, self.ctx.module_id, name);
                }
                return Ok(());
            }
            RPat::Assign(ref p) => {
                slog::debug!(
                    self.logger,
                    "({}) declare_vars: Assign({:?}), ty = {:?}",
                    self.scope.depth(),
                    p.left,
                    ty
                );
                self.declare_vars_inner_with_ty(kind, &p.left, export, ty, actual_ty)?;

                return Ok(());
            }

            RPat::Array(RArrayPat {
                span,
                ref elems,
                ref type_ann,
                ref optional,
                node_id,
                ..
            }) => {
                let ty = match ty {
                    None => try_opt!(type_ann.as_ref().map(|v| v.type_ann.validate_with(self))),
                    Some(ty) => Some(ty),
                };

                // TODO: Handle type annotation

                if type_ann.is_none() {
                    if let Some(ty) = ty.clone() {
                        if let Some(m) = &mut self.mutations {
                            m.for_pats.entry(*node_id).or_default().optional = Some(true);
                            m.for_pats
                                .entry(*node_id)
                                .or_default()
                                .ty
                                .fill_with(|| ty.generalize_lit().generalize_tuple().into());
                        }
                    }
                }

                if type_ann.is_none() {
                    let ctxt = self.ctx.module_id;
                    if let Some(m) = &mut self.mutations {
                        //
                        m.for_pats.entry(*node_id).or_default().ty.fill_with(|| {
                            Type::Ref(Ref {
                                span: *span,
                                ctxt,
                                type_name: RTsEntityName::Ident(RIdent::new("Iterable".into(), DUMMY_SP)),
                                type_args: Some(box TypeParamInstantiation {
                                    span: *span,
                                    params: vec![Type::Keyword(RTsKeywordType {
                                        span: DUMMY_SP,
                                        kind: TsKeywordTypeKind::TsAnyKeyword,
                                    })],
                                }),
                            })
                        });
                    }
                }

                // TODO: Store type.
                for (idx, elem) in elems.iter().enumerate() {
                    match elem {
                        Some(elem) => {
                            let elem_ty = match &ty {
                                Some(ty) => self
                                    .access_property(
                                        elem.span(),
                                        ty.clone(),
                                        &Key::Num(RNumber {
                                            span: elem.span(),
                                            value: idx as f64,
                                        }),
                                        TypeOfMode::RValue,
                                        IdCtx::Var,
                                    )
                                    .map(Some)
                                    .context("tried to access property to declare variables using an array pattern")?,
                                None => None,
                            };

                            // TODO: actual_ty
                            self.declare_vars_inner_with_ty(kind, elem, export, elem_ty, None)?;
                        }
                        // Skip
                        None => {}
                    }
                }

                return Ok(());
            }

            RPat::Object(RObjectPat {
                ref props,
                ref type_ann,
                node_id,
                ..
            }) => {
                if self.ctx.in_declare {
                    self.storage.report(Error::DestructuringAssignInAmbientContext { span });
                }

                let ty = match ty {
                    None => try_opt!(type_ann.as_ref().map(|v| v.type_ann.validate_with(self))),
                    Some(ty) => Some(ty),
                };

                if type_ann.is_none() {
                    if let Some(m) = &mut self.mutations {
                        m.for_pats.entry(*node_id).or_default().ty = Some(Type::TypeLit(TypeLit {
                            span,
                            // TODO: Fill it
                            members: vec![],
                            metadata: Default::default(),
                        }));
                    }
                }

                for prop in props {
                    match prop {
                        RObjectPatProp::Assign(RAssignPatProp { span, key, value, .. }) => {
                            let span = *span;

                            let prop_ty = match &ty {
                                Some(ty) => self
                                    .access_property(
                                        span,
                                        ty.clone(),
                                        &Key::Normal {
                                            span: key.span,
                                            sym: key.sym.clone(),
                                        },
                                        TypeOfMode::RValue,
                                        IdCtx::Var,
                                    )
                                    .convert_err(|err| match err {
                                        Error::NoSuchProperty { span, .. } if value.is_none() => {
                                            Error::NoSuchPropertyWhileDeclWithBidningPat { span }
                                        }
                                        _ => err,
                                    })
                                    .context("tried to access property to declare variables using an object pattern")
                                    .report(&mut self.storage),
                                None => None,
                            };

                            match value {
                                Some(value) => {
                                    // TODO: Assign this
                                    let _type_of_default_value =
                                        value.validate_with_default(self).report(&mut self.storage);

                                    // TODO: actual_ty
                                    self.declare_vars_inner_with_ty(
                                        kind,
                                        &RPat::Ident(key.clone()),
                                        export,
                                        prop_ty,
                                        None,
                                    )
                                    .context(
                                        "tried to declare a variable from an assignment property in an object pattern",
                                    )?;
                                }
                                None => {
                                    // TODO: actual_ty
                                    self.declare_vars_inner_with_ty(
                                        kind,
                                        &RPat::Ident(key.clone()),
                                        export,
                                        prop_ty,
                                        None,
                                    )
                                    .context(
                                        "tried to declare a variable from a simple property in an object pattern",
                                    )?;
                                }
                            }
                        }

                        RObjectPatProp::KeyValue(p) => {
                            let span = p.span();
                            let key = p.key.validate_with(self)?;

                            let prop_ty = match &ty {
                                Some(ty) => self
                                    .access_property(span, ty.clone(), &key, TypeOfMode::RValue, IdCtx::Var)
                                    .map(Some)
                                    .context("tried to access property to declare variables using an object pattern")?,
                                None => None,
                            };

                            // TODO: actual_ty
                            self.declare_vars_inner_with_ty(kind, &p.value, export, prop_ty, None)
                                .context("tried to declare a variable from key-value property in an object pattern")?;
                        }

                        RObjectPatProp::Rest(RRestPat { .. }) => {
                            unimplemented!("rest pattern in object pattern")
                        }
                    }
                }

                return Ok(());
            }

            RPat::Rest(RRestPat {
                ref arg,
                type_ann: ref ty,
                node_id,
                ..
            }) => {
                let mut arg = arg.clone();

                self.declare_vars_inner(kind, &arg, export)?;

                let new_ty = arg.get_mut_ty().take();
                if ty.is_none() {
                    if let Some(arg_node_id) = arg.node_id() {
                        if let Some(m) = &mut self.mutations {
                            let ty = m.for_pats.entry(arg_node_id).or_default().ty.take();
                            if let Some(ty) = ty {
                                m.for_pats.entry(*node_id).or_default().ty = Some(ty);
                            }
                        }
                    }
                }

                return Ok(());
            }

            RPat::Invalid(..) | RPat::Expr(box RExpr::Invalid(..)) => Ok(()),

            _ => unimplemented!("declare_vars for patterns other than ident: {:#?}", pat),
        }
    }
}
