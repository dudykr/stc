use crate::analyzer::expr::GetIteratorOpts;
use crate::analyzer::expr::IdCtx;
use crate::analyzer::expr::TypeOfMode;
use crate::analyzer::pat::PatMode;
use crate::analyzer::types::NormalizeTypeOpts;
use crate::analyzer::util::opt_union;
use crate::analyzer::util::ResultExt;
use crate::analyzer::Analyzer;
use crate::ty::TypeExt;
use crate::validator::ValidateWith;
use crate::ValidationResult;
use itertools::Itertools;
use rnode::NodeId;
use stc_ts_ast_rnode::RArrayPat;
use stc_ts_ast_rnode::RBindingIdent;
use stc_ts_ast_rnode::RExpr;
use stc_ts_ast_rnode::RIdent;
use stc_ts_ast_rnode::RNumber;
use stc_ts_ast_rnode::RObjectPat;
use stc_ts_ast_rnode::RObjectPatProp;
use stc_ts_ast_rnode::RPat;
use stc_ts_ast_rnode::RRestPat;
use stc_ts_ast_rnode::RStr;
use stc_ts_ast_rnode::RTsEntityName;
use stc_ts_ast_rnode::RTsKeywordType;
use stc_ts_ast_rnode::RTsLit;
use stc_ts_ast_rnode::RTsLitType;
use stc_ts_errors::debug::dump_type_as_string;
use stc_ts_errors::DebugExt;
use stc_ts_errors::Error;
use stc_ts_type_ops::Fix;
use stc_ts_types::Id;
use stc_ts_types::Key;
use stc_ts_types::ModuleId;
use stc_ts_types::Ref;
use stc_ts_types::Type;
use stc_ts_types::TypeLit;
use stc_ts_types::TypeParamInstantiation;
use stc_ts_types::Union;
use stc_ts_utils::OptionExt;
use stc_ts_utils::PatExt;
use stc_utils::TryOpt;
use std::borrow::Cow;
use swc_common::Span;
use swc_common::Spanned;
use swc_common::DUMMY_SP;
use swc_ecma_ast::TsKeywordTypeKind;
use swc_ecma_ast::VarDeclKind;

/// All bool fields default to `false`.
#[derive(Debug, Default, Clone, Copy)]
pub(crate) struct DeclareVarsOpts {
    pub kind: Option<VarDeclKind>,
    pub use_iterator_for_array: bool,
}

impl Analyzer<'_, '_> {
    /// TODO: Rename to declare_vars
    ///
    /// # Parameters
    ///
    ///
    /// ## actual
    ///
    /// The type of actual value.
    ///
    ///
    /// ## default
    ///
    /// The type of default value specified by an assignment pattern.
    pub(crate) fn add_vars(
        &mut self,
        pat: &RPat,
        ty: Option<Type>,
        actual: Option<Type>,
        default: Option<Type>,
        opts: DeclareVarsOpts,
    ) -> ValidationResult<()> {
        if let Some(ty) = &ty {
            ty.assert_valid();
        }
        if let Some(ty) = &actual {
            ty.assert_valid();
        }
        if let Some(ty) = &default {
            ty.assert_valid();
        }

        let span = pat.span();

        if match pat {
            RPat::Ident(..) => false,
            _ => true,
        } {
            match ty.as_ref().map(Type::normalize) {
                Some(ty @ Type::Ref(..)) => {
                    let ty = self
                        .expand_top_ref(ty.span(), Cow::Borrowed(&ty))
                        .context("tried to expand reference to declare a complex variable")?;

                    return self.add_vars(pat, Some(ty), actual, default, opts);
                }
                _ => {}
            }
        }

        match pat {
            RPat::Ident(i) => {
                if let Some(ty) = &ty {
                    slog::debug!(
                        &self.logger,
                        "[vars]: Declaring {} as {}",
                        i.id.sym,
                        dump_type_as_string(&self.cm, &ty)
                    );
                } else {
                    slog::debug!(&self.logger, "[vars]: Declaring {} without type", i.id.sym);
                }

                let ty = opt_union(span, ty, default);

                let kind = opts.kind.unwrap_or(VarDeclKind::Var);

                self.declare_var(
                    span,
                    kind,
                    i.id.clone().into(),
                    ty,
                    actual,
                    // initialized
                    true,
                    // let/const declarations does not allow multiple declarations with
                    // same name
                    kind == VarDeclKind::Var,
                    false,
                )?;
                Ok(())
            }

            RPat::Assign(p) => {
                let right = p
                    .right
                    .validate_with_args(self, (TypeOfMode::RValue, None, None))
                    .report(&mut self.storage)
                    .unwrap_or_else(|| Type::any(span));

                let default = opt_union(span, default, Some(right));

                return self
                    .add_vars(&p.left, ty, actual, default, opts)
                    .context("tried to declare a variable with an assignment pattern");
            }

            RPat::Array(arr) => {
                if opts.use_iterator_for_array {
                    // Handle tuple
                    //
                    //      const [a , setA] = useState();
                    //

                    let ty = ty.map(|ty| {
                        self.get_iterator(
                            span,
                            Cow::Owned(ty),
                            GetIteratorOpts {
                                disallow_str: true,
                                ..Default::default()
                            },
                        )
                        .context("tried to convert a type to an iterator to assign with an array pattern.")
                        .unwrap_or_else(|err| {
                            self.storage.report(err);
                            Cow::Owned(Type::any(span))
                        })
                    });

                    let default = default.map(|ty| {
                        self.get_iterator(
                            span,
                            Cow::Owned(ty),
                            GetIteratorOpts {
                                disallow_str: true,
                                ..Default::default()
                            },
                        )
                        .context(
                            "tried to convert a type to an iterator to assign with an array pattern (default value)",
                        )
                        .unwrap_or_else(|err| {
                            self.storage.report(err);
                            Cow::Owned(Type::any(span))
                        })
                    });

                    for (i, elem) in arr.elems.iter().enumerate() {
                        if let Some(elem) = elem {
                            let elem_ty = ty.as_ref().try_map(|ty| {
                                Ok(self
                                    .get_element_from_iterator(span, Cow::Borrowed(&ty), i)
                                    .context(
                                        "tried to get the type of nth element from iterator to declare vars with an \
                                         array pattern",
                                    )?
                                    .into_owned())
                            })?;

                            let default_elem_ty = default
                                .as_ref()
                                .and_then(|ty| {
                                    self.get_element_from_iterator(span, Cow::Borrowed(&ty), i)
                                        .context(
                                            "tried to get the type of nth element from iterator to declare vars with \
                                             an array pattern (default value)",
                                        )
                                        .ok()
                                })
                                .map(Cow::into_owned);

                            // TODO: actual_ty
                            self.add_vars(elem, elem_ty, None, default_elem_ty, opts)?;
                        }
                    }

                    Ok(())
                } else {
                }
            }

            _ => {}
        }
    }

    pub(crate) fn exclude_props(&mut self, span: Span, ty: &Type, keys: &[Key]) -> ValidationResult<Type> {
        let ty = (|| -> ValidationResult<_> {
            let ty = self.normalize(
                None,
                Cow::Borrowed(ty),
                NormalizeTypeOpts {
                    preserve_mapped: false,
                    ..Default::default()
                },
            )?;

            if ty.is_any() || ty.is_kwd(TsKeywordTypeKind::TsObjectKeyword) {
                return Ok(ty.into_owned());
            }

            match ty.normalize() {
                Type::TypeLit(lit) => {
                    let mut new_members = vec![];
                    'outer: for m in &lit.members {
                        if let Some(key) = m.key() {
                            for prop in keys {
                                if self.key_matches(span, &key, prop, false) {
                                    continue 'outer;
                                }
                            }

                            new_members.push(m.clone());
                        }
                    }

                    return Ok(Type::TypeLit(TypeLit {
                        span: lit.span,
                        members: new_members,
                        metadata: lit.metadata,
                    }));
                }

                Type::Union(u) => {
                    let types = u
                        .types
                        .iter()
                        .map(|ty| self.exclude_props(span, ty, keys))
                        .collect::<Result<_, _>>()?;

                    return Ok(Type::Union(Union { span: u.span, types }));
                }

                Type::Intersection(..) | Type::Class(..) | Type::Interface(..) | Type::ClassDef(..) => {
                    let ty = self
                        .type_to_type_lit(ty.span(), &ty)?
                        .map(Cow::into_owned)
                        .map(Type::TypeLit);
                    if let Some(ty) = ty {
                        return self.exclude_props(span, &ty, keys);
                    }
                }
                // TODO
                Type::Function(..) | Type::Constructor(..) => {
                    return Ok(Type::TypeLit(TypeLit {
                        span: ty.span(),
                        members: vec![],
                        metadata: Default::default(),
                    }))
                }

                // Create Omit<T, 'foo' | 'bar'>
                Type::Param(..) => {
                    let mut key_types = keys
                        .iter()
                        .filter_map(|key| match key {
                            Key::BigInt(v) => Some(Type::Lit(RTsLitType {
                                node_id: NodeId::invalid(),
                                span: v.span,
                                lit: RTsLit::BigInt(v.clone()),
                            })),
                            Key::Num(v) => Some(Type::Lit(RTsLitType {
                                node_id: NodeId::invalid(),
                                span: v.span,
                                lit: RTsLit::Number(v.clone()),
                            })),
                            Key::Normal { span, sym } => Some(Type::Lit(RTsLitType {
                                node_id: NodeId::invalid(),
                                span: *span,
                                lit: RTsLit::Str(RStr {
                                    span: *span,
                                    value: sym.clone(),
                                    has_escape: false,
                                    kind: Default::default(),
                                }),
                            })),

                            // TODO
                            _ => None,
                        })
                        .collect_vec();
                    if key_types.is_empty() {
                        return Ok(ty.into_owned());
                    }
                    let keys = Type::Union(Union { span, types: key_types });

                    return Ok(Type::Ref(Ref {
                        span,
                        ctxt: ModuleId::builtin(),
                        type_name: RTsEntityName::Ident(RIdent::new("Omit".into(), DUMMY_SP)),
                        type_args: Some(box TypeParamInstantiation {
                            span,
                            params: vec![ty.clone().into_owned(), keys],
                        }),
                    }));
                }
                _ => {}
            }

            unimplemented!("exclude_props: {:#?}", ty)
        })()?;

        Ok(ty.fixed())
    }

    /// Updates variable list.
    ///
    /// This method should be called for function parameters including error
    /// variable from a catch clause.
    pub(super) fn declare_vars_inner_with_ty(
        &mut self,
        kind: VarDeclKind,
        pat: &RPat,
        ty: Option<Type>,
        actual_ty: Option<Type>,
        default_ty: Option<Type>,
    ) -> ValidationResult<()> {
        let marks = self.marks();

        match &ty {
            Some(ty) => {
                ty.assert_valid();
            }
            _ => {}
        }
        match &actual_ty {
            Some(ty) => {
                ty.assert_valid();
            }
            _ => {}
        }

        let span = ty
            .as_ref()
            .map(|v| v.span())
            .and_then(|span| if span.is_dummy() { None } else { Some(span) })
            .unwrap_or_else(|| pat.span());
        if !self.is_builtin {
            debug_assert!(!span.is_dummy(), "Cannot declare a variable with a dummy span")
        }

        match &*pat {
            RPat::Ident(i) => {
                let name: Id = Id::from(i.id.clone());
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
                    false,
                )?;

                return Ok(());
            }
            RPat::Assign(ref p) => {
                let type_ann = p.left.get_ty();
                let type_ann = match type_ann {
                    Some(v) => v.validate_with(self).report(&mut self.storage),
                    None => None,
                };

                let right: Option<Type> = p
                    .right
                    .validate_with_args(self, (TypeOfMode::RValue, None, type_ann.as_ref()))
                    .report(&mut self.storage);
                let right = right.unwrap_or_else(|| Type::any(span));

                slog::debug!(
                    self.logger,
                    "({}) declare_vars: Assign({:?}), ty = {:?}",
                    self.scope.depth(),
                    p.left,
                    ty
                );
                self.declare_vars_inner_with_ty(kind, &p.left, ty, actual_ty, Some(right))
                    .report(&mut self.storage);

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
                                .fill_with(|| ty.generalize_lit(marks).generalize_tuple().into());
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
                            match elem {
                                RPat::Rest(elem) => {
                                    // Rest element is special.
                                    let type_for_rest_arg = match ty {
                                        Some(ty) => self
                                            .get_lefting_elements(Some(*span), Cow::Owned(ty), idx)
                                            .context(
                                                "tried to get lefting elements of an iterator to declare variables \
                                                 using a rest pattern",
                                            )
                                            .map(Cow::into_owned)
                                            .report(&mut self.storage),
                                        None => None,
                                    };

                                    let default = match default_ty {
                                        Some(ty) => self
                                            .get_lefting_elements(Some(*span), Cow::Owned(ty), idx)
                                            .context(
                                                "tried to get lefting elements of an iterator to declare variables \
                                                 using a rest pattern",
                                            )
                                            .map(Cow::into_owned)
                                            .report(&mut self.storage),
                                        None => None,
                                    };

                                    self.declare_vars_inner_with_ty(kind, &elem.arg, type_for_rest_arg, None, default)
                                        .context("tried to declare lefting elements to the arugment of a rest pattern")
                                        .report(&mut self.storage);
                                    break;
                                }
                                _ => {}
                            }

                            let elem_ty = match &ty {
                                Some(ty) => self
                                    .access_property(
                                        elem.span(),
                                        &ty,
                                        &Key::Num(RNumber {
                                            span: elem.span(),
                                            value: idx as f64,
                                        }),
                                        TypeOfMode::RValue,
                                        IdCtx::Var,
                                    )
                                    .context("tried to access property to declare variables using an array pattern")
                                    .report(&mut self.storage),
                                None => None,
                            };

                            let default = match &default_ty {
                                Some(ty) => self
                                    .access_property(
                                        elem.span(),
                                        &ty,
                                        &Key::Num(RNumber {
                                            span: elem.span(),
                                            value: idx as f64,
                                        }),
                                        TypeOfMode::RValue,
                                        IdCtx::Var,
                                    )
                                    .context("tried to access property to declare variables using an array pattern")
                                    .report(&mut self.storage),
                                None => None,
                            };

                            // TODO: actual_ty
                            self.declare_vars_inner_with_ty(kind, elem, elem_ty, None, default)
                                .report(&mut self.storage);
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
                // if self.ctx.in_declare {
                //     self.storage.report(Error::DestructuringAssignInAmbientContext { span });
                // }

                let ty = match ty {
                    None => try_opt!(type_ann.as_ref().map(|v| v.type_ann.validate_with(self))),
                    Some(mut ty) => {
                        ty.respan(span);
                        Some(ty)
                    }
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

                let mut used_keys = vec![];

                for prop in props {
                    match prop {
                        RObjectPatProp::Assign(prop) => {
                            let span = prop.span;
                            let mut key = Key::Normal {
                                span: prop.key.span,
                                sym: prop.key.sym.clone(),
                            };
                            used_keys.push(key.clone());

                            let default_prop_ty = default_ty.as_ref().and_then(|obj| {
                                self.access_property(span, obj, &key, TypeOfMode::RValue, IdCtx::Var)
                                    .ok()
                            });

                            let prop_ty = match &ty {
                                Some(ty) => self
                                    .access_property(span, ty, &key, TypeOfMode::RValue, IdCtx::Var)
                                    .convert_err(|err| match err {
                                        Error::NoSuchProperty { span, .. } if prop.value.is_none() => {
                                            Error::NoSuchPropertyWhileDeclWithBidningPat { span }
                                        }
                                        _ => err,
                                    })
                                    .context("tried to access property to declare variables using an object pattern")
                                    .report(&mut self.storage),
                                None => None,
                            };

                            match &prop.value {
                                Some(value) => {
                                    // TODO: Assign this
                                    let type_of_default_value =
                                        value.validate_with_default(self).report(&mut self.storage);

                                    let default = opt_union(span, default_prop_ty, type_of_default_value);

                                    // TODO: actual_ty
                                    self.declare_vars_inner_with_ty(
                                        kind,
                                        &RPat::Ident(RBindingIdent {
                                            node_id: NodeId::invalid(),
                                            id: prop.key.clone(),
                                            type_ann: None,
                                        }),
                                        prop_ty,
                                        None,
                                        default,
                                    )
                                    .context(
                                        "tried to declare a variable from an assignment property in an object pattern",
                                    )
                                    .report(&mut self.storage);
                                }
                                None => {
                                    // TODO: actual_ty
                                    self.declare_vars_inner_with_ty(
                                        kind,
                                        &RPat::Ident(RBindingIdent {
                                            node_id: NodeId::invalid(),
                                            id: prop.key.clone(),
                                            type_ann: None,
                                        }),
                                        prop_ty,
                                        None,
                                        default_prop_ty,
                                    )
                                    .context("tried to declare a variable from a simple property in an object pattern")
                                    .report(&mut self.storage);
                                }
                            }
                        }

                        RObjectPatProp::KeyValue(p) => {
                            let span = p.span();
                            let key = p.key.validate_with(self)?;
                            used_keys.push(key.clone());

                            let prop_ty = match &ty {
                                Some(ty) => self
                                    .access_property(span, &ty, &key, TypeOfMode::RValue, IdCtx::Var)
                                    .map(Some)
                                    .context("tried to access property to declare variables using an object pattern")
                                    .report(&mut self.storage)
                                    .flatten()
                                    .or_else(|| Some(Type::any(span))),
                                None => None,
                            };

                            let default = default_ty.as_ref().and_then(|obj| {
                                self.access_property(span, obj, &key, TypeOfMode::RValue, IdCtx::Var)
                                    .ok()
                            });

                            // TODO: actual_ty
                            self.declare_vars_inner_with_ty(kind, &p.value, prop_ty, None, default)
                                .context("tried to declare a variable from key-value property in an object pattern")
                                .report(&mut self.storage);
                        }

                        RObjectPatProp::Rest(pat) => {
                            let default = default_ty
                                .as_ref()
                                .and_then(|ty| self.exclude_props(span, &ty, &used_keys).ok());

                            match ty {
                                Some(ty) => {
                                    let rest_ty = self
                                        .exclude_props(span, &ty, &used_keys)
                                        .context("tried to exclude keys for declare vars with a object rest pattern")?;

                                    return self
                                        .declare_complex_vars(kind, &pat.arg, rest_ty, None, default)
                                        .context("tried to declare vars with an object rest pattern");
                                }
                                None => {
                                    return self
                                        .declare_vars_inner_with_ty(kind, &pat.arg, None, None, default)
                                        .context("tried to declare vars with an object rest pattern without types");
                                }
                            }
                        }
                    }
                }

                match self.ctx.pat_mode {
                    PatMode::Decl => {
                        // TODO: Report errors for unused properties
                    }
                    _ => {}
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

                self.declare_vars_inner(kind, &arg).report(&mut self.storage);

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
