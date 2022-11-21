use std::borrow::Cow;

use itertools::Itertools;
use rnode::{FoldWith, NodeId};
use stc_ts_ast_rnode::{RBindingIdent, RExpr, RIdent, RNumber, RObjectPatProp, RPat, RStr, RTsEntityName, RTsLit};
use stc_ts_errors::{ctx, debug::dump_type_as_string, DebugExt, ErrorKind};
use stc_ts_type_ops::{widen::Widen, Fix};
use stc_ts_types::{Array, Key, KeywordType, LitType, Ref, Tuple, Type, TypeLit, TypeParamInstantiation, Union};
use stc_ts_utils::{run, PatExt};
use stc_utils::{cache::Freeze, TryOpt};
use swc_common::{Span, Spanned, SyntaxContext, DUMMY_SP};
use swc_ecma_ast::{TsKeywordTypeKind, VarDeclKind};
use tracing::debug;

use crate::{
    analyzer::{
        assign::AssignOpts,
        expr::{AccessPropertyOpts, GetIteratorOpts, IdCtx, TypeOfMode},
        types::NormalizeTypeOpts,
        util::{opt_union, ResultExt},
        Analyzer, Ctx,
    },
    validator::ValidateWith,
    VResult,
};

/// The kind of binding.
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum VarKind {
    Var(VarDeclKind),
    /// Function parameters.
    Param,
    Class,
    /// [stc_ts_ast_rnode::RFnDecl]
    Fn,
    Import,
    Enum,
    Error,
}

/// All bool fields default to `false`.
#[derive(Debug, Clone, Copy)]
pub(crate) struct DeclareVarsOpts {
    pub kind: VarKind,
    pub use_iterator_for_array: bool,
}

impl Analyzer<'_, '_> {
    /// TODO(kdy1): Rename to declare_vars
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
    ) -> VResult<()> {
        let _ctx = ctx!("add_vars");

        if let Some(ty) = &ty {
            ty.assert_valid();
            if !self.is_builtin {
                ty.assert_clone_cheap();
            }
        }
        if let Some(ty) = &actual {
            ty.assert_valid();
            if !self.is_builtin {
                ty.assert_clone_cheap();
            }
        }
        if let Some(ty) = &default {
            ty.assert_valid();
            if !self.is_builtin {
                ty.assert_clone_cheap();
            }
        }

        let span = pat.span().with_ctxt(SyntaxContext::empty());

        if !matches!(pat, RPat::Ident(..)) {
            if let Some(ty @ Type::Ref(..)) = ty.as_ref().map(Type::normalize) {
                let mut ty = self
                    .expand_top_ref(ty.span(), Cow::Borrowed(ty), Default::default())
                    .context("tried to expand reference to declare a complex variable")?
                    .into_owned();

                ty.make_clone_cheap();

                return self.add_vars(pat, Some(ty), actual, default, opts);
            }
        }

        match pat {
            RPat::Ident(i) => {
                if let Some(ty) = &ty {
                    if cfg!(debug_assertions) {
                        debug!("[vars]: Declaring {} as {}", i.id.sym, dump_type_as_string(ty));
                    }
                } else {
                    if cfg!(debug_assertions) {
                        debug!("[vars]: Declaring {} without type", i.id.sym);
                    }
                }

                let mut ty = match (default, ty) {
                    (Some(default), Some(ty)) => {
                        if let Some(true) = self.extends(span, &default, &ty, Default::default()) {
                            Some(ty)
                        } else {
                            opt_union(span, Some(ty), Some(default))
                        }
                    }
                    (default, ty) => opt_union(span, ty, default),
                };

                ty.make_clone_cheap();

                if let Some(ty) = &ty {
                    if let Some(m) = &mut self.mutations {
                        m.for_pats.entry(i.node_id).or_default().ty = Some(ty.clone());
                    }
                }

                self.declare_var(
                    span,
                    opts.kind,
                    i.id.clone().into(),
                    ty,
                    actual,
                    // initialized
                    true,
                    // let/const declarations does not allow multiple declarations with
                    // same name
                    opts.kind == VarKind::Var(VarDeclKind::Var),
                    false,
                )?;
                Ok(())
            }

            RPat::Assign(p) => {
                let type_ann = p.left.get_ty();
                let type_ann: Option<Type> = match type_ann {
                    Some(v) => v.validate_with(self).report(&mut self.storage),
                    None => None,
                };
                let is_typed = type_ann.is_some();
                let mut type_ann = type_ann.or(default);
                type_ann.make_clone_cheap();

                let mut right = p
                    .right
                    .validate_with_args(self, (TypeOfMode::RValue, None, type_ann.as_ref().or(ty.as_ref())))
                    .report(&mut self.storage)
                    .unwrap_or_else(|| Type::any(span, Default::default()));

                if self.ctx.is_fn_param && type_ann.is_none() {
                    // If the declaration includes an initializer expression (which is permitted
                    // only when the parameter list occurs in conjunction with a
                    // function body), the parameter type is the widened form (section
                    // 3.11) of the type of the initializer expression.

                    right = right.fold_with(&mut Widen { tuple_to_array: true });
                }

                right.make_clone_cheap();

                if let Some(left) = &type_ann {
                    self.assign_with_opts(
                        &mut Default::default(),
                        left,
                        &right,
                        AssignOpts {
                            span: p.right.span(),
                            allow_assignment_to_param_constraint: false,
                            ..Default::default()
                        },
                    )
                    .report(&mut self.storage);
                }

                let default = if is_typed {
                    type_ann
                } else {
                    opt_union(span, type_ann, Some(right))
                };

                self.add_vars(
                    &p.left,
                    ty,
                    actual,
                    default,
                    DeclareVarsOpts {
                        use_iterator_for_array: true,
                        ..opts
                    },
                )
                .context("tried to declare a variable with an assignment pattern")
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
                            Cow::Owned(Type::any(span, Default::default()))
                        })
                    });

                    let default_ty = default;
                    let default = default_ty.as_ref().map(|ty| {
                        self.get_iterator(
                            span,
                            Cow::Borrowed(ty),
                            GetIteratorOpts {
                                disallow_str: true,
                                ..Default::default()
                            },
                        )
                        .context("tried to convert a type to an iterator to assign with an array pattern (default value)")
                        .unwrap_or_else(|err| {
                            self.storage.report(err);
                            Cow::Owned(Type::any(span, Default::default()))
                        })
                    });

                    for (idx, elem) in arr.elems.iter().enumerate() {
                        if let Some(elem) = elem {
                            let elem_ty = ty
                                .as_ref()
                                .try_map(|ty| -> VResult<Type> {
                                    let result = self.get_element_from_iterator(span, Cow::Borrowed(ty), idx).with_context(|| {
                                        format!(
                                            "tried to get the type of {}th element from iterator to declare vars with an array pattern",
                                            idx
                                        )
                                    });

                                    match result {
                                        Ok(ty) => Ok(ty.into_owned()),
                                        Err(err) => match &*err {
                                            ErrorKind::TupleIndexError { .. } => match elem {
                                                RPat::Assign(p) => {
                                                    let type_ann = p.left.get_ty();
                                                    let type_ann: Option<Type> =
                                                        type_ann.and_then(|v| v.validate_with(self).report(&mut self.storage));
                                                    let type_ann = type_ann.or_else(|| default_ty.clone());

                                                    let right = p
                                                        .right
                                                        .validate_with_args(
                                                            self,
                                                            (TypeOfMode::RValue, None, type_ann.as_ref().or(Some(ty))),
                                                        )
                                                        .report(&mut self.storage)
                                                        .unwrap_or_else(|| Type::any(span, Default::default()));

                                                    Ok(right)
                                                }
                                                RPat::Rest(p) => {
                                                    // [a, ...b] = [1]
                                                    // b should be an empty tuple
                                                    Ok(Type::Tuple(Tuple {
                                                        span: p.span,
                                                        elems: vec![],
                                                        metadata: Default::default(),
                                                    }))
                                                }
                                                _ => Err(err),
                                            },
                                            _ => Err(err),
                                        },
                                    }
                                })?
                                .freezed();

                            let default_elem_ty = default
                                .as_ref()
                                .and_then(|ty| {
                                    self.get_element_from_iterator(span, Cow::Borrowed(ty), idx)
                                        .with_context(|| {
                                            format!(
                                                "tried to get the type of {}th element from iterator to declare vars with an array \
                                                 pattern (default value)",
                                                idx
                                            )
                                        })
                                        .ok()
                                })
                                .map(Cow::into_owned)
                                .freezed();

                            // TODO(kdy1): actual_ty
                            self.add_vars(elem, elem_ty, None, default_elem_ty, opts)?;
                        }
                    }

                    Ok(())
                } else {
                    for (idx, elem) in arr.elems.iter().enumerate() {
                        match elem {
                            Some(elem) => {
                                if let RPat::Rest(elem) = elem {
                                    // Rest element is special.
                                    let type_for_rest_arg = match ty {
                                        Some(ty) => self
                                            .get_rest_elements(Some(span), Cow::Owned(ty), idx)
                                            .context(
                                                "tried to get lefting elements of an iterator to declare variables using a rest pattern",
                                            )
                                            .map(Cow::into_owned)
                                            .report(&mut self.storage),
                                        None => None,
                                    }
                                    .freezed();

                                    let default = match default {
                                        Some(ty) => self
                                            .get_rest_elements(Some(span), Cow::Owned(ty), idx)
                                            .context(
                                                "tried to get lefting elements of an iterator to declare variables using a rest pattern",
                                            )
                                            .map(Cow::into_owned)
                                            .report(&mut self.storage),
                                        None => None,
                                    }
                                    .freezed();

                                    self.add_vars(&elem.arg, type_for_rest_arg, None, default, opts)
                                        .context("tried to declare lefting elements to the arugment of a rest pattern")
                                        .report(&mut self.storage);
                                    break;
                                }

                                let elem_ty = match &ty {
                                    Some(ty) => self
                                        .access_property(
                                            elem.span(),
                                            ty,
                                            &Key::Num(RNumber {
                                                span: elem.span(),
                                                value: idx as f64,
                                                raw: None,
                                            }),
                                            TypeOfMode::RValue,
                                            IdCtx::Var,
                                            Default::default(),
                                        )
                                        .context("tried to access property to declare variables using an array pattern")
                                        .report(&mut self.storage),
                                    None => None,
                                }
                                .freezed();

                                let default = match &default {
                                    Some(ty) => self
                                        .access_property(
                                            elem.span(),
                                            ty,
                                            &Key::Num(RNumber {
                                                span: elem.span(),
                                                value: idx as f64,
                                                raw: None,
                                            }),
                                            TypeOfMode::RValue,
                                            IdCtx::Var,
                                            Default::default(),
                                        )
                                        .context("tried to access property to declare variables using an array pattern")
                                        .report(&mut self.storage),
                                    None => None,
                                }
                                .freezed();

                                // TODO(kdy1): actual_ty
                                self.add_vars(elem, elem_ty, None, default, opts).report(&mut self.storage);
                            }
                            // Skip
                            None => {}
                        }
                    }

                    Ok(())
                }
            }

            RPat::Object(obj) => {
                let should_use_no_such_property = !matches!(ty.as_ref().map(Type::normalize), Some(Type::TypeLit(..)));

                // TODO(kdy1): Normalize static
                //
                let mut used_keys = vec![];

                for prop in &obj.props {
                    match prop {
                        RObjectPatProp::KeyValue(prop) => {
                            let key = prop.key.validate_with(self)?;
                            used_keys.push(key.clone());

                            let ctx = Ctx {
                                disallow_unknown_object_property: true,
                                ..self.ctx
                            };
                            let prop_ty = ty.as_ref().try_map(|ty| {
                                self.with_ctx(ctx)
                                    .access_property(
                                        span,
                                        ty,
                                        &key,
                                        TypeOfMode::RValue,
                                        IdCtx::Var,
                                        AccessPropertyOpts {
                                            disallow_indexing_array_with_string: true,
                                            disallow_creating_indexed_type_from_ty_els: true,
                                            disallow_inexact: true,
                                            ..Default::default()
                                        },
                                    )
                                    .context("tried to access poprerty to declare variables")
                            });

                            let default_prop_ty = default
                                .as_ref()
                                .and_then(|ty| {
                                    self.with_ctx(ctx)
                                        .access_property(
                                            span,
                                            ty,
                                            &key,
                                            TypeOfMode::RValue,
                                            IdCtx::Var,
                                            AccessPropertyOpts {
                                                disallow_indexing_array_with_string: true,
                                                disallow_creating_indexed_type_from_ty_els: true,
                                                disallow_inexact: true,
                                                ..Default::default()
                                            },
                                        )
                                        .ok()
                                })
                                .freezed();

                            match prop_ty {
                                Ok(prop_ty) => {
                                    // TODO(kdy1): actual_ty
                                    self.add_vars(&prop.value, prop_ty.freezed(), None, default_prop_ty, opts)
                                        .report(&mut self.storage);
                                }

                                Err(err) => {
                                    match &*err {
                                        ErrorKind::NoSuchProperty { span, .. } | ErrorKind::NoSuchPropertyInClass { span, .. }
                                            if !should_use_no_such_property =>
                                        {
                                            if default_prop_ty.is_none() {
                                                self.storage.report(ErrorKind::NoInitAndNoDefault { span: *span }.into())
                                            }
                                        }
                                        _ => self.storage.report(err),
                                    }

                                    self.add_vars(&prop.value, None, None, default_prop_ty, opts)
                                        .report(&mut self.storage);
                                }
                            }
                        }
                        RObjectPatProp::Assign(prop) => {
                            let key = Key::Normal {
                                span: prop.key.span,
                                sym: prop.key.sym.clone(),
                            };
                            used_keys.push(key.clone());

                            let ctx = Ctx {
                                disallow_unknown_object_property: true,
                                ..self.ctx
                            };

                            let mut prop_ty = ty.as_ref().try_map(|ty| {
                                self.with_ctx(ctx)
                                    .access_property(
                                        span,
                                        ty,
                                        &key,
                                        TypeOfMode::RValue,
                                        IdCtx::Var,
                                        AccessPropertyOpts {
                                            disallow_indexing_array_with_string: true,
                                            disallow_creating_indexed_type_from_ty_els: true,
                                            disallow_inexact: true,
                                            ..Default::default()
                                        },
                                    )
                                    .context("tried to access poprerty to declare variables")
                            });

                            let default_prop_ty = default.as_ref().and_then(|ty| {
                                self.with_ctx(ctx)
                                    .access_property(
                                        span,
                                        ty,
                                        &key,
                                        TypeOfMode::RValue,
                                        IdCtx::Var,
                                        AccessPropertyOpts {
                                            disallow_indexing_array_with_string: true,
                                            disallow_creating_indexed_type_from_ty_els: true,
                                            disallow_inexact: true,
                                            ..Default::default()
                                        },
                                    )
                                    .ok()
                            });

                            if prop.value.is_some() {
                                prop_ty = prop_ty.map(|ty| {
                                    // Optional
                                    ty.map(|ty| {
                                        Type::new_union(
                                            span,
                                            vec![
                                                ty,
                                                Type::Keyword(KeywordType {
                                                    span,
                                                    kind: TsKeywordTypeKind::TsUndefinedKeyword,
                                                    metadata: Default::default(),
                                                }),
                                            ],
                                        )
                                    })
                                });
                            }

                            match prop_ty {
                                Ok(prop_ty) => {
                                    let prop_ty = prop_ty.map(Type::freezed);

                                    match &prop.value {
                                        Some(default) => {
                                            let default_value_type = default
                                                .validate_with_args(
                                                    self,
                                                    (TypeOfMode::RValue, None, prop_ty.as_ref().or(default_prop_ty.as_ref())),
                                                )
                                                .context("tried to validate default value of an assignment pattern")
                                                .report(&mut self.storage)
                                                .freezed();

                                            let default = opt_union(span, default_prop_ty, default_value_type).freezed();

                                            self.add_vars(
                                                &RPat::Ident(RBindingIdent {
                                                    node_id: NodeId::invalid(),
                                                    id: prop.key.clone(),
                                                    type_ann: None,
                                                }),
                                                prop_ty.clone(),
                                                None,
                                                default,
                                                opts,
                                            )
                                            .report(&mut self.storage);

                                            if let Some(prop_ty) = &prop_ty {
                                                self.try_assign_pat(
                                                    span,
                                                    &RPat::Ident(RBindingIdent {
                                                        node_id: NodeId::invalid(),
                                                        id: prop.key.clone(),
                                                        type_ann: None,
                                                    }),
                                                    prop_ty,
                                                )
                                                .context("tried to assign default values")
                                                .report(&mut self.storage);
                                            }
                                        }
                                        None => {
                                            // TODO(kdy1): actual_ty
                                            self.add_vars(
                                                &RPat::Ident(RBindingIdent {
                                                    node_id: NodeId::invalid(),
                                                    id: prop.key.clone(),
                                                    type_ann: None,
                                                }),
                                                prop_ty,
                                                None,
                                                default_prop_ty,
                                                opts,
                                            )
                                            .report(&mut self.storage);
                                        }
                                    }
                                }
                                Err(err) => {
                                    match &*err {
                                        ErrorKind::NoSuchProperty { span, .. } | ErrorKind::NoSuchPropertyInClass { span, .. }
                                            if !should_use_no_such_property =>
                                        {
                                            if default_prop_ty.is_none() {
                                                self.storage.report(ErrorKind::NoInitAndNoDefault { span: *span }.into())
                                            }
                                        }
                                        _ => self.storage.report(err),
                                    }

                                    self.add_vars(
                                        &RPat::Ident(RBindingIdent {
                                            node_id: NodeId::invalid(),
                                            id: prop.key.clone(),
                                            type_ann: None,
                                        }),
                                        None,
                                        None,
                                        default_prop_ty,
                                        opts,
                                    )
                                    .report(&mut self.storage);
                                }
                            }
                        }
                        RObjectPatProp::Rest(pat) => {
                            let rest_ty = ty
                                .as_ref()
                                .try_map(|ty| {
                                    self.exclude_props(pat.span(), ty, &used_keys)
                                        .context("tried to exclude keys for assignment with a object rest pattern")
                                })?
                                .freezed();

                            let default = default
                                .and_then(|ty| self.exclude_props(pat.span(), &ty, &used_keys).ok())
                                .freezed();

                            return self
                                .add_vars(&pat.arg, rest_ty, None, default, opts)
                                .context("tried to assign to an object rest pattern");
                        }
                    }
                }

                Ok(())
            }

            RPat::Rest(pat) => {
                let ty = ty.map(|ty| self.ensure_iterable(span, ty)).transpose()?;
                let actual = actual.map(|ty| self.ensure_iterable(span, ty)).transpose()?;
                let default = default.map(|ty| self.ensure_iterable(span, ty)).transpose()?;

                self.add_vars(&pat.arg, ty, actual, default, opts)
            }

            _ => {
                unimplemented!("declare_vars({:#?}, {:#?})", pat, ty)
            }
        }
    }

    #[cfg_attr(debug_assertions, tracing::instrument(skip_all))]
    pub(crate) fn exclude_props(&mut self, span: Span, ty: &Type, keys: &[Key]) -> VResult<Type> {
        let span = span.with_ctxt(SyntaxContext::empty());

        let ty = (|| -> VResult<_> {
            let mut ty = self.normalize(
                Some(span),
                Cow::Borrowed(ty),
                NormalizeTypeOpts {
                    preserve_mapped: false,
                    ..Default::default()
                },
            )?;
            ty.make_clone_cheap();

            if ty.is_any() || ty.is_unknown() || ty.is_kwd(TsKeywordTypeKind::TsObjectKeyword) {
                return Ok(ty.into_owned());
            }

            match ty.normalize() {
                Type::TypeLit(lit) => {
                    let mut new_members = vec![];
                    'outer: for m in &lit.members {
                        if let Some(key) = m.key() {
                            for prop in keys {
                                if self.key_matches(span, key, prop, false) {
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

                    return Ok(Type::Union(Union {
                        span: u.span,
                        types,
                        metadata: u.metadata,
                    }));
                }

                Type::Intersection(..) | Type::Class(..) | Type::Interface(..) | Type::ClassDef(..) => {
                    let ty = self
                        .convert_type_to_type_lit(ty.span(), Cow::Borrowed(&ty))?
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
                    let key_types = keys
                        .iter()
                        .filter_map(|key| match key {
                            Key::BigInt(v) => Some(Type::Lit(LitType {
                                span: v.span.with_ctxt(SyntaxContext::empty()),
                                lit: RTsLit::BigInt(v.clone()),
                                metadata: Default::default(),
                            })),
                            Key::Num(v) => Some(Type::Lit(LitType {
                                span: v.span.with_ctxt(SyntaxContext::empty()),
                                lit: RTsLit::Number(v.clone()),
                                metadata: Default::default(),
                            })),
                            Key::Normal { span, sym } => Some(Type::Lit(LitType {
                                span: span.with_ctxt(SyntaxContext::empty()),
                                lit: RTsLit::Str(RStr {
                                    span: *span,
                                    value: sym.clone(),
                                    raw: None,
                                }),
                                metadata: Default::default(),
                            })),

                            // TODO
                            _ => None,
                        })
                        .collect_vec();
                    if key_types.is_empty() {
                        return Ok(ty.into_owned());
                    }
                    let keys = Type::Union(Union {
                        span,
                        types: key_types,
                        metadata: Default::default(),
                    });

                    return Ok(Type::Ref(Ref {
                        span,
                        type_name: RTsEntityName::Ident(RIdent::new("Omit".into(), DUMMY_SP)),
                        type_args: Some(box TypeParamInstantiation {
                            span,
                            params: vec![ty.clone().into_owned(), keys],
                        }),
                        metadata: Default::default(),
                    }));
                }
                _ => {}
            }

            unimplemented!("exclude_props: {}", dump_type_as_string(&ty))
        })()?;

        Ok(ty.fixed())
    }

    /// TODO(kdy1): Remove this. All logics are merged into add_vars.
    #[cfg_attr(debug_assertions, tracing::instrument(skip_all))]
    pub(super) fn declare_vars_inner_with_ty(
        &mut self,
        kind: VarKind,
        pat: &RPat,
        ty: Option<Type>,
        actual_ty: Option<Type>,
        default_ty: Option<Type>,
    ) -> VResult<()> {
        let marks = self.marks();

        let span = ty
            .as_ref()
            .map(|v| v.span())
            .and_then(|span| if span.is_dummy() { None } else { Some(span) })
            .unwrap_or_else(|| pat.span());
        if !self.is_builtin {
            debug_assert!(!span.is_dummy(), "Cannot declare a variable with a dummy span")
        }

        match pat {
            RPat::Ident(..) | RPat::Assign(..) | RPat::Array(..) | RPat::Object(..) | RPat::Rest(..) => self.add_vars(
                pat,
                ty,
                actual_ty,
                default_ty,
                DeclareVarsOpts {
                    kind,
                    use_iterator_for_array: false,
                },
            ),

            RPat::Invalid(..) | RPat::Expr(box RExpr::Invalid(..)) => Ok(()),

            _ => unimplemented!("declare_vars for patterns other than ident: {:#?}", pat),
        }
    }

    fn ensure_iterable(&mut self, span: Span, ty: Type) -> VResult<Type> {
        run(|| {
            if let Ok(..) = self.get_iterator(
                span,
                Cow::Borrowed(&ty),
                GetIteratorOpts {
                    disallow_str: true,
                    ..Default::default()
                },
            ) {
                return Ok(ty.freezed());
            }

            Ok(Type::Array(Array {
                span,
                elem_type: box ty,
                metadata: Default::default(),
            })
            .freezed())
        })
        .context("tried to ensure iterator")
    }
}
