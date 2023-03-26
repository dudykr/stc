use std::borrow::Cow;

use itertools::Itertools;
use rnode::{FoldWith, NodeId};
use stc_ts_ast_rnode::{RBindingIdent, RExpr, RIdent, RNumber, RObjectPatProp, RPat, RStr, RTsEntityName, RTsLit};
use stc_ts_errors::{
    debug::{dump_type_as_string, force_dump_type_as_string},
    DebugExt, ErrorKind,
};
use stc_ts_type_ops::{tuple_to_array::TupleToArray, widen::Widen, Fix};
use stc_ts_types::{
    type_id::DestructureId, Array, CommonTypeMetadata, Instance, Key, LitType, OptionalType, PropertySignature, Ref, RestType, Tuple,
    TupleElement, TupleMetadata, Type, TypeElement, TypeLit, TypeLitMetadata, TypeParam, TypeParamInstantiation, Union,
};
use stc_ts_utils::{run, PatExt};
use stc_utils::{cache::Freeze, dev_span, TryOpt};
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
    ty::TypeExt,
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

impl Default for DeclareVarsOpts {
    fn default() -> Self {
        Self {
            kind: VarKind::Var(VarDeclKind::Var),
            use_iterator_for_array: false,
        }
    }
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
    ) -> VResult<Option<Type>> {
        if let Some(ty) = &ty {
            ty.assert_valid();
            if !self.config.is_builtin {
                ty.assert_clone_cheap();
            }
        }
        if let Some(ty) = &actual {
            ty.assert_valid();
            if !self.config.is_builtin {
                ty.assert_clone_cheap();
            }
        }
        if let Some(ty) = &default {
            ty.assert_valid();
            if !self.config.is_builtin {
                ty.assert_clone_cheap();
            }
        }

        let span = pat.span().with_ctxt(SyntaxContext::empty());

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

                ty.freeze();

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
                    false,
                )
            }

            RPat::Assign(p) => {
                let type_ann = p.left.get_ty();
                let type_ann: Option<Type> = match type_ann {
                    Some(v) => v.validate_with(self).report(&mut self.storage),
                    None => None,
                };
                let is_typed = type_ann.is_some();
                let mut type_ann = type_ann.or(default);
                type_ann.freeze();

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
                    match &*p.left {
                        RPat::Array(p_left) => {
                            right = right.fold_with(&mut Widen { tuple_to_array: false });
                        }
                        _ => {
                            right = right.fold_with(&mut Widen { tuple_to_array: true });
                        }
                    }
                }

                right.freeze();

                if let Some(type_ann) = &type_ann {
                    self.assign_with_opts(
                        &mut Default::default(),
                        type_ann,
                        &right,
                        AssignOpts {
                            span: p.right.span(),
                            allow_assignment_to_param_constraint: false,
                            ..Default::default()
                        },
                    )
                    .context("tried to assign a value to a variable with an assignment pattern")
                    .report(&mut self.storage);
                }

                let default = if is_typed {
                    type_ann
                } else {
                    opt_union(span, type_ann, Some(right))
                }
                .freezed();

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

                    let ty = ty
                        .map(|ty| {
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
                        })
                        .freezed()
                        .map(Cow::into_owned);

                    let default_ty = default;
                    let default = default_ty
                        .as_ref()
                        .map(|ty| {
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
                        })
                        .freezed()
                        .map(Cow::into_owned);

                    for (idx, elem) in arr.elems.iter().enumerate() {
                        if let Some(elem) = elem {
                            if let RPat::Rest(elem) = elem {
                                // Rest element is special.
                                let type_for_rest_arg = match &ty {
                                    Some(ty) => self
                                        .get_rest_elements(Some(span), Cow::Borrowed(ty), idx)
                                        .context("tried to get left elements of an iterator to declare variables using a rest pattern")
                                        .map(Cow::into_owned)
                                        .report(&mut self.storage),
                                    None => None,
                                }
                                .freezed();

                                let default = match default {
                                    Some(ty) => self
                                        .get_rest_elements(Some(span), Cow::Borrowed(&ty), idx)
                                        .context("tried to get left elements of an iterator to declare variables using a rest pattern")
                                        .map(Cow::into_owned)
                                        .report(&mut self.storage),
                                    None => None,
                                }
                                .freezed();

                                self.add_vars(&elem.arg, type_for_rest_arg, None, default, DeclareVarsOpts { ..opts })
                                    .context("tried to declare left elements to the argument of a rest pattern")
                                    .report(&mut self.storage);
                                break;
                            }

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
                                        Ok(ty) => Ok(ty.into_owned().generalize_lit()),
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
                                                        tracker: Default::default(),
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
                                .map(|ty| ty.generalize_lit())
                                .freezed();

                            // TODO(kdy1): actual_ty

                            self.add_vars(elem, elem_ty, None, default_elem_ty, opts)?;
                        }
                    }
                    // Type inference for functions
                    let default_ty = match default_ty {
                        Some(d_ty) => {
                            let d_ty = d_ty.fold_with(&mut Widen { tuple_to_array: false });
                            let mut left_elems = vec![];

                            for (i, left_element) in arr.elems.iter().enumerate() {
                                if let Some(r_pat) = left_element {
                                    match r_pat {
                                        RPat::Assign(p) => {
                                            let elem_ty = box p
                                                .right
                                                .validate_with_default(self)?
                                                .fold_with(&mut Widen { tuple_to_array: false })
                                                .union_with_undefined(span)
                                                .freezed();

                                            left_elems.push(TupleElement {
                                                span,
                                                label: None,
                                                ty: box Type::Optional(OptionalType {
                                                    span,
                                                    ty: elem_ty,
                                                    metadata: Default::default(),
                                                    tracker: Default::default(),
                                                }),
                                                tracker: Default::default(),
                                            });
                                        }
                                        _ => {
                                            left_elems.push(TupleElement {
                                                span,
                                                label: None,
                                                ty: box Type::Optional(OptionalType {
                                                    span,
                                                    ty: box Type::any(span, Default::default()),
                                                    metadata: Default::default(),
                                                    tracker: Default::default(),
                                                }),
                                                tracker: Default::default(),
                                            });
                                        }
                                    }
                                }
                            }

                            match d_ty {
                                Type::Tuple(mut ty) => {
                                    let right_type_len = ty.elems.len();
                                    let left_type_len = left_elems.len();
                                    let assign_possible = left_type_len > right_type_len;

                                    if assign_possible {
                                        let assign_range = left_type_len - right_type_len;
                                        let start = left_type_len - assign_range;
                                        ty.elems.extend(left_elems.drain(start..start + assign_range));
                                    }

                                    Some(Type::Tuple(ty))
                                }

                                Type::Array(..) => Some(Type::Tuple(Tuple {
                                    span,
                                    elems: left_elems,
                                    metadata: Default::default(),
                                    tracker: Default::default(),
                                })),
                                _ => Some(d_ty),
                            }
                        }
                        None => None,
                    };

                    Ok(ty.or(default_ty))
                } else {
                    let mut elems = vec![];
                    let destructure_key = self.get_destructor_unique_key();

                    let mut has_rest = false;
                    for (idx, elem) in arr.elems.iter().enumerate() {
                        match elem {
                            Some(elem) => {
                                if let RPat::Rest(elem) = elem {
                                    has_rest = true;
                                    // Rest element is special.
                                    let type_for_rest_arg = match &ty {
                                        Some(ty) => self
                                            .get_rest_elements(Some(span), Cow::Borrowed(ty), idx)
                                            .context("tried to get left elements of an iterator to declare variables using a rest pattern")
                                            .map(Cow::into_owned)
                                            .report(&mut self.storage),
                                        None => None,
                                    }
                                    .freezed();

                                    let default = match &default {
                                        Some(ty) => self
                                            .get_rest_elements(Some(span), Cow::Borrowed(ty), idx)
                                            .context("tried to get left elements of an iterator to declare variables using a rest pattern")
                                            .map(Cow::into_owned)
                                            .report(&mut self.storage),
                                        None => None,
                                    }
                                    .freezed();

                                    let rest_ty = self
                                        .add_vars(&elem.arg, type_for_rest_arg, None, default, DeclareVarsOpts { ..opts })
                                        .context("tried to declare left elements to the argument of a rest pattern")
                                        .report(&mut self.storage)
                                        .flatten();

                                    elems.push(TupleElement {
                                        span: elem.span(),
                                        label: Some(*elem.arg.clone()),
                                        ty: box Type::Rest(RestType {
                                            span: elem.span,
                                            ty: box rest_ty.unwrap_or_else(|| Type::any(elem.span, Default::default())),
                                            metadata: Default::default(),
                                            tracker: Default::default(),
                                        })
                                        .freezed(),
                                        tracker: Default::default(),
                                    });

                                    break;
                                }

                                let mut elem_ty = match &ty {
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
                                        .map(|ty| ty.generalize_lit())
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
                                        .map(|ty| ty.generalize_lit())
                                        .context("tried to access property to declare variables using an array pattern")
                                        .report(&mut self.storage),
                                    None => None,
                                }
                                .freezed();

                                if let Some(ty) = &mut elem_ty {
                                    add_destructure_sign(ty, destructure_key);
                                }

                                // TODO(kdy1): actual_ty
                                let elem_ty = self
                                    .add_vars(elem, elem_ty, None, default, opts)
                                    .report(&mut self.storage)
                                    .flatten();

                                elems.push(TupleElement {
                                    span: elem.span(),
                                    label: Some(elem.clone()),
                                    ty: box elem_ty.unwrap_or_else(|| Type::any(elem.span(), Default::default())).freezed(),
                                    tracker: Default::default(),
                                });
                            }
                            // Skip
                            None => {}
                        }
                    }

                    let save_ty = ty.clone().map(|ty| {
                        if let Ok(ty) = self.normalize(Some(span), Cow::Borrowed(&ty), Default::default()) {
                            let mut ty = ty.into_owned();
                            if let Type::Union(Union { types, .. }) = ty.normalize_mut() {
                                'outer: for member in types.iter_mut() {
                                    if let Type::Tuple(tuple) = member.normalize_mut() {
                                        for (idx, (inner, outer)) in tuple.elems.iter_mut().zip(elems.iter()).enumerate() {
                                            if has_rest && elems.len() - 1 == idx {
                                                break 'outer;
                                            }
                                            inner.label = outer.label.clone();
                                        }
                                    }
                                }
                            }
                            return ty;
                        }
                        ty
                    });

                    let mut real_ty = Type::Tuple(Tuple {
                        span,
                        elems,
                        metadata: TupleMetadata {
                            common: CommonTypeMetadata {
                                destructure_key,
                                ..Default::default()
                            },
                            ..Default::default()
                        },
                        tracker: Default::default(),
                    });

                    if let Some(ty) = &ty {
                        let t = ty.normalize_instance();

                        if t.is_array() || (t.is_tuple() && arr.type_ann.is_none() && self.ctx.is_calling_iife) {
                            real_ty = real_ty.fold_with(&mut TupleToArray);
                            real_ty.fix();
                        }
                    }

                    real_ty.freeze();
                    self.regist_destructure(span, save_ty, Some(destructure_key));
                    Ok(Some(real_ty))
                }
            }

            RPat::Object(obj) => {
                let normalize_ty = ty.as_ref().map(Type::normalize);
                let should_use_no_such_property = !matches!(normalize_ty, Some(Type::TypeLit(..)));
                let destructure_key = self.regist_destructure(span, ty.clone(), None);

                let mut real = Type::TypeLit(TypeLit {
                    span,
                    members: vec![],
                    metadata: TypeLitMetadata {
                        common: CommonTypeMetadata {
                            destructure_key,
                            ..Default::default()
                        },
                        ..Default::default()
                    },
                    tracker: Default::default(),
                });

                // TODO(kdy1): Normalize static
                //
                let mut used_keys = vec![];

                for (idx, prop) in obj.props.iter().enumerate() {
                    let is_last = idx == obj.props.len() - 1;

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
                                    .map(|ty| ty.generalize_lit())
                                    .context("tried to access property to declare variables")
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
                                .map(|ty| ty.generalize_lit())
                                .freezed();

                            let real_property_type = match prop_ty {
                                Ok(prop_ty) => {
                                    // TODO(kdy1): actual_ty
                                    self.add_vars(&prop.value, prop_ty.freezed(), None, default_prop_ty, opts)
                                        .report(&mut self.storage)
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
                                        .report(&mut self.storage)
                                }
                            }
                            .flatten()
                            .map(|v| box v);

                            real = self.append_type_element(
                                real,
                                TypeElement::Property(PropertySignature {
                                    span,
                                    accessibility: None,
                                    readonly: false,
                                    key,
                                    optional: true,
                                    params: Vec::new(),
                                    type_ann: real_property_type,
                                    type_params: None,
                                    metadata: Default::default(),
                                    accessor: Default::default(),
                                }),
                            )?;
                        }
                        RObjectPatProp::Assign(prop) => {
                            let key = Key::Normal {
                                span: prop.key.span,
                                sym: prop.key.sym.clone(),
                            };
                            used_keys.push(key.clone());
                            let optional = default.is_some() || prop.value.is_some();

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
                                    .map(|ty| ty.generalize_lit())
                                    .context("tried to access property to declare variables")
                            });

                            let mut default_prop_ty = default
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
                                .map(|ty| ty.generalize_lit())
                                .freezed();

                            let real_property_type = match prop_ty {
                                Ok(mut prop_ty) => {
                                    if let Some(ty) = &mut prop_ty {
                                        add_destructure_sign(ty, destructure_key);
                                    }

                                    let prop_ty = prop_ty.map(Type::freezed);

                                    match &prop.value {
                                        Some(default) => {
                                            let mut default_value_type = default
                                                .validate_with_args(
                                                    self,
                                                    (TypeOfMode::RValue, None, prop_ty.as_ref().or(default_prop_ty.as_ref())),
                                                )
                                                .context("tried to validate default value of an assignment pattern")
                                                .report(&mut self.storage);

                                            if self.ctx.is_fn_param && prop_ty.is_none() {
                                                default_value_type = default_value_type.fold_with(&mut Widen { tuple_to_array: true });
                                            }

                                            default_value_type.freeze();

                                            let mut default = opt_union(span, default_prop_ty, default_value_type).freezed();
                                            // TODO(kdy1): Pass default when it's possible.
                                            if prop_ty.is_some() {
                                                default = None;
                                            }

                                            let result = self
                                                .add_vars(
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

                                            result
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
                                            .report(&mut self.storage)
                                        }
                                    }
                                }
                                Err(err) => {
                                    match &*err {
                                        ErrorKind::NoSuchProperty { span, .. } | ErrorKind::NoSuchPropertyInClass { span, .. }
                                            if !should_use_no_such_property =>
                                        {
                                            if default_prop_ty.is_none() && prop.value.is_none() {
                                                self.storage.report(ErrorKind::NoInitAndNoDefault { span: *span }.into())
                                            }
                                        }
                                        _ => self.storage.report(err),
                                    }

                                    if let Some(ty) = &mut default_prop_ty {
                                        add_destructure_sign(ty, destructure_key);
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
                                    .report(&mut self.storage)
                                }
                            };

                            real = self.append_type_element(
                                real,
                                TypeElement::Property(PropertySignature {
                                    span,
                                    accessibility: None,
                                    readonly: false,
                                    key,
                                    optional,
                                    params: Vec::new(),
                                    type_ann: real_property_type.flatten().map(|v| box v),
                                    type_params: None,
                                    metadata: Default::default(),
                                    accessor: Default::default(),
                                }),
                            )?;
                        }
                        RObjectPatProp::Rest(pat) => {
                            if !is_last {
                                return Err(ErrorKind::RestPropertyNotLast { span: pat.span }.into());
                            }

                            let mut rest_ty = ty
                                .as_ref()
                                .try_map(|ty| {
                                    self.exclude_props(pat.span(), ty, &used_keys)
                                        .context("tried to exclude keys for assignment with a object rest pattern")
                                })?
                                .freezed();

                            let mut default = default
                                .as_ref()
                                .and_then(|ty| self.exclude_props(pat.span(), ty, &used_keys).ok())
                                .freezed();

                            if let Some(ty) = &mut rest_ty {
                                remove_readonly(ty);
                                add_destructure_sign(ty, destructure_key);
                            }

                            if let Some(ty) = &mut default {
                                remove_readonly(ty);
                            }

                            let rest = self
                                .add_vars(&pat.arg, rest_ty, None, default, opts)
                                .context("tried to assign to an object rest pattern")?;

                            if let Some(rest) = rest {
                                real.freeze();
                                real = self.append_type(span, real, rest, Default::default())?;
                            }
                            break;
                        }
                    }
                }

                Ok(Some(real))
            }

            RPat::Rest(pat) => {
                let ty = ty.map(|ty| self.ensure_iterable(span, ty)).transpose()?;
                let actual = actual.map(|ty| self.ensure_iterable(span, ty)).transpose()?;
                let default = default.map(|ty| self.ensure_iterable(span, ty)).transpose()?;

                self.add_vars(&pat.arg, ty, actual, default, DeclareVarsOpts { ..opts })
            }
            RPat::Invalid(..) | RPat::Expr(box RExpr::Invalid(..)) => Ok(None),

            _ => {
                unimplemented!("declare_vars({:#?}, {:#?})", pat, ty)
            }
        }
    }

    pub(crate) fn exclude_props(&mut self, span: Span, ty: &Type, keys: &[Key]) -> VResult<Type> {
        let _tracing = dev_span!("exclude_props");

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
            ty.freeze();

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
                        tracker: Default::default(),
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
                        tracker: Default::default(),
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
                        tracker: Default::default(),
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
                                tracker: Default::default(),
                            })),
                            Key::Num(v) => Some(Type::Lit(LitType {
                                span: v.span.with_ctxt(SyntaxContext::empty()),
                                lit: RTsLit::Number(v.clone()),
                                metadata: Default::default(),
                                tracker: Default::default(),
                            })),
                            Key::Normal { span, sym } => Some(Type::Lit(LitType {
                                span: span.with_ctxt(SyntaxContext::empty()),
                                lit: RTsLit::Str(RStr {
                                    span: *span,
                                    value: sym.clone(),
                                    raw: None,
                                }),
                                metadata: Default::default(),
                                tracker: Default::default(),
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
                        tracker: Default::default(),
                    });

                    return Ok(Type::Ref(Ref {
                        span,
                        type_name: RTsEntityName::Ident(RIdent::new("Omit".into(), DUMMY_SP)),
                        type_args: Some(box TypeParamInstantiation {
                            span,
                            params: vec![ty.clone().into_owned(), keys],
                        }),
                        metadata: Default::default(),
                        tracker: Default::default(),
                    }));
                }
                _ => {}
            }

            Err(ErrorKind::Unimplemented {
                span,
                msg: format!("exclude_props: {}", force_dump_type_as_string(&ty)),
            }
            .into())
        })()?;

        Ok(ty.fixed())
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
                tracker: Default::default(),
            })
            .freezed())
        })
        .context("tried to ensure iterator")
    }

    pub fn regist_destructure(&mut self, span: Span, ty: Option<Type>, des_key: Option<DestructureId>) -> DestructureId {
        match ty.as_ref().map(Type::normalize) {
            Some(real @ Type::Union(..)) => {
                let des_key = des_key.unwrap_or_else(|| self.get_destructor_unique_key());
                let destructure_key = des_key;
                if let Ok(result) = self.declare_destructor(span, real, des_key) {
                    if result {
                        return destructure_key;
                    }
                }
            }
            Some(Type::Param(TypeParam {
                constraint: Some(box result),
                ..
            })) => {
                if let Ok(result) = self.normalize(Some(span), Cow::Borrowed(result), Default::default()) {
                    return self.regist_destructure(span, Some(result.into_owned()), des_key);
                }
            }

            Some(Type::Instance(Instance { ty: box result, .. })) => {
                if let Ok(result) = self.normalize(Some(span), Cow::Borrowed(result), Default::default()) {
                    return self.regist_destructure(span, Some(result.into_owned()), des_key);
                }
            }

            Some(Type::Tuple(Tuple { elems, .. })) => {
                if elems.len() == 1 {
                    if let Some(TupleElement { ty: box ty, .. }) = elems.first() {
                        return self.regist_destructure(span, Some(ty.clone()), des_key);
                    }
                }
            }

            Some(Type::Rest(RestType { ty: box ty, .. })) => {
                return self.regist_destructure(span, Some(ty.clone()), des_key);
            }
            _ => {}
        }
        DestructureId(0)
    }
}

fn remove_readonly(ty: &mut Type) {
    if let Some(tl) = ty.as_type_lit_mut() {
        for m in &mut tl.members {
            if let TypeElement::Property(p) = m {
                p.readonly = false;
            }
        }

        ty.freeze();
    }
}

fn add_destructure_sign(ty: &mut Type, key: DestructureId) {
    ty.metadata_mut().destructure_key = key;
    ty.freeze();
}
