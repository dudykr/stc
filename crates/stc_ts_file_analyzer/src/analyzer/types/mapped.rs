use std::{borrow::Cow, collections::HashMap};

use rnode::{NodeId, Visit, VisitMut, VisitMutWith, VisitWith};
use stc_ts_ast_rnode::{RBindingIdent, RIdent, RNumber, RPat, RTsEnumMemberId, RTsLit};
use stc_ts_base_type_ops::apply_mapped_flags;
use stc_ts_errors::{
    debug::{dump_type_as_string, force_dump_type_as_string},
    DebugExt,
};
use stc_ts_generics::type_param::finder::TypeParamNameUsageFinder;
use stc_ts_types::{
    replace::replace_type, Array, Conditional, FnParam, Id, Index, IndexSignature, IndexedAccessType, Key, KeywordType, LitType, Mapped,
    PropertySignature, Readonly, RestType, Tuple, TupleElement, Type, TypeElement, TypeLit, TypeParam,
};
use stc_utils::{
    cache::{Freeze, ALLOW_DEEP_CLONE},
    dev_span, stack,
};
use swc_common::{Span, Spanned, SyntaxContext, TypeEq};
use swc_ecma_ast::{TruePlusMinus, TsKeywordTypeKind};
use tracing::{debug, error};

use crate::{
    analyzer::{types::NormalizeTypeOpts, Analyzer},
    VResult,
};

impl Analyzer<'_, '_> {
    /// Required because mapped type can specified by user, like

    ///
    /// ```ts
    /// declare const a: Partial<Foo>;
    /// ```
    ///
    ///
    /// TODO(kdy1): Handle index signatures.
    pub(crate) fn expand_mapped(&mut self, span: Span, m: &Mapped) -> VResult<Option<Type>> {
        let _guard = stack::track(span)?;
        let _tracing = dev_span!("expand_mapped");

        let orig = dump_type_as_string(&ALLOW_DEEP_CLONE.set(&(), || Type::Mapped(m.clone())));

        let ty = self.expand_mapped_inner(span, m)?;

        if let Some(ty) = &ty {
            let expanded = dump_type_as_string(ty);

            debug!("[types/mapped]: Expanded {} as {}", orig, expanded);
        } else {
            debug!("[types/mapped]: Cannot expand\n{}", orig);
        }

        Ok(ty)
    }

    fn expand_mapped_inner(&mut self, span: Span, m: &Mapped) -> VResult<Option<Type>> {
        match m.type_param.constraint.as_deref().map(|v| v.normalize()) {
            Some(Type::Index(Index { ty: keyof_operand, .. })) => {
                return self.expand_mapped_type_with_keyof(span, keyof_operand, keyof_operand, m)
            }
            _ => {
                if let Some(constraint) = m.type_param.constraint.as_deref() {
                    if constraint.is_kwd(TsKeywordTypeKind::TsStringKeyword) || constraint.is_kwd(TsKeywordTypeKind::TsNumberKeyword) {
                        let index_signature = TypeElement::Index(IndexSignature {
                            params: vec![FnParam {
                                span,
                                required: true,
                                pat: RPat::Ident(RBindingIdent {
                                    node_id: NodeId::invalid(),
                                    id: RIdent::new("___mapped".into(), span.with_ctxt(SyntaxContext::empty())),
                                    type_ann: None,
                                }),
                                ty: box constraint.clone(),
                            }],
                            type_ann: m.ty.clone(),
                            readonly: m.readonly.map_or(false, |v| match v {
                                TruePlusMinus::True => true,
                                TruePlusMinus::Plus => true,
                                TruePlusMinus::Minus => false,
                            }),
                            span: m.span,
                            is_static: false,
                        });
                        return Ok(Some(Type::TypeLit(TypeLit {
                            span: m.span,
                            members: vec![index_signature],
                            metadata: Default::default(),
                            tracker: Default::default(),
                        })));
                    }

                    if let Some(keys) = self.convert_type_to_keys_for_mapped_type(span, constraint, m.name_type.as_deref())? {
                        let members = keys
                            .into_iter()
                            .map(|key| -> VResult<_> {
                                let ty = match &m.ty {
                                    Some(mapped_ty) => self
                                        .expand_key_in_mapped(m.type_param.name.clone(), mapped_ty, &key)
                                        .map(Box::new)
                                        .map(Some)?,
                                    None => None,
                                };
                                let p = PropertySignature {
                                    span: key.span(),
                                    accessibility: None,
                                    readonly: false,
                                    key,
                                    optional: false,
                                    params: Default::default(),
                                    type_ann: ty,
                                    type_params: Default::default(),
                                    metadata: Default::default(),
                                    accessor: Default::default(),
                                };
                                let mut el = TypeElement::Property(p);
                                apply_mapped_flags(&mut el, m.optional, m.readonly);

                                Ok(el)
                            })
                            .collect::<Result<_, _>>()?;

                        return Ok(Some(Type::TypeLit(TypeLit {
                            span: m.span,
                            members,
                            metadata: Default::default(),
                            tracker: Default::default(),
                        })));
                    }
                }
            }
        }

        Ok(None)
    }

    fn expand_mapped_type_with_keyof(
        &mut self,
        span: Span,
        keyof_operand: &Type,
        original_keyof_operand: &Type,
        m: &Mapped,
    ) -> VResult<Option<Type>> {
        let _tracing = dev_span!("expand_mapped_type_with_keyof");

        let keyof_operand = self
            .normalize(Some(span), Cow::Borrowed(keyof_operand), Default::default())
            .context("tried to normalize the operand of `in keyof`")?;

        if let Some(mapped_ty) = &m.ty {
            // Special case, but many usages can be handled with this check.
            if (*keyof_operand).type_eq(mapped_ty) {
                let new_type = self
                    .convert_type_to_type_lit(span, Cow::Borrowed(&keyof_operand))
                    .context("tried to convert a type to type literal to expand mapped type")?
                    .map(Cow::into_owned);

                if let Some(mut new) = new_type {
                    for member in &mut new.members {
                        apply_mapped_flags(member, m.optional, m.readonly);
                    }

                    return Ok(Some(Type::TypeLit(new)));
                }
            }
        }

        match keyof_operand.normalize() {
            Type::Array(array) => {
                let elem_type = m.ty.clone().unwrap_or_else(|| box Type::any(span, Default::default()));
                let elem_type = match m.optional {
                    Some(TruePlusMinus::True) => box elem_type.union_with_undefined(span),
                    _ => elem_type,
                };

                let mut ty = Type::Array(Array {
                    span,
                    elem_type,
                    metadata: array.metadata,
                    tracker: Default::default(),
                });

                replace_type(
                    &mut ty,
                    |ty| {
                        // Check for indexed access type
                        if let Type::IndexedAccessType(iat) = ty.normalize() {
                            if iat.obj_type.as_ref().type_eq(original_keyof_operand) {
                                if let Type::Param(index_type) = iat.index_type.normalize() {
                                    return index_type.name == m.type_param.name;
                                }
                            }
                        }

                        false
                    },
                    |_| Some(*array.elem_type.clone()),
                );

                return Ok(Some(ty));
            }
            Type::Tuple(tuple) => {
                // type ToArray<T> = { [P in keyof T]: T[P][] };
                // type F<T extends unknown[]> = ToArray<[string, number, ...T]>

                // =>

                // type F<T extends unknown[]> = [string[], number[], ...ToArray<T>]

                let ty = Type::Tuple(Tuple {
                    span,
                    elems: tuple
                        .elems
                        .iter()
                        .enumerate()
                        .map(|(idx, elem)| {
                            let mut ty = m.ty.clone().unwrap_or_else(|| box Type::any(span, Default::default()));

                            if let Type::Rest(elem_rest_ty) = elem.ty.normalize() {
                                let mut mapped_ty = m.ty.clone();
                                // Replace `T` with `N` in mapped_ty
                                //
                                // type ToArray<T> = { [P in keyof T]: T[P][] };
                                //
                                //  declare function fm1<N extends unknown[]>(t: ToArray<[string, number,
                                // ...N]>): N;

                                if let Some(mapped_ty) = &mut mapped_ty {
                                    replace_type(
                                        mapped_ty,
                                        |ty| {
                                            if original_keyof_operand.type_eq(ty) {
                                                return true;
                                            }

                                            false
                                        },
                                        |ty| Some(*elem_rest_ty.ty.clone()),
                                    );
                                }
                                mapped_ty.freeze();

                                *ty = Type::Rest(RestType {
                                    span,
                                    ty: box Type::Mapped(Mapped {
                                        type_param: TypeParam {
                                            constraint: Some(box Type::Index(Index {
                                                span: elem.span,
                                                ty: elem_rest_ty.ty.clone(),
                                                metadata: Default::default(),
                                                tracker: Default::default(),
                                            })),
                                            tracker: Default::default(),
                                            ..m.type_param.clone()
                                        },
                                        ty: mapped_ty.clone(),
                                        tracker: Default::default(),
                                        ..m.clone()
                                    }),
                                    metadata: Default::default(),
                                    tracker: Default::default(),
                                });
                            } else {
                                replace_type(
                                    &mut ty,
                                    |ty| {
                                        // Check for indexed access type
                                        if let Type::IndexedAccessType(iat) = ty.normalize() {
                                            if iat.obj_type.as_ref().type_eq(original_keyof_operand) {
                                                if let Type::Param(index_type) = iat.index_type.normalize() {
                                                    return index_type.name == m.type_param.name;
                                                }
                                            }
                                        }

                                        false
                                    },
                                    |_| Some(*elem.ty.clone()),
                                );
                            }

                            let ty = match m.optional {
                                Some(TruePlusMinus::True) => box ty.union_with_undefined(span),
                                _ => ty,
                            };

                            Ok(TupleElement { ty, ..elem.clone() })
                        })
                        .collect::<VResult<_>>()?,
                    metadata: tuple.metadata,
                    tracker: Default::default(),
                });

                return Ok(Some(ty));
            }
            _ => (),
        }

        // Delegate by recursively calling this function.
        match keyof_operand.normalize() {
            Type::Readonly(Readonly { ty, .. }) => {
                if let Some(v) = self
                    .expand_mapped_type_with_keyof(span, ty, original_keyof_operand, m)
                    .context("tried to expand mapped type using a readonly operator")?
                {
                    return Ok(Some(v));
                }
            }

            Type::Param(TypeParam {
                constraint: Some(constraint),
                ..
            }) => {
                if let Some(v) = self
                    .expand_mapped_type_with_keyof(span, constraint, original_keyof_operand, m)
                    .context("tried to expand mapped type using a constraint")?
                {
                    return Ok(Some(v));
                }
            }
            _ => (),
        }

        let keys =
            self.get_property_names_for_mapped_type(span, &keyof_operand, &m.type_param, original_keyof_operand, m.name_type.as_deref())?;
        if let Some(keys) = keys {
            let members = keys
                .into_iter()
                .map(|key| -> VResult<_> {
                    match key {
                        PropertyName::Key(key) => {
                            let ty = match &m.ty {
                                Some(mapped_ty) => self
                                    .expand_key_in_mapped(m.type_param.name.clone(), mapped_ty, &key)
                                    .map(Box::new)
                                    .map(Some)?,
                                None => None,
                            };

                            let p = PropertySignature {
                                span: key.span(),
                                accessibility: None,
                                readonly: false,
                                key,
                                optional: false,
                                params: Default::default(),
                                type_ann: ty,
                                type_params: Default::default(),
                                metadata: Default::default(),
                                accessor: Default::default(),
                            };
                            let mut el = TypeElement::Property(p);

                            apply_mapped_flags(&mut el, m.optional, m.readonly);
                            Ok(el)
                        }
                        PropertyName::IndexSignature { span, params, readonly } => {
                            let ty = match &m.ty {
                                Some(mapped_ty) => {
                                    let mut map = HashMap::default();
                                    map.insert(m.type_param.name.clone(), *params[0].ty.clone());
                                    self.expand_type_params(&map, m.ty.clone(), Default::default())?
                                }
                                None => None,
                            };

                            Ok(TypeElement::Index(IndexSignature {
                                span,
                                is_static: false,
                                params,
                                type_ann: ty,
                                readonly: match m.readonly {
                                    Some(v) => match v {
                                        TruePlusMinus::True => true,
                                        TruePlusMinus::Plus => true,
                                        TruePlusMinus::Minus => false,
                                    },
                                    None => readonly,
                                },
                            }))
                        }
                    }
                })
                .collect::<Result<_, _>>()?;

            return Ok(Some(Type::TypeLit(TypeLit {
                span: m.span,
                members,
                metadata: Default::default(),
                tracker: Default::default(),
            })));
        }

        if let Some(mapped_ty) = m.ty.as_deref() {
            let found_type_param_in_keyof_operand = {
                let mut v = TypeParamNameUsageFinder::default();
                keyof_operand.visit_with(&mut v);
                !v.params.is_empty()
            };
            if !found_type_param_in_keyof_operand {
                // Check if type in `keyof T` is only used as `T[K]`.
                // If so, we can just use the type.
                //
                // {
                //     [P#5430#0 in keyof number[]]: Box<number[][P]>;
                // };

                let mut finder = IndexedAccessTypeFinder {
                    obj: &keyof_operand,
                    key: &m.type_param.name,
                    can_replace_indexed_type: false,
                };

                mapped_ty.visit_with(&mut finder);
                if finder.can_replace_indexed_type {
                    let mut replacer = IndexedAccessTypeReplacer {
                        obj: &keyof_operand,
                        key: &m.type_param.name,
                    };

                    let mut ret_ty = mapped_ty.clone();
                    ret_ty.visit_mut_with(&mut replacer);

                    ret_ty = self.apply_mapped_flags_to_type(span, ret_ty, m.optional, m.readonly)?;

                    return Ok(Some(ret_ty));
                }
            }
        }

        // error!(
        //     "unimplemented: expand_mapped_type_with_keyof\nkeyof: {}",
        //     force_dump_type_as_string(&keyof_operand)
        // );
        Ok(None)
    }

    /// TODO(kdy1): Optimize
    fn expand_key_in_mapped(&mut self, mapped_type_param: Id, mapped_ty: &Type, key: &Key) -> VResult<Type> {
        let mapped_ty = mapped_ty.clone();
        let mut type_params = HashMap::default();
        type_params.insert(mapped_type_param, key.ty().into_owned().freezed());
        self.expand_type_params(&type_params, mapped_ty, Default::default())
    }

    /// Evaluate a type and convert it to keys.
    ///
    /// Used for types like `'foo' | 'bar'` or alias of them.
    fn convert_type_to_keys_for_mapped_type(&mut self, span: Span, ty: &Type, name_type: Option<&Type>) -> VResult<Option<Vec<Key>>> {
        let _tracing = dev_span!("convert_type_to_keys_for_mapped_type");

        let _stack = stack::track(span)?;

        let ty = ty.normalize();

        match ty {
            Type::Ref(..) | Type::Alias(..) | Type::Query(..) => {
                let ty = self.normalize(
                    Some(span),
                    Cow::Borrowed(ty),
                    NormalizeTypeOpts {
                        preserve_global_this: true,
                        preserve_union: true,
                        ..Default::default()
                    },
                )?;
                self.convert_type_to_keys_for_mapped_type(span, &ty, name_type)
            }

            Type::Lit(LitType { lit, .. }) => match lit {
                RTsLit::BigInt(v) => Ok(Some(vec![Key::BigInt(v.clone())])),
                RTsLit::Number(v) => Ok(Some(vec![Key::Num(v.clone())])),
                RTsLit::Str(v) => Ok(Some(vec![Key::Normal {
                    span: v.span,
                    sym: v.value.clone(),
                }])),
                RTsLit::Tpl(t) if t.quasis.len() == 1 => Ok(Some(vec![Key::Normal {
                    span: t.span,
                    sym: match &t.quasis[0].cooked {
                        Some(v) => (&**v).into(),
                        _ => return Ok(None),
                    },
                }])),
                RTsLit::Bool(_) | RTsLit::Tpl(_) => Ok(None),
            },

            Type::Union(u) => {
                let mut keys = vec![];

                for ty in &u.types {
                    let elem_keys = self.convert_type_to_keys_for_mapped_type(span, ty, name_type)?;
                    match elem_keys {
                        Some(v) => keys.extend(v),
                        None => return Ok(None),
                    }
                }

                Ok(Some(keys))
            }

            Type::Enum(e) => {
                let mut keys = vec![];

                for v in &e.members {
                    if let Type::Lit(v) = v.val.normalize() {
                        match &v.lit {
                            RTsLit::Str(key) => {
                                keys.push(Key::Normal {
                                    span: key.span,
                                    sym: key.value.clone(),
                                });
                            }
                            RTsLit::Number(key) => {
                                keys.push(Key::Num(key.clone()));
                            }
                            _ => {}
                        }
                    }
                }

                Ok(Some(keys))
            }

            Type::EnumVariant(e) => {
                let mut keys = vec![];

                let items = self.convert_type_to_keys_for_mapped_type(span, &Type::Enum(e.def.cheap_clone()), name_type)?;
                keys.extend(items.into_iter().flatten());

                Ok(Some(keys))
            }

            Type::TypeLit(..) | Type::Interface(..) | Type::Class(..) | Type::ClassDef(..) => Ok(None),

            _ => {
                error!("unimplemented: convert_type_to_keys: {}", force_dump_type_as_string(ty));
                Ok(None)
            }
        }
    }

    /// Get keys of `ty` as a property name.
    pub(crate) fn get_property_names_for_mapped_type(
        &mut self,
        span: Span,
        ty: &Type,
        type_param: &TypeParam,
        original_keyof_operand: &Type,
        name_type: Option<&Type>,
    ) -> VResult<Option<Vec<PropertyName>>> {
        let _tracing = dev_span!("get_property_names_for_mapped_type");

        if let Some(name_type) = name_type {
            let property_names = self
                .get_property_names_for_mapped_type(span, ty, type_param, original_keyof_operand, None)
                .context("tried to get property names from a type to expand a mapper typw with a name type")?;

            let property_names = match property_names {
                Some(v) => v,
                None => return Ok(None),
            };

            let name_type = self
                .normalize(
                    Some(span),
                    Cow::Borrowed(name_type),
                    NormalizeTypeOpts {
                        preserve_global_this: true,
                        ..Default::default()
                    },
                )
                .context("tried to normalize a name type to expand a mapper type")?
                .freezed();

            let mut new_keys = vec![];

            for property_name in property_names {
                match property_name {
                    PropertyName::Key(key) => {
                        let mut new_key = name_type.clone().into_owned();

                        // Replace T with ty
                        replace_type(&mut new_key, |needle| needle.type_eq(original_keyof_operand), |_| Some(ty.clone()));
                        // Replace K with key
                        replace_type(
                            &mut new_key,
                            |needle| match needle.normalize() {
                                Type::Param(needle) => needle.name.sym() == type_param.name.sym(),
                                _ => false,
                            },
                            |_| Some(key.ty().into_owned()),
                        );

                        let new_key = self
                            .normalize(
                                Some(span),
                                Cow::Owned(new_key),
                                NormalizeTypeOpts {
                                    preserve_mapped: true,
                                    preserve_global_this: true,
                                    preserve_intersection: true,
                                    preserve_union: true,
                                    ..Default::default()
                                },
                            )?
                            .freezed()
                            .into_owned();

                        new_keys.push(new_key);
                    }

                    PropertyName::IndexSignature { span, params, readonly } => todo!(),
                }
            }

            let keys = self.convert_type_to_keys_for_mapped_type(span, &Type::new_union(span, new_keys), None)?;
            let keys = match keys {
                Some(v) => v,
                None => return Ok(None),
            };
            return Ok(Some(keys.into_iter().map(|v| v.into()).collect()));
        }

        let ty = self
            .normalize(
                Some(span),
                Cow::Borrowed(ty),
                NormalizeTypeOpts {
                    normalize_keywords: true,
                    ..Default::default()
                },
            )
            .context("tried to normalize a type to get keys from it")?;

        if ty.is_any() {
            return Ok(None);
        }

        match ty.normalize() {
            Type::Keyword(KeywordType {
                kind: TsKeywordTypeKind::TsUndefinedKeyword | TsKeywordTypeKind::TsNullKeyword,
                ..
            }) => return Ok(Some(vec![])),

            Type::TypeLit(ty) => {
                let mut keys = vec![];
                for m in &ty.members {
                    match m {
                        TypeElement::Call(_) => {}
                        TypeElement::Constructor(_) => {}
                        TypeElement::Property(p) => {
                            keys.push(p.key.clone().into());
                        }
                        TypeElement::Method(m) => {
                            keys.push(m.key.clone().into());
                        }
                        TypeElement::Index(i) => {
                            keys.push(PropertyName::IndexSignature {
                                span: i.span,
                                params: i.params.clone(),
                                readonly: i.readonly,
                            });
                        }
                    }
                }

                return Ok(Some(keys));
            }
            Type::Interface(ty) => {
                let mut keys = vec![];
                for m in &ty.body {
                    match m {
                        TypeElement::Call(_) => {}
                        TypeElement::Constructor(_) => {}
                        TypeElement::Property(p) => {
                            keys.push(p.key.clone().into());
                        }
                        TypeElement::Method(m) => {
                            keys.push(m.key.clone().into());
                        }
                        TypeElement::Index(i) => {
                            keys.push(PropertyName::IndexSignature {
                                span: i.span,
                                params: i.params.clone(),
                                readonly: i.readonly,
                            });
                        }
                    }
                }

                for parent in &ty.extends {
                    let parent = self.type_of_ts_entity_name(span, &parent.expr, parent.type_args.as_deref())?;
                    if let Some(parent_keys) =
                        self.get_property_names_for_mapped_type(span, &parent, type_param, original_keyof_operand, name_type)?
                    {
                        keys.extend(parent_keys);
                    }
                }

                return Ok(Some(keys));
            }
            Type::Enum(e) => {
                let mut keys = vec![];
                for member in &e.members {
                    keys.push(PropertyName::Key(match &member.id {
                        RTsEnumMemberId::Ident(i) => Key::Normal {
                            span: i.span,
                            sym: i.sym.clone(),
                        },
                        RTsEnumMemberId::Str(s) => Key::Normal {
                            span: s.span,
                            sym: s.value.clone(),
                        },
                    }))
                }

                return Ok(Some(keys));
            }
            Type::Param(..) => return Ok(None),

            Type::Intersection(ty) => {
                let keys_types = ty
                    .types
                    .iter()
                    .map(|ty| -> VResult<_> {
                        self.get_property_names_for_mapped_type(span, ty, type_param, original_keyof_operand, name_type)
                    })
                    .collect::<Result<Vec<_>, _>>()?;

                if keys_types.is_empty() {
                    return Ok(None);
                }

                let mut result: Vec<PropertyName> = vec![];

                if keys_types.iter().all(|keys| keys.is_none()) {
                    return Ok(None);
                }

                let sets = &keys_types[1..];

                for key in keys_types[0].iter().flatten().filter(|item| {
                    {
                        sets.iter().all(|set| set.is_none() || set.as_ref().unwrap().contains(item))
                    }
                }) {
                    if result.iter().any(|prev| prev.type_eq(key)) {
                        continue;
                    }

                    result.push(key.clone());
                }

                if result.is_empty() {
                    return Ok(None);
                }

                return Ok(Some(result));
            }

            Type::Union(ty) => {
                let keys_types = ty
                    .types
                    .iter()
                    .map(|ty| -> VResult<_> {
                        self.get_property_names_for_mapped_type(span, ty, type_param, original_keyof_operand, name_type)
                    })
                    .collect::<Result<Vec<_>, _>>()?;

                let mut result: Vec<PropertyName> = vec![];

                if keys_types.iter().all(|keys| keys.is_none()) {
                    return Ok(None);
                }

                for keys in keys_types.into_iter().flatten() {
                    for key in keys {
                        if result.iter().any(|prev| prev.type_eq(&key)) {
                            continue;
                        }

                        result.push(key);
                    }
                }

                return Ok(Some(result));
            }

            Type::Tuple(tuple) => {
                // numeric keys and length

                let mut keys = vec![];
                for (i, elem) in tuple.elems.iter().enumerate() {
                    if elem.ty.is_rest() {
                        keys.push(PropertyName::IndexSignature {
                            span: elem.span,
                            params: vec![FnParam {
                                span: elem.span,
                                required: true,
                                pat: RPat::Ident(RBindingIdent {
                                    node_id: NodeId::invalid(),
                                    id: RIdent::new("__key".into(), elem.span.with_ctxt(SyntaxContext::empty())),
                                    type_ann: None,
                                }),
                                ty: box Type::Keyword(KeywordType {
                                    span: elem.span,
                                    kind: TsKeywordTypeKind::TsNumberKeyword,
                                    metadata: Default::default(),
                                    tracker: Default::default(),
                                }),
                            }],
                            readonly: false,
                        });
                        return Ok(Some(keys));
                    }
                    keys.push(PropertyName::Key(Key::Num(RNumber {
                        span: elem.span,
                        value: i as _,
                        raw: None,
                    })));
                }

                // .length

                // keys.push(PropertyName::Key(Key::Normal {
                //     span: tuple.span,
                //     sym: js_word!("length"),
                // }));

                return Ok(Some(keys));
            }

            Type::Array(array) => {
                // numeric indexer and length

                return Ok(Some(vec![
                    PropertyName::IndexSignature {
                        span: array.span,
                        params: vec![FnParam {
                            span: array.span,
                            required: true,
                            pat: RPat::Ident(RBindingIdent {
                                node_id: NodeId::invalid(),
                                id: RIdent::new("__array_key".into(), array.span.with_ctxt(SyntaxContext::empty())),
                                type_ann: None,
                            }),
                            ty: box Type::Keyword(KeywordType {
                                span: array.span,
                                kind: TsKeywordTypeKind::TsNumberKeyword,
                                metadata: Default::default(),
                                tracker: Default::default(),
                            }),
                        }],
                        readonly: false,
                    },
                    // PropertyName::Key(Key::Normal {
                    //     span: array.span,
                    //     sym: js_word!("length"),
                    // }),
                ]));
            }

            Type::Mapped(m) => {
                if let Some(Type::Index(Index { ty, .. })) = m.type_param.constraint.as_deref().map(|ty| ty.normalize()) {
                    return self
                        .get_property_names_for_mapped_type(span, ty, type_param, original_keyof_operand, name_type)
                        .context("tried to get property names by using `keyof` constraint");
                }
            }

            _ => {}
        }

        Ok(None)
    }

    pub(crate) fn apply_mapped_flags_to_type(
        &mut self,
        span: Span,
        ty: Type,
        optional: Option<TruePlusMinus>,
        readonly: Option<TruePlusMinus>,
    ) -> VResult<Type> {
        let type_lit = self.convert_type_to_type_lit(span, Cow::Borrowed(&ty))?.map(Cow::into_owned);
        if let Some(mut type_lit) = type_lit {
            for m in &mut type_lit.members {
                apply_mapped_flags(m, optional, readonly);
            }

            Ok(Type::TypeLit(type_lit))
        } else {
            Ok(ty)
        }
    }
}

#[derive(Debug, Clone, Spanned, TypeEq, PartialEq)]
pub(crate) enum PropertyName {
    Key(Key),
    /// Created from an index signature.
    IndexSignature {
        span: Span,
        params: Vec<FnParam>,
        readonly: bool,
    },
}

impl From<Key> for PropertyName {
    fn from(key: Key) -> Self {
        Self::Key(key)
    }
}

#[derive(Debug)]
struct IndexedAccessTypeFinder<'a> {
    obj: &'a Type,
    key: &'a Id,

    can_replace_indexed_type: bool,
}

impl Visit<Conditional> for IndexedAccessTypeFinder<'_> {
    fn visit(&mut self, n: &Conditional) {
        n.check_type.visit_children_with(self);

        n.extends_type.visit_children_with(self);
    }
}

impl Visit<IndexedAccessType> for IndexedAccessTypeFinder<'_> {
    fn visit(&mut self, n: &IndexedAccessType) {
        if (*n.obj_type).type_eq(self.obj)
            && match n.index_type.normalize() {
                Type::Param(index) => *self.key == index.name,
                _ => false,
            }
        {
            self.can_replace_indexed_type = true;
            return;
        }

        n.visit_children_with(self);
    }
}

#[derive(Debug)]
struct IndexedAccessTypeReplacer<'a> {
    obj: &'a Type,
    key: &'a Id,
}

impl VisitMut<Type> for IndexedAccessTypeReplacer<'_> {
    fn visit_mut(&mut self, ty: &mut Type) {
        {
            let mut v = IndexedAccessTypeFinder {
                obj: self.obj,
                key: self.key,
                can_replace_indexed_type: false,
            };

            ty.visit_with(&mut v);
            if !v.can_replace_indexed_type {
                return;
            }
        }

        // TODO(kdy1): PERF
        ty.normalize_mut();

        if let Type::IndexedAccessType(n) = ty {
            if (*n.obj_type).type_eq(self.obj)
                && match n.index_type.normalize() {
                    Type::Param(index) => *self.key == index.name,
                    _ => false,
                }
            {
                *ty = self.obj.clone();
            }
        }
    }
}
