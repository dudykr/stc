use crate::{
    analyzer::{Analyzer, ScopeKind},
    util::type_ext::TypeVecExt,
    validator::ValidateWith,
    ValidationResult,
};
use fxhash::FxHashMap;
use indexmap::IndexSet;
use itertools::Itertools;
use rnode::{FoldWith, NodeId, VisitMut, VisitMutWith};
use stc_ts_ast_rnode::{RBindingIdent, RIdent, RObjectLit, RPat, RPropOrSpread, RSpreadElement, RTsKeywordType};
use stc_ts_errors::{DebugExt, Error};
use stc_ts_file_analyzer_macros::validator;
use stc_ts_generics::type_param::replacer::TypeParamReplacer;
use stc_ts_type_ops::Fix;
use stc_ts_types::{
    Accessor, CallSignature, FnParam, Function, Key, PropertySignature, Type, TypeElement, TypeLit, TypeLitMetadata,
    TypeParamDecl, Union,
};
use std::{borrow::Cow, iter::repeat, time::Instant};
use swc_atoms::JsWord;
use swc_common::{Spanned, TypeEq, DUMMY_SP};
use swc_ecma_ast::TsKeywordTypeKind;

#[validator]
impl Analyzer<'_, '_> {
    fn validate(&mut self, node: &RObjectLit, type_ann: Option<&Type>) -> ValidationResult {
        let type_ann = self.expand_type_ann(type_ann)?;

        self.with_child(ScopeKind::ObjectLit, Default::default(), |a: &mut Analyzer| {
            let mut ret = Type::TypeLit(TypeLit {
                span: node.span,
                members: vec![],
                metadata: Default::default(),
            });

            let mut known_keys = vec![];
            for prop in node.props.iter() {
                ret = a.append_prop_or_spread_to_type(&mut known_keys, ret, prop, type_ann.as_deref())?;
            }

            Ok(ret)
        })
    }
}

struct UnionNormalizer<'a, 'b, 'c> {
    anaylzer: &'a mut Analyzer<'b, 'c>,
    preserve_specified: bool,
}

impl UnionNormalizer<'_, '_, '_> {
    /// We need to know shape of normalized type literal.
    ///
    /// We use indexset to remove duplicate while preserving order.
    fn find_keys(&self, types: &[Type]) -> IndexSet<JsWord> {
        types
            .iter()
            .filter_map(|ty| match ty.normalize() {
                Type::TypeLit(ty) => Some(&ty.members),
                _ => None,
            })
            .flatten()
            .filter_map(|member| member.non_computed_key().cloned())
            .collect()
    }

    fn normalize_fn_types(&mut self, ty: &mut Type) {
        let u = match ty {
            Type::Union(u) => u,
            _ => return,
        };
        if u.types.iter().any(|ty| !ty.normalize().is_function()) {
            return;
        }

        let mut new_type_params = None;
        let mut new_params = vec![];
        let mut return_types = vec![];

        for ty in &u.types {
            match ty.normalize() {
                Type::Function(f) => {
                    if new_type_params.is_none() {
                        new_type_params = f.type_params.clone();
                    }

                    for (idx, param) in f.params.iter().enumerate() {
                        if new_params.len() <= idx {
                            new_params.extend(repeat(vec![]).take(idx + 1 - new_params.len()));
                        }

                        new_params[idx].push(param);
                    }

                    return_types.push(*f.ret_ty.clone());
                }

                _ => {}
            }
        }

        return_types.dedup_type();
        if let Some(ty) = return_types
            .iter()
            .find(|ty| ty.is_kwd(TsKeywordTypeKind::TsVoidKeyword))
        {
            return_types = vec![ty.clone()]
        }

        *ty = Type::Function(Function {
            span: u.span,
            type_params: new_type_params,
            params: new_params
                .into_iter()
                .map(|params| {
                    let mut pat = None;
                    let mut types = vec![];
                    for param in params {
                        if pat.is_none() {
                            pat = Some(param.pat.clone());
                        }

                        types.push(*param.ty.clone());
                    }
                    types.dedup_type();

                    let ty = box Type::intersection(DUMMY_SP, types);
                    FnParam {
                        span: DUMMY_SP,
                        // TODO
                        required: true,
                        // TODO
                        pat: pat.unwrap_or_else(|| {
                            RPat::Ident(RBindingIdent {
                                node_id: NodeId::invalid(),
                                id: RIdent::new("a".into(), DUMMY_SP),
                                type_ann: None,
                            })
                        }),
                        ty,
                    }
                })
                .collect_vec(),
            ret_ty: box Type::union(return_types),
        })
    }

    /// TODO: Add type parameters.
    fn normalize_call_signatures(&self, ty: &mut Type) {
        let u = match ty {
            Type::Union(u) => u,
            _ => return,
        };
        if u.types.iter().any(|ty| !ty.normalize().is_type_lit()) {
            return;
        }
        let mut inexact = false;
        let mut prev_specified = false;

        let mut new_type_params = FxHashMap::<_, TypeParamDecl>::default();
        let mut new_params = FxHashMap::<_, Vec<_>>::default();
        let mut new_return_types = FxHashMap::<_, Vec<_>>::default();
        let mut extra_members = vec![];
        //
        for (type_idx, ty) in u.types.iter().enumerate() {
            match ty.normalize() {
                Type::TypeLit(ty) => {
                    inexact |= ty.metadata.inexact;
                    prev_specified |= ty.metadata.specified;

                    for (i, m) in ty.members.iter().enumerate() {
                        //
                        match m {
                            TypeElement::Call(CallSignature {
                                type_params,
                                params,
                                ret_ty,
                                ..
                            }) => {
                                let mut params = params.clone();

                                if let Some(type_params) = type_params {
                                    if let Some(prev) = new_type_params.get(&i) {
                                        // We replace new type params woth previous type param.
                                        let mut inferred = prev
                                            .params
                                            .iter()
                                            .cloned()
                                            .map(Type::Param)
                                            .zip(type_params.params.iter())
                                            .map(|(prev, new)| (new.name.clone(), prev))
                                            .collect();

                                        params = params.fold_with(&mut TypeParamReplacer {
                                            inferred,
                                            include_type_params: true,
                                        });
                                    } else {
                                        new_type_params.entry(i).or_insert_with(|| type_params.clone());
                                    }
                                }

                                // Parameters are intersectioned, and return
                                // types are unioned.

                                for (idx, param) in params.into_iter().enumerate() {
                                    let new_params = new_params.entry(i).or_default();
                                    if new_params.len() <= idx {
                                        new_params.extend(repeat(vec![]).take(idx + 1 - new_params.len()));
                                    }

                                    new_params[idx].push(param);
                                }

                                new_return_types
                                    .entry(i)
                                    .or_default()
                                    .extend(ret_ty.clone().map(|v| *v));
                            }
                            _ => {
                                if extra_members.len() <= type_idx {
                                    extra_members.extend(repeat(vec![]).take(type_idx + 1 - extra_members.len()));
                                }

                                extra_members[type_idx].push(m.clone())
                            }
                        }
                    }
                }
                _ => {}
            }
        }

        if new_params.is_empty() {
            return;
        }

        let mut members = vec![];

        for (i, new_params) in new_params {
            let mut return_types = new_return_types.remove(&i).unwrap_or_default();
            return_types.dedup_type();
            if let Some(ty) = return_types
                .iter()
                .find(|ty| ty.is_kwd(TsKeywordTypeKind::TsVoidKeyword))
            {
                return_types = vec![ty.clone()]
            }

            let type_params = new_type_params.remove(&i);

            members.push(TypeElement::Call(CallSignature {
                span: DUMMY_SP,
                ret_ty: Some(box Type::union(return_types)),
                type_params,
                params: new_params
                    .into_iter()
                    .map(|params| {
                        let mut pat = None;
                        let mut types = vec![];
                        for param in params {
                            if pat.is_none() {
                                pat = Some(param.pat);
                            }

                            types.push(*param.ty);
                        }
                        types.dedup_type();

                        let ty = box Type::intersection(DUMMY_SP, types);
                        FnParam {
                            span: DUMMY_SP,
                            // TODO
                            required: true,
                            // TODO
                            pat: pat.unwrap_or_else(|| {
                                RPat::Ident(RBindingIdent {
                                    node_id: NodeId::invalid(),
                                    id: RIdent::new("a".into(), DUMMY_SP),
                                    type_ann: None,
                                })
                            }),
                            ty,
                        }
                    })
                    .collect_vec(),
            }));
        }

        let new_lit = TypeLit {
            span: u.span,
            members,
            metadata: TypeLitMetadata {
                normalized: true,
                inexact,
                specified: self.preserve_specified && prev_specified,
                ..Default::default()
            },
        };

        if extra_members.is_empty() {
            *ty = Type::TypeLit(new_lit);
            return;
        }

        let mut new_types = extra_members
            .into_iter()
            .map(|extra_members| {
                let mut new_lit = new_lit.clone();
                new_lit.members.extend(extra_members);
                new_lit
            })
            .map(Type::TypeLit)
            .collect_vec();

        u.types = new_types;
    }

    fn normalize_keys(&self, u: &mut Union) {
        if u.types.len() <= 1 {
            return;
        }

        let keys = self.find_keys(&u.types);

        let inexact = u.types.iter().any(|ty| match ty.normalize() {
            Type::TypeLit(ty) => ty.metadata.inexact,
            _ => false,
        });

        // Add properties.
        for ty in u.types.iter_mut() {
            match ty.normalize_mut() {
                Type::TypeLit(ty) => {
                    ty.metadata.inexact |= inexact;
                    ty.metadata.normalized = true;

                    for key in &keys {
                        let has_key = ty.members.iter().any(|member| {
                            if let Some(member_key) = member.non_computed_key() {
                                *key == *member_key
                            } else {
                                false
                            }
                        });

                        if !has_key {
                            ty.members.push(TypeElement::Property(PropertySignature {
                                span: DUMMY_SP,
                                accessibility: None,
                                readonly: false,
                                key: Key::Normal {
                                    span: DUMMY_SP,
                                    sym: key.clone(),
                                },
                                optional: true,
                                params: Default::default(),
                                type_ann: Some(box Type::Keyword(RTsKeywordType {
                                    span: DUMMY_SP,
                                    kind: swc_ecma_ast::TsKeywordTypeKind::TsUndefinedKeyword,
                                })),
                                type_params: Default::default(),
                                metadata: Default::default(),
                                accessor: Default::default(),
                            }))
                        }
                    }
                }
                _ => {}
            }
        }
    }
}

impl VisitMut<Type> for UnionNormalizer<'_, '_, '_> {
    fn visit_mut(&mut self, ty: &mut Type) {
        ty.visit_mut_children_with(self);

        self.normalize_call_signatures(ty);
        self.normalize_fn_types(ty);
    }
}

impl VisitMut<Union> for UnionNormalizer<'_, '_, '_> {
    fn visit_mut(&mut self, u: &mut Union) {
        u.visit_mut_children_with(self);

        // If an union does not contains object literals, skip it.
        if u.types.iter().all(|ty| !ty.normalize().is_type_lit()) {
            return;
        }

        self.normalize_keys(u);
    }
}

impl Analyzer<'_, '_> {
    /// Object literals in unions are normalized upon widening.
    ///
    ///```ts
    /// var a = [{ a: 0 }, { a: 1, b: "x" }];
    /// ```
    ///
    /// Type of `a` in the code above is `{ a: number, b?: undefined } | {
    /// a:number, b: string }`.
    pub(super) fn normalize_union(&mut self, ty: &mut Type, preserve_specified: bool) {
        let start = Instant::now();
        ty.visit_mut_with(&mut UnionNormalizer {
            anaylzer: self,
            preserve_specified,
        });

        let end = Instant::now();

        slog::debug!(self.logger, "Normlaized unions (time = {:?})", end - start);
    }

    fn append_prop_or_spread_to_type(
        &mut self,
        known_keys: &mut Vec<Key>,
        to: Type,
        prop: &RPropOrSpread,
        object_type: Option<&Type>,
    ) -> ValidationResult {
        match prop {
            RPropOrSpread::Spread(RSpreadElement { expr, .. }) => {
                let prop_ty: Type = expr.validate_with_default(self)?;
                self.append_type(to, prop_ty)
            }
            RPropOrSpread::Prop(prop) => {
                let p: TypeElement = prop.validate_with_args(self, object_type)?;

                match p {
                    TypeElement::Method(..)
                    | TypeElement::Property(PropertySignature {
                        accessor:
                            Accessor {
                                getter: false,
                                setter: false,
                            },
                        ..
                    }) => {
                        if let Some(key) = p.key() {
                            let span = key.span();

                            // Check if duplicate key exists.
                            // We show errors on the second key and latters.
                            //
                            // See: es6/Symbols/symbolProperty36.ts

                            if known_keys.iter().any(|prev_key| {
                                // TODO: Use
                                // self.key_matches(span, prev_key, key, false)
                                prev_key.type_eq(&key)
                            }) {
                                self.storage.report(Error::DuplicateProperty { span })
                            }

                            known_keys.push(key.clone());
                        }
                    }
                    _ => {}
                }

                self.append_type_element(to, p)
            }
        }
    }

    /// If rhs is an union type, return type will be union.
    ///
    /// `{ a: number } + ( {b: number} | { c: number } )` => `{ a: number, b:
    /// number } | { a: number, c: number }`
    fn append_type(&mut self, to: Type, rhs: Type) -> ValidationResult<Type> {
        if to.is_any() || to.is_unknown() {
            return Ok(to);
        }
        if rhs.is_any() || rhs.is_unknown() {
            return Ok(rhs);
        }

        if rhs.is_kwd(TsKeywordTypeKind::TsNullKeyword)
            || rhs.is_kwd(TsKeywordTypeKind::TsUndefinedKeyword)
            || rhs.is_kwd(TsKeywordTypeKind::TsVoidKeyword)
        {
            return Ok(to);
        }

        if to.is_kwd(TsKeywordTypeKind::TsObjectKeyword) || rhs.is_kwd(TsKeywordTypeKind::TsObjectKeyword) {
            return Ok(Type::Keyword(RTsKeywordType {
                span: to.span(),
                kind: TsKeywordTypeKind::TsObjectKeyword,
            }));
        }

        match rhs.normalize() {
            Type::Ref(..) => {
                let rhs = self
                    .expand_top_ref(rhs.span(), Cow::Owned(rhs), Default::default())?
                    .into_owned();
                return self.append_type(to, rhs);
            }

            Type::Interface(..) | Type::Class(..) | Type::Intersection(..) | Type::Mapped(..) => {
                // Append as a type literal.
                if let Some(rhs) = self.type_to_type_lit(rhs.span(), &rhs)? {
                    return self.append_type(to, Type::TypeLit(rhs.into_owned()));
                }
            }

            _ => {}
        }

        let mut to = to.foldable();
        match to {
            Type::TypeLit(ref mut lit) => {
                lit.metadata.inexact = true;

                match rhs {
                    Type::TypeLit(rhs) => {
                        lit.members.extend(rhs.members);
                        return Ok(to);
                    }
                    Type::Union(rhs) => {
                        return Ok(Type::Union(Union {
                            span: lit.span,
                            types: rhs
                                .types
                                .into_iter()
                                .map(|rhs| self.append_type(to.clone(), rhs))
                                .collect::<Result<_, _>>()?,
                        })
                        .fixed())
                    }
                    _ => {}
                }
            }

            Type::Union(to) => {
                return Ok(Type::Union(Union {
                    span: to.span,
                    types: to
                        .types
                        .into_iter()
                        .map(|to| self.append_type(to, rhs.clone()))
                        .collect::<Result<_, _>>()?,
                })
                .fixed())
            }

            _ => {}
        }

        unimplemented!("append_type:\n{:?}\n{:?}", to, rhs)
    }

    fn append_type_element(&mut self, to: Type, rhs: TypeElement) -> ValidationResult {
        if to.is_any() || to.is_unknown() {
            return Ok(to);
        }
        if to.is_kwd(TsKeywordTypeKind::TsObjectKeyword) {
            return Ok(to);
        }

        let mut to = if let Some(key) = rhs.key() {
            match key {
                Key::Computed(..) => to.foldable(),
                _ => self
                    .exclude_props(rhs.span(), &to, &[key.clone()])
                    .context("tried to exclude properties before appending a type element")?,
            }
        } else {
            to.foldable()
        };

        match to {
            Type::TypeLit(ref mut lit) => {
                // TODO: Remove previous member with same key.

                lit.members.push(rhs);
                Ok(to)
            }
            Type::Union(to) => Ok(Type::Union(Union {
                span: to.span,
                types: to
                    .types
                    .into_iter()
                    .map(|to| self.append_type_element(to, rhs.clone()))
                    .collect::<Result<_, _>>()?,
            })
            .fixed()),
            _ => {
                unimplemented!("append_type_element\n{:?}\n{:?}", to, rhs)
            }
        }
    }
}
