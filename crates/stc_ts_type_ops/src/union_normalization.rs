use std::iter::repeat;

use indexmap::IndexSet;
use rnode::{FoldWith, NodeId, VisitMut, VisitMutWith};
use rustc_hash::FxHashMap;
use stc_ts_ast_rnode::{RBindingIdent, RIdent, RPat};
use stc_ts_generics::type_param::replacer::TypeParamReplacer;
use stc_ts_types::{
    CallSignature, FnParam, Function, FunctionMetadata, Key, KeywordType, PropertySignature, Type, TypeElement, TypeLit, TypeLitMetadata,
    TypeParamDecl, Union,
};
use stc_utils::{cache::Freeze, dev_span, ext::TypeVecExt};
use swc_atoms::JsWord;
use swc_common::DUMMY_SP;
use swc_ecma_ast::TsKeywordTypeKind;

/// See https://github.com/dudykr/stc/blob/e8f1daf0e336d978a1de5479ad9676093faf5921/crates/stc_ts_type_checker/tests/conformance/expressions/objectLiterals/objectLiteralNormalization.ts
pub struct ObjectUnionNormalizer {
    pub preserve_specified: bool,
}

impl ObjectUnionNormalizer {
    /// We need to know shape of normalized type literal.
    ///
    /// We use indexset to remove duplicate while preserving order.
    fn find_keys(&self, types: &[Type]) -> IndexSet<Vec<JsWord>> {
        types.iter().flat_map(|t| self.find_keys_of_type(t)).collect()
    }

    fn find_keys_of_type(&self, ty: &Type) -> IndexSet<Vec<JsWord>> {
        match ty.normalize() {
            Type::TypeLit(ty) => {
                let mut keys = IndexSet::default();

                for el in ty.members.iter() {
                    let key = el.non_computed_key().cloned();

                    let key = match key {
                        Some(v) => v,
                        _ => continue,
                    };

                    if let TypeElement::Property(PropertySignature {
                        type_ann: Some(type_ann), ..
                    }) = el
                    {
                        let nested_keys = self.find_keys_of_type(type_ann);
                        if nested_keys.is_empty() {
                            keys.insert(vec![key]);
                        } else {
                            keys.extend(nested_keys.into_iter().map(|mut keys| {
                                keys.insert(0, key.clone());
                                keys
                            }));
                        }
                        continue;
                    }

                    keys.insert(vec![key]);
                }

                return keys;
            }
            Type::Union(ty) => return self.find_keys(&ty.types),
            _ => {}
        }

        Default::default()
    }

    fn normalize_fn_types(&mut self, ty: &mut Type) {
        if !ty.is_union_type() {
            return;
        }
        ty.freeze();
        let u = match ty.as_union_type_mut() {
            Some(v) => v,
            None => return,
        };
        if u.types.iter().any(|ty| !ty.is_fn_type()) {
            return;
        }

        let mut new_type_params = None;
        let mut new_params = vec![];
        let mut return_types = vec![];

        for ty in &u.types {
            if let Type::Function(f) = ty.normalize() {
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
        }

        return_types.dedup_type();
        if let Some(ty) = return_types.iter().find(|ty| ty.is_kwd(TsKeywordTypeKind::TsVoidKeyword)) {
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

                    let ty = Box::new(Type::new_intersection(DUMMY_SP, types));
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
                .collect(),
            ret_ty: Box::new(Type::new_union(u.span, return_types)),
            metadata: FunctionMetadata {
                common: u.metadata.common,
                ..Default::default()
            },
            tracker: Default::default(),
        })
    }

    /// TODO(kdy1): Add type parameters.
    fn normalize_call_signatures(&self, ty: &mut Type) {
        if !ty.is_union_type() {
            return;
        }
        ty.freeze();

        let u = match ty.as_union_type_mut() {
            Some(u) => u,
            _ => return,
        };
        if u.types.iter().any(|ty| !ty.is_type_lit()) {
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
            if let Type::TypeLit(ty) = ty.normalize() {
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
                                    // We replace new type params with previous type param.
                                    let inferred = prev
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

                            // Parameters are intersected, and return types are unified.

                            for (idx, param) in params.into_iter().enumerate() {
                                let new_params = new_params.entry(i).or_default();
                                if new_params.len() <= idx {
                                    new_params.extend(repeat(vec![]).take(idx + 1 - new_params.len()));
                                }

                                new_params[idx].push(param);
                            }

                            new_return_types.entry(i).or_default().extend(ret_ty.clone().map(|v| *v));
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
        }

        if new_params.is_empty() {
            return;
        }

        let mut members = vec![];

        for (i, new_params) in new_params {
            let mut return_types = new_return_types.remove(&i).unwrap_or_default();
            return_types.dedup_type();
            if let Some(ty) = return_types.iter().find(|ty| ty.is_kwd(TsKeywordTypeKind::TsVoidKeyword)) {
                return_types = vec![ty.clone()]
            }

            let type_params = new_type_params.remove(&i);

            members.push(TypeElement::Call(CallSignature {
                span: DUMMY_SP,
                ret_ty: Some(Box::new(Type::new_union(DUMMY_SP, return_types))),
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

                        let ty = Box::new(Type::new_intersection(DUMMY_SP, types));
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
                    .collect(),
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
            tracker: Default::default(),
        };

        if extra_members.is_empty() {
            *ty = Type::TypeLit(new_lit);
            return;
        }

        let new_types = extra_members
            .into_iter()
            .map(|extra_members| {
                let mut new_lit = new_lit.clone();
                new_lit.members.extend(extra_members);
                new_lit
            })
            .map(Type::TypeLit)
            .collect();

        u.types = new_types;
    }

    /// - `types`: Types of a union.
    fn normalize_keys(&self, types: &mut Vec<Type>) {
        fn insert_property_to(ty: &mut Type, keys: &[JsWord], inexact: bool) {
            if let Some(ty) = ty.as_union_type_mut() {
                for ty in &mut ty.types {
                    insert_property_to(ty, keys, inexact);
                }
                return;
            }

            if let Some(ty) = ty.as_type_lit_mut() {
                ty.metadata.inexact |= inexact;
                ty.metadata.normalized = true;

                match keys.len() {
                    0 => {
                        unreachable!()
                    }
                    1 => {
                        let key = &keys[0];
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
                                type_ann: Some(Box::new(Type::Keyword(KeywordType {
                                    span: DUMMY_SP,
                                    kind: TsKeywordTypeKind::TsUndefinedKeyword,
                                    metadata: Default::default(),
                                    tracker: Default::default(),
                                }))),
                                type_params: Default::default(),
                                metadata: Default::default(),
                                accessor: Default::default(),
                            }))
                        }
                    }
                    _ => {
                        // Normalization applies to nested properties
                        let key = &keys[0];
                        let idx = ty.members.iter().position(|member| {
                            if let Some(member_key) = member.non_computed_key() {
                                *key == *member_key
                            } else {
                                false
                            }
                        });

                        let idx = match idx {
                            Some(v) => v,
                            _ => {
                                let idx = ty.members.len();
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
                                    type_ann: Some(Box::new(Type::TypeLit(TypeLit {
                                        span: DUMMY_SP,
                                        members: Default::default(),
                                        metadata: Default::default(),
                                        tracker: Default::default(),
                                    }))),
                                    type_params: Default::default(),
                                    metadata: Default::default(),
                                    accessor: Default::default(),
                                }));
                                idx
                            }
                        };

                        if let TypeElement::Property(prop) = &mut ty.members[idx] {
                            if let Some(ty) = prop.type_ann.as_deref_mut() {
                                insert_property_to(ty, &keys[1..], inexact)
                            }
                        }
                    }
                }
            }
        }

        let _tracing = dev_span!("normalize_keys");

        let deep = self.find_keys(&*types);

        let inexact = types.iter().any(|ty| match ty.normalize() {
            Type::TypeLit(ty) => ty.metadata.inexact,
            _ => false,
        });

        // Add properties.
        for ty in types.iter_mut() {
            for keys in &deep {
                insert_property_to(ty, keys, inexact);
            }
        }
    }
}

impl VisitMut<Type> for ObjectUnionNormalizer {
    fn visit_mut(&mut self, ty: &mut Type) {
        // TODO(kdy1): PERF
        ty.normalize_mut();

        ty.visit_mut_children_with(self);

        self.normalize_call_signatures(ty);
        self.normalize_fn_types(ty);
    }
}

impl VisitMut<Union> for ObjectUnionNormalizer {
    fn visit_mut(&mut self, u: &mut Union) {
        u.visit_mut_children_with(self);

        // If an union does not contains object literals, skip it.
        if u.types.iter().all(|ty| !ty.is_type_lit()) {
            return;
        }

        if u.types.len() > 1 {
            self.normalize_keys(&mut u.types);
        }
    }
}
