use std::iter::repeat;

use indexmap::IndexSet;
use rnode::{FoldWith, NodeId, VisitMut, VisitMutWith};
use rustc_hash::FxHashMap;
use stc_ts_ast_rnode::{RBindingIdent, RIdent, RPat};
use stc_ts_generics::type_param::replacer::TypeParamReplacer;
use stc_ts_types::{
    CallSignature, FnParam, Function, FunctionMetadata, Key, KeywordType, PropertySignature, Type,
    TypeElement, TypeLit, TypeLitMetadata, TypeParamDecl, Union,
};
use stc_utils::{cache::Freeze, ext::TypeVecExt};
use swc_atoms::JsWord;
use swc_common::DUMMY_SP;
use swc_ecma_ast::TsKeywordTypeKind;
use tracing::instrument;

pub struct UnionNormalizer {
    pub preserve_specified: bool,
}

impl UnionNormalizer {
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
        if !ty.is_union_type() {
            return;
        }
        ty.make_clone_cheap();
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
                .collect(),
            ret_ty: box Type::union(return_types),
            metadata: FunctionMetadata {
                common: u.metadata.common,
                ..Default::default()
            },
        })
    }

    /// TODO(kdy1): Add type parameters.
    fn normalize_call_signatures(&self, ty: &mut Type) {
        if !ty.normalize().is_union_type() {
            return;
        }
        ty.make_clone_cheap();

        let u = match ty.normalize_mut() {
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
                                        new_type_params
                                            .entry(i)
                                            .or_insert_with(|| type_params.clone());
                                    }
                                }

                                // Parameters are intersectioned, and return
                                // types are unioned.

                                for (idx, param) in params.into_iter().enumerate() {
                                    let new_params = new_params.entry(i).or_default();
                                    if new_params.len() <= idx {
                                        new_params.extend(
                                            repeat(vec![]).take(idx + 1 - new_params.len()),
                                        );
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
                                    extra_members.extend(
                                        repeat(vec![]).take(type_idx + 1 - extra_members.len()),
                                    );
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

    #[instrument(skip(self, u))]
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
            if matches!(ty.normalize(), Type::TypeLit(..)) {
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
                                    type_ann: Some(box Type::Keyword(KeywordType {
                                        span: DUMMY_SP,
                                        kind: swc_ecma_ast::TsKeywordTypeKind::TsUndefinedKeyword,
                                        metadata: Default::default(),
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
}

impl VisitMut<Type> for UnionNormalizer {
    fn visit_mut(&mut self, ty: &mut Type) {
        // TODO(kdy1): PERF
        ty.normalize_mut();

        ty.visit_mut_children_with(self);

        self.normalize_call_signatures(ty);
        self.normalize_fn_types(ty);
    }
}

impl VisitMut<Union> for UnionNormalizer {
    fn visit_mut(&mut self, u: &mut Union) {
        u.visit_mut_children_with(self);

        // If an union does not contains object literals, skip it.
        if u.types.iter().all(|ty| !ty.normalize().is_type_lit()) {
            return;
        }

        self.normalize_keys(u);
    }
}
