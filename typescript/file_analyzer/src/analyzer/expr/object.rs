use crate::analyzer::Analyzer;
use crate::analyzer::ScopeKind;
use crate::util::type_ext::TypeVecExt;
use crate::validator::ValidateWith;
use crate::ValidationResult;
use fxhash::FxHashMap;
use indexmap::IndexSet;
use itertools::Itertools;
use rnode::FoldWith;
use rnode::VisitMut;
use rnode::VisitMutWith;
use stc_ts_ast_rnode::RIdent;
use stc_ts_ast_rnode::RObjectLit;
use stc_ts_ast_rnode::RPat;
use stc_ts_ast_rnode::RPropOrSpread;
use stc_ts_ast_rnode::RSpreadElement;
use stc_ts_ast_rnode::RTsKeywordType;
use stc_ts_file_analyzer_macros::validator;
use stc_ts_generics::type_param::replacer::TypeParamReplacer;
use stc_ts_types::CallSignature;
use stc_ts_types::FnParam;
use stc_ts_types::Key;
use stc_ts_types::PropertySignature;
use stc_ts_types::Type;
use stc_ts_types::TypeElement;
use stc_ts_types::TypeLit;
use stc_ts_types::TypeParamDecl;
use stc_ts_types::Union;
use std::iter::repeat;
use swc_atoms::JsWord;
use swc_common::DUMMY_SP;

#[validator]
impl Analyzer<'_, '_> {
    fn validate(&mut self, node: &RObjectLit) -> ValidationResult {
        self.with_child(ScopeKind::ObjectLit, Default::default(), |a: &mut Analyzer| {
            let mut ty = box Type::TypeLit(TypeLit {
                span: node.span,
                members: vec![],
            });

            for prop in node.props.iter() {
                ty = a.append_prop_or_spread_to_type(ty, prop)?;
            }

            Ok(ty)
        })
    }
}

struct ObjectUnionNormalizer<'a, 'b, 'c> {
    anaylzer: &'a mut Analyzer<'b, 'c>,
}

impl ObjectUnionNormalizer<'_, '_, '_> {
    /// We need to know shape of normalized type literal.
    ///
    /// We use indexset to remove duplicate while preserving order.
    fn find_keys(&self, types: &[Box<Type>]) -> IndexSet<JsWord> {
        types
            .iter()
            .filter_map(|ty| match &**ty {
                Type::TypeLit(ty) => Some(&ty.members),
                _ => None,
            })
            .flatten()
            .filter_map(|member| member.non_computed_key().cloned())
            .collect()
    }

    /// TODO: Add type parameters.
    fn normalize_call_signatures(&self, ty: &mut Type) {
        let u = match ty {
            Type::Union(u) => u,
            _ => return,
        };

        let mut new_type_params = FxHashMap::<_, TypeParamDecl>::default();
        let mut new_params = FxHashMap::<_, Vec<_>>::default();
        let mut new_return_types = FxHashMap::<_, Vec<_>>::default();
        let mut extra_members = vec![];
        //
        for (type_idx, ty) in u.types.iter().enumerate() {
            match &**ty {
                Type::TypeLit(ty) => {
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
                                            .map(Box::new)
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

                                new_return_types.entry(i).or_default().extend(ret_ty.clone());
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
            let type_params = new_type_params.remove(&i);

            members.push(TypeElement::Call(CallSignature {
                span: DUMMY_SP,
                ret_ty: Some(Type::union(return_types)),
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

                            types.push(param.ty);
                        }
                        types.dedup_type();

                        let ty = Type::intersection(DUMMY_SP, types);
                        FnParam {
                            span: DUMMY_SP,
                            // TODO
                            required: true,
                            // TODO
                            pat: pat.unwrap_or_else(|| RPat::Ident(RIdent::new("a".into(), DUMMY_SP))),
                            ty,
                        }
                    })
                    .collect_vec(),
            }));
        }

        let new_lit = TypeLit { span: u.span, members };

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
            .map(Box::new)
            .collect_vec();

        u.types = new_types;
    }

    fn normalize_keys(&self, u: &mut Union) {
        let keys = self.find_keys(&u.types);

        // Add properties.
        for ty in u.types.iter_mut() {
            match ty.normalize_mut() {
                Type::TypeLit(ty) => {
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
                            }))
                        }
                    }
                }
                _ => {}
            }
        }
    }
}

impl VisitMut<Type> for ObjectUnionNormalizer<'_, '_, '_> {
    fn visit_mut(&mut self, ty: &mut Type) {
        ty.visit_mut_children_with(self);

        self.normalize_call_signatures(ty);
    }
}

impl VisitMut<Union> for ObjectUnionNormalizer<'_, '_, '_> {
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
    pub(super) fn normalize_union_of_objects(&mut self, ty: &mut Type) {
        ty.visit_mut_with(&mut ObjectUnionNormalizer { anaylzer: self });
    }

    fn append_prop_or_spread_to_type(&mut self, to: Box<Type>, prop: &RPropOrSpread) -> ValidationResult {
        match prop {
            RPropOrSpread::Spread(RSpreadElement { expr, .. }) => {
                let prop_ty: Box<Type> = expr.validate_with_default(self)?;
                self.append_type(to, prop_ty)
            }
            RPropOrSpread::Prop(prop) => {
                let p: TypeElement = prop.validate_with(self)?;
                self.append_type_element(to, p)
            }
        }
    }

    /// If rhs is an union type, return type will be union.
    ///
    /// `{ a: number } + ( {b: number} | { c: number } )` => `{ a: number, b:
    /// number } | { a: number, c: number }`
    fn append_type(&mut self, to: Box<Type>, rhs: Box<Type>) -> ValidationResult<Box<Type>> {
        if to.is_any() || to.is_unknown() {
            return Ok(to);
        }
        if rhs.is_any() || rhs.is_unknown() {
            return Ok(to);
        }
        let mut to = to.foldable();
        match to {
            Type::TypeLit(ref mut lit) => match *rhs {
                Type::TypeLit(rhs) => {
                    lit.members.extend(rhs.members);
                    return Ok(box to);
                }
                Type::Union(rhs) => {
                    return Ok(box Type::Union(Union {
                        span: lit.span,
                        types: rhs
                            .types
                            .into_iter()
                            .map(|rhs| self.append_type(box to.clone(), rhs))
                            .collect::<Result<_, _>>()?,
                    }))
                }
                _ => {}
            },

            Type::Union(to) => {
                return Ok(box Type::Union(Union {
                    span: to.span,
                    types: to
                        .types
                        .into_iter()
                        .map(|to| self.append_type(to, rhs.clone()))
                        .collect::<Result<_, _>>()?,
                }))
            }
            _ => {}
        }

        unimplemented!("append_type:\n{:?}\n{:?}", to, rhs)
    }

    fn append_type_element(&mut self, to: Box<Type>, rhs: TypeElement) -> ValidationResult<Box<Type>> {
        if to.is_any() || to.is_unknown() {
            return Ok(to);
        }
        let mut to = to.foldable();

        match to {
            Type::TypeLit(ref mut lit) => {
                // TODO: Remove previous member with same key.

                lit.members.push(rhs);
                Ok(box to)
            }
            Type::Union(to) => Ok(box Type::Union(Union {
                span: to.span,
                types: to
                    .types
                    .into_iter()
                    .map(|to| self.append_type_element(to, rhs.clone()))
                    .collect::<Result<_, _>>()?,
            })),
            _ => {
                unimplemented!("append_type_element\n{:?}\n{:?}", to, rhs)
            }
        }
    }
}
