use rnode::{Fold, FoldWith, VisitMutWith};
use stc_ts_ast_rnode::{RNumber, RStr, RTsLit};
use stc_ts_env::Env;
use stc_ts_errors::debug::dump_type_as_string;
use stc_ts_type_ops::{is_str_lit_or_union, PreventComplexSimplification};
use stc_ts_types::{
    Array, Class, ClassDef, ClassMember, CommonTypeMetadata, IndexedAccessType,
    IndexedAccessTypeMetadata, Key, KeywordType, KeywordTypeMetadata, LitType, LitTypeMetadata,
    Mapped, Operator, PropertySignature, TypeElement, TypeLit, TypeLitMetadata, TypeParam, Union,
};
use stc_utils::ext::TypeVecExt;
use swc_atoms::js_word;
use swc_common::{EqIgnoreSpan, Spanned};
use swc_ecma_ast::{TsKeywordTypeKind, TsTypeOperatorOp};
use tracing::{info, trace};

use crate::{analyzer::Analyzer, ty::Type};

impl Analyzer<'_, '_> {
    /// TODO(kdy1): Remove this.
    ///
    /// Check if it's okay to generalize `ty`.
    #[cfg_attr(debug_assertions, tracing::instrument(skip_all))]
    pub(super) fn may_generalize(&self, ty: &Type) -> bool {
        trace!("may_generalize({:?})", ty);
        match ty.n() {
            Type::Function(f) => {
                if !self.may_generalize(&f.ret_ty) {
                    return false;
                }
                for param in &f.params {
                    if !self.may_generalize(&param.ty) {
                        return false;
                    }
                }
            }
            Type::Union(u) => {
                if u.types.iter().any(|ty| !self.may_generalize(ty)) {
                    return false;
                }
            }

            _ => {}
        }

        !ty.metadata().prevent_generalization
    }

    #[cfg_attr(debug_assertions, tracing::instrument(skip_all))]
    pub(super) fn prevent_inference_while_simplifying(&self, ty: &mut Type) {
        ty.visit_mut_with(&mut PreventComplexSimplification);
    }

    #[cfg_attr(debug_assertions, tracing::instrument(skip_all))]
    pub(super) fn simplify(&self, ty: Type) -> Type {
        info!("Simplifying {}", dump_type_as_string(&self.cm, &ty));
        ty.fold_with(&mut Simplifier { env: &self.env })
    }
}

/// Simplifies the type.
struct Simplifier<'a> {
    env: &'a Env,
}

impl Simplifier<'_> {
    fn should_skip_inference(&mut self, ty: &Type) -> bool {
        ty.metadata().prevent_complex_simplification
    }
}

impl Fold<Union> for Simplifier<'_> {
    fn fold(&mut self, mut union: Union) -> Union {
        let should_remove_null_and_undefined = union.types.iter().any(|ty| match ty.n() {
            Type::TypeLit(..) => true,
            Type::Ref(..) => true,
            Type::Function(..) => true,
            _ => false,
        });

        if should_remove_null_and_undefined {
            union.types.retain(|ty| {
                if ty.is_kwd(TsKeywordTypeKind::TsNullKeyword)
                    | ty.is_kwd(TsKeywordTypeKind::TsUndefinedKeyword)
                {
                    return false;
                }

                true
            });
        }

        let has_array = union.types.iter().any(|ty| match ty.n() {
            Type::Array(..) => true,
            _ => false,
        });

        // Remove empty tuple
        if has_array {
            union.types.retain(|ty| match ty.n() {
                Type::Tuple(tuple) => !tuple.elems.is_empty(),
                _ => true,
            });
        }

        union.types.dedup_type();

        union
    }
}

impl Fold<Type> for Simplifier<'_> {
    fn fold(&mut self, mut ty: Type) -> Type {
        // TODO(kdy1): PERF
        ty.nm();

        match ty.n() {
            Type::Array(Array {
                elem_type:
                    box Type::IndexedAccessType(IndexedAccessType {
                        obj_type: box Type::Param(..),
                        index_type:
                            box Type::Param(TypeParam {
                                constraint:
                                    Some(box Type::Operator(Operator {
                                        op: TsTypeOperatorOp::KeyOf,
                                        ty: box Type::Param(..),
                                        ..
                                    })),
                                ..
                            }),
                        ..
                    }),
                ..
            }) => return ty,

            _ => {}
        }

        if self.should_skip_inference(&ty) {
            match ty.n() {
                Type::IndexedAccessType(IndexedAccessType {
                    obj_type:
                        box Type::Param(TypeParam {
                            constraint: Some(..),
                            ..
                        }),
                    ..
                }) => {
                    return ty;
                }
                _ => {}
            }
        }

        ty = ty.fold_children_with(self);

        match ty {
            Type::IndexedAccessType(IndexedAccessType {
                span,
                readonly,
                obj_type: box Type::Keyword(k),
                index_type,
                metadata,
                ..
            }) => {
                let obj_type = self
                    .env
                    .get_global_type(
                        span,
                        &match k.kind {
                            TsKeywordTypeKind::TsAnyKeyword => {
                                return Type::any(
                                    span,
                                    KeywordTypeMetadata {
                                        common: metadata.common,
                                        ..Default::default()
                                    },
                                )
                            }
                            TsKeywordTypeKind::TsUnknownKeyword => {
                                return Type::unknown(
                                    span,
                                    KeywordTypeMetadata {
                                        common: metadata.common,
                                        ..Default::default()
                                    },
                                )
                            }
                            TsKeywordTypeKind::TsNeverKeyword => {
                                return Type::never(
                                    span,
                                    KeywordTypeMetadata {
                                        common: metadata.common,
                                        ..Default::default()
                                    },
                                )
                            }
                            TsKeywordTypeKind::TsIntrinsicKeyword => {
                                return Type::Keyword(KeywordType {
                                    span,
                                    kind: TsKeywordTypeKind::TsIntrinsicKeyword,
                                    metadata: KeywordTypeMetadata {
                                        common: metadata.common,
                                        ..Default::default()
                                    },
                                })
                            }
                            TsKeywordTypeKind::TsNumberKeyword => js_word!("Number"),
                            TsKeywordTypeKind::TsObjectKeyword => js_word!("Object"),
                            TsKeywordTypeKind::TsBooleanKeyword => js_word!("Boolean"),
                            TsKeywordTypeKind::TsStringKeyword => js_word!("String"),
                            TsKeywordTypeKind::TsSymbolKeyword => js_word!("Symbol"),
                            TsKeywordTypeKind::TsVoidKeyword
                            | TsKeywordTypeKind::TsUndefinedKeyword
                            | TsKeywordTypeKind::TsNullKeyword
                            | TsKeywordTypeKind::TsBigIntKeyword => {
                                return Type::IndexedAccessType(IndexedAccessType {
                                    span,
                                    readonly,
                                    obj_type: box Type::Keyword(k),
                                    index_type,
                                    metadata: IndexedAccessTypeMetadata {
                                        common: metadata.common,
                                        ..Default::default()
                                    },
                                })
                            }
                        },
                    )
                    .unwrap();

                let s = match index_type.n() {
                    Type::Lit(LitType {
                        lit: RTsLit::Str(s),
                        ..
                    }) => s.clone(),
                    _ => {
                        return Type::IndexedAccessType(IndexedAccessType {
                            span,
                            readonly,
                            obj_type: box Type::Keyword(k),
                            index_type,
                            metadata,
                        })
                    }
                };

                match obj_type.n() {
                    Type::Interface(i) => {
                        for element in &i.body {
                            match element {
                                TypeElement::Property(PropertySignature {
                                    key,
                                    type_ann: Some(type_ann),
                                    ..
                                }) if *key == s.value => return *type_ann.clone(),
                                TypeElement::Method(_) => {}

                                _ => {}
                            }
                        }
                    }
                    _ => unreachable!("Keyword types should have corresponding interfaced defined"),
                }

                return Type::IndexedAccessType(IndexedAccessType {
                    span,
                    readonly,
                    obj_type: box obj_type,
                    index_type,
                    metadata,
                });
            }

            Type::Union(u) if u.types.is_empty() => {
                return Type::never(
                    u.span,
                    KeywordTypeMetadata {
                        common: u.metadata.common,
                        ..Default::default()
                    },
                );
            }

            Type::Intersection(ref i) => {
                let is_str = i.types.iter().any(|ty| ty.is_str());
                let is_num = i.types.iter().any(|ty| ty.is_num());
                let is_bool = i.types.iter().any(|ty| ty.is_bool());

                // LHS is never.
                if u32::from(is_str) + u32::from(is_num) + u32::from(is_bool) >= 2 {
                    return Type::never(
                        i.span,
                        KeywordTypeMetadata {
                            common: i.metadata.common,
                        },
                    );
                }
            }

            Type::Mapped(Mapped {
                span,
                type_param:
                    TypeParam {
                        name,
                        constraint: Some(constraint),
                        ..
                    },
                ty,
                ..
            }) if is_str_lit_or_union(&constraint) => {
                let members = constraint
                    .iter_union()
                    .filter_map(|ty| match ty.n() {
                        Type::Lit(LitType {
                            lit: RTsLit::Str(s),
                            ..
                        }) => Some(s),
                        _ => None,
                    })
                    .map(|key| {
                        TypeElement::Property(PropertySignature {
                            span,
                            accessibility: None,
                            // TODO(kdy1):
                            readonly: false,
                            key: Key::Normal {
                                span: key.span,
                                sym: key.value.clone(),
                            },
                            optional: false,
                            params: Default::default(),
                            type_ann: ty.clone(),
                            type_params: Default::default(),
                            metadata: Default::default(),
                            accessor: Default::default(),
                        })
                    })
                    .collect();

                return Type::TypeLit(TypeLit {
                    span,
                    members,
                    metadata: Default::default(),
                });
            }

            // TODO(kdy1): Handle optional and reaonly
            Type::Mapped(Mapped {
                type_param:
                    TypeParam {
                        name: p1,
                        constraint:
                            Some(box Type::Operator(Operator {
                                op: TsTypeOperatorOp::KeyOf,
                                ty,
                                ..
                            })),
                        ..
                    },
                ty:
                    Some(box Type::IndexedAccessType(IndexedAccessType {
                        obj_type,
                        index_type: box Type::Param(p2),
                        ..
                    })),
                ..
            }) if p1 == p2.name && obj_type == ty => return *ty,

            Type::IndexedAccessType(IndexedAccessType {
                span,
                readonly,
                obj_type: box Type::Intersection(obj),
                index_type:
                    index_type @ box Type::Lit(LitType {
                        lit: RTsLit::Str(..),
                        ..
                    }),
                metadata,
                ..
            }) if obj.types.iter().all(|ty| match ty.n() {
                Type::TypeLit(..) => true,
                Type::Keyword(KeywordType {
                    kind: TsKeywordTypeKind::TsUnknownKeyword,
                    ..
                }) => true,
                _ => false,
            }) =>
            {
                let inexact = obj
                    .types
                    .iter()
                    .filter_map(|ty| match ty.n() {
                        Type::TypeLit(ty) => Some(ty),
                        _ => None,
                    })
                    .any(|ty| ty.metadata.inexact);
                let mut members = obj
                    .types
                    .into_iter()
                    .filter_map(|ty| {
                        // TODO(kdy1): PERF
                        match ty.foldable() {
                            Type::TypeLit(ty) => Some(ty.members),
                            _ => None,
                        }
                    })
                    .flatten()
                    .collect::<Vec<_>>();

                members.dedup_by(|a, b| {
                    if let Some(a_key) = a.key() {
                        if let Some(b_key) = b.key() {
                            if a_key.eq_ignore_span(&*b_key) {
                                true
                            } else {
                                false
                            }
                        } else {
                            false
                        }
                    } else {
                        false
                    }
                });

                return Type::IndexedAccessType(IndexedAccessType {
                    span,
                    readonly,
                    obj_type: box Type::TypeLit(TypeLit {
                        span,
                        members,
                        metadata: TypeLitMetadata {
                            inexact,
                            ..Default::default()
                        },
                    }),
                    index_type,
                    metadata,
                })
                .fold_with(self);
            }

            Type::IndexedAccessType(IndexedAccessType {
                span,
                readonly,
                obj_type: box Type::Union(obj),
                index_type:
                    box Type::Lit(LitType {
                        lit: RTsLit::Str(s),
                        ..
                    }),
                ..
            }) if obj.types.iter().all(|ty| match ty.n() {
                Type::TypeLit(..) => true,
                _ => false,
            }) =>
            {
                let mut types = obj
                    .types
                    .into_iter()
                    .map(|ty| match ty.foldable() {
                        Type::TypeLit(ty) => ty.members,
                        _ => unreachable!(),
                    })
                    .flatten()
                    .filter_map(|element| {
                        let span = element.span();

                        match element {
                            TypeElement::Property(p) if p.key == s.value => Some(
                                p.type_ann
                                    .map(|v| *v)
                                    .unwrap_or_else(|| Type::any(span, Default::default())),
                            ),
                            _ => None,
                        }
                    })
                    .collect::<Vec<_>>();

                types.dedup_type();

                return Type::union(types);
            }

            Type::IndexedAccessType(IndexedAccessType {
                span,
                obj_type: box Type::Tuple(tuple),
                index_type:
                    box Type::Lit(LitType {
                        lit:
                            RTsLit::Str(RStr {
                                value: js_word!("length"),
                                ..
                            }),
                        ..
                    }),
                metadata,
                ..
            }) => {
                return Type::Lit(LitType {
                    span,
                    lit: RTsLit::Number(RNumber {
                        span,
                        value: tuple.elems.len() as _,
                    }),
                    metadata: LitTypeMetadata {
                        common: CommonTypeMetadata {
                            prevent_generalization: true,
                            ..metadata.common
                        },
                        ..Default::default()
                    },
                });
            }

            Type::IndexedAccessType(IndexedAccessType {
                span,
                obj_type: box Type::Array(..),
                index_type:
                    box Type::Lit(LitType {
                        lit:
                            RTsLit::Str(RStr {
                                value: js_word!("length"),
                                ..
                            }),
                        ..
                    }),
                metadata,
                ..
            }) => {
                return Type::Keyword(KeywordType {
                    span,
                    kind: TsKeywordTypeKind::TsNumberKeyword,
                    metadata: KeywordTypeMetadata {
                        common: metadata.common,
                        ..Default::default()
                    },
                });
            }

            Type::IndexedAccessType(IndexedAccessType {
                span,
                obj_type: box Type::Tuple(tuple),
                index_type:
                    box Type::Lit(LitType {
                        lit: RTsLit::Str(RStr { value, .. }),
                        ..
                    }),
                metadata,
                ..
            }) if value.parse::<usize>().is_ok() => {
                let idx = value.parse::<usize>().unwrap();
                return tuple
                    .elems
                    .into_iter()
                    .map(|el| *el.ty)
                    .nth(idx)
                    .unwrap_or_else(|| {
                        Type::never(
                            span,
                            KeywordTypeMetadata {
                                common: metadata.common,
                                ..Default::default()
                            },
                        )
                    });
            }
            Type::Union(ty) if ty.types.len() == 1 => return ty.types.into_iter().next().unwrap(),

            // Convert
            //
            //
            // [P in "value"]: {
            //     value: {
            //         [P in "value"]: T[P];
            //     };
            // }[P];
            //
            //
            // into
            //
            // [P in "value"]: {
            //     value: T['value'];
            // }[P];
            //
            // and then into
            //
            // [P in "value"]: {
            //     value: T['value'];
            // }['value'];
            Type::Mapped(Mapped {
                span,
                ty:
                    Some(box Type::IndexedAccessType(IndexedAccessType {
                        obj_type:
                            box Type::TypeLit(TypeLit {
                                metadata: obj_type_metadata,
                                members,
                                ..
                            }),
                        index_type: box Type::Param(index_type),
                        ..
                    })),
                type_param:
                    TypeParam {
                        name,
                        constraint: Some(constraint),
                        ..
                    },
                ..
            }) if index_type.name == name && is_str_lit_or_union(&constraint) => {
                let mut new_members = vec![];

                for member in &members {
                    for key in constraint.iter_union() {
                        let key = match key.n() {
                            Type::Lit(LitType {
                                lit: RTsLit::Str(v),
                                ..
                            }) => v.clone(),
                            _ => unreachable!(),
                        };

                        if let Some(member_key) = member.key() {
                            if *member_key != key.value {
                                continue;
                            }
                        }

                        match member {
                            TypeElement::Call(_) => unimplemented!(),
                            TypeElement::Constructor(_) => unimplemented!(),
                            TypeElement::Property(p) => new_members.push(p.clone().into()),
                            TypeElement::Method(_) => {
                                unimplemented!("Expansion of TypeElement::Method")
                            }
                            TypeElement::Index(_) => unimplemented!(),
                        }
                    }
                }

                return Type::TypeLit(TypeLit {
                    span,
                    members: new_members,
                    metadata: obj_type_metadata,
                });
            }

            Type::IndexedAccessType(IndexedAccessType {
                obj_type:
                    box Type::Param(TypeParam {
                        constraint: Some(box Type::TypeLit(TypeLit { members, .. })),
                        ..
                    }),
                index_type:
                    box Type::Lit(LitType {
                        lit: RTsLit::Str(v),
                        ..
                    }),
                ..
            })
            | Type::IndexedAccessType(IndexedAccessType {
                obj_type: box Type::TypeLit(TypeLit { members, .. }),
                index_type:
                    box Type::Lit(LitType {
                        lit: RTsLit::Str(v),
                        ..
                    }),
                ..
            }) if members
                .iter()
                .any(|element| match element.key().as_deref() {
                    Some(key) => *key == v.value,
                    _ => false,
                }) =>
            {
                let el = members
                    .into_iter()
                    .find(|element| match element.key().as_deref() {
                        Some(key) => *key == v.value,
                        _ => false,
                    })
                    .unwrap();

                match el {
                    TypeElement::Call(_) => {
                        unimplemented!("Generic mapped type inference involving `Call` element")
                    }
                    TypeElement::Constructor(_) => {
                        unimplemented!(
                            "Generic mapped type inference involving `Constructor` element"
                        )
                    }
                    TypeElement::Property(p) => {
                        let span = p.span;
                        return p
                            .type_ann
                            .map(|v| *v)
                            .unwrap_or_else(|| Type::any(span, Default::default()));
                    }
                    TypeElement::Method(_) => {
                        unimplemented!("Generic mapped type inference involving `Method` element")
                    }
                    TypeElement::Index(_) => {
                        unimplemented!(
                            "Generic mapped type inference involving IndexSignature element"
                        )
                    }
                }
            }

            Type::IndexedAccessType(IndexedAccessType {
                obj_type:
                    box Type::Class(Class {
                        def: box ClassDef { body, .. },
                        ..
                    }),
                index_type:
                    box Type::Lit(LitType {
                        lit: RTsLit::Str(s),
                        ..
                    }),
                ..
            }) if body.iter().any(|member| match member {
                ClassMember::Constructor(_) => false,
                ClassMember::Method(_) => false,
                ClassMember::Property(p) => p.key == s.value,
                ClassMember::IndexSignature(_) => false,
            }) =>
            {
                let member = body
                    .into_iter()
                    .find(|member| match member {
                        ClassMember::Constructor(_) => false,
                        ClassMember::Method(_) => false,
                        ClassMember::Property(p) => p.key == s.value,
                        ClassMember::IndexSignature(_) => false,
                    })
                    .unwrap();

                match member {
                    ClassMember::Method(_) => unimplemented!(),
                    ClassMember::Property(p) => {
                        if let Some(value) = p.value {
                            return *value;
                        }

                        return Type::any(p.span, Default::default());
                    }
                    ClassMember::Constructor(_) => unreachable!(),
                    ClassMember::IndexSignature(_) => unreachable!(),
                }
            }

            Type::IndexedAccessType(IndexedAccessType {
                obj_type:
                    box Type::Class(Class {
                        def: box ClassDef { body, .. },
                        ..
                    }),
                index_type: box Type::Union(keys),
                ..
            }) if keys.types.iter().all(|ty| is_str_lit_or_union(&ty)) => {
                let mut new_types = keys
                    .types
                    .into_iter()
                    .map(|key| match key.n() {
                        Type::Lit(LitType {
                            lit: RTsLit::Str(s),
                            ..
                        }) => s.clone(),
                        _ => unreachable!(),
                    })
                    .map(|key| {
                        let member = body
                            .iter()
                            .find(|member| match member {
                                ClassMember::Constructor(_) => false,
                                ClassMember::Method(_) => false,
                                ClassMember::Property(p) => p.key == key.value,
                                ClassMember::IndexSignature(_) => false,
                            })
                            .unwrap();

                        match member {
                            ClassMember::Method(_) => unimplemented!(),
                            ClassMember::Property(p) => {
                                if let Some(value) = &p.value {
                                    return *value.clone();
                                }

                                return Type::any(p.span, Default::default());
                            }
                            ClassMember::Constructor(_) => unreachable!(),
                            ClassMember::IndexSignature(_) => unreachable!(),
                        }
                    })
                    .collect::<Vec<_>>();

                new_types.dedup_type();

                return Type::union(new_types);
            }

            _ => {}
        }

        ty
    }
}
