use std::borrow::Cow;

use itertools::Itertools;
use stc_ts_ast_rnode::{RIdent, RTsEntityName, RTsLit};
use stc_ts_errors::{debug::dump_type_as_string, DebugExt};
use stc_ts_type_ops::is_str_lit_or_union;
use stc_ts_types::{
    Class, ClassMember, ClassProperty, KeywordType, KeywordTypeMetadata, Method, MethodSignature, PropertySignature, Ref, Type,
    TypeElement, Union,
};
use stc_utils::{cache::Freeze, debug_ctx, ext::TypeVecExt, try_cache};
use swc_atoms::js_word;
use swc_common::{Span, SyntaxContext, TypeEq, DUMMY_SP};
use swc_ecma_ast::TsKeywordTypeKind;

use crate::{
    analyzer::{types::NormalizeTypeOpts, Analyzer},
    VResult,
};

impl Analyzer<'_, '_> {
    /// Evaluates `keyof` operator.
    ///
    /// # Parameters
    ///
    /// ## `ty`
    /// Should be operand of `keyof`.
    pub(crate) fn keyof(&mut self, span: Span, ty: &Type) -> VResult<Type> {
        let span = span.with_ctxt(SyntaxContext::empty());

        let _ctx = debug_ctx!(format!("keyof: {}", dump_type_as_string(&self.cm, ty)));

        if !self.is_builtin {
            debug_assert!(!span.is_dummy(), "Cannot perform `keyof` operation with dummy span");
        }

        let ty = (|| -> VResult<_> {
            let mut ty = self
                .normalize(
                    Some(span),
                    Cow::Borrowed(ty),
                    NormalizeTypeOpts {
                        process_only_key: true,
                        ..Default::default()
                    },
                )
                .context("tried to normalize")?;

            if matches!(ty.normalize(), Type::TypeLit(..)) {
                ty.make_clone_cheap()
            }

            match ty.normalize() {
                Type::Lit(ty) => {
                    return self
                        .keyof(
                            span,
                            &Type::Keyword(KeywordType {
                                span: ty.span,
                                kind: match &ty.lit {
                                    RTsLit::BigInt(_) => TsKeywordTypeKind::TsBigIntKeyword,
                                    RTsLit::Number(_) => TsKeywordTypeKind::TsNumberKeyword,
                                    RTsLit::Str(_) => TsKeywordTypeKind::TsStringKeyword,
                                    RTsLit::Bool(_) => TsKeywordTypeKind::TsBooleanKeyword,
                                    RTsLit::Tpl(_) => unreachable!(),
                                },
                                metadata: KeywordTypeMetadata {
                                    common: ty.metadata.common,
                                },
                            }),
                        )
                        .context("tried applying `keyof` to a literal by delegating to keyword type handler")
                }
                Type::Keyword(KeywordType { kind, .. }) => match kind {
                    TsKeywordTypeKind::TsAnyKeyword => {
                        let string = Type::Keyword(KeywordType {
                            span,
                            kind: TsKeywordTypeKind::TsStringKeyword,
                            metadata: Default::default(),
                        });
                        let number = Type::Keyword(KeywordType {
                            span,
                            kind: TsKeywordTypeKind::TsNumberKeyword,
                            metadata: Default::default(),
                        });
                        let symbol = Type::Keyword(KeywordType {
                            span,
                            kind: TsKeywordTypeKind::TsSymbolKeyword,
                            metadata: Default::default(),
                        });
                        return Ok(Type::Union(Union {
                            span,
                            types: vec![string, number, symbol],
                            metadata: Default::default(),
                        }));
                    }
                    TsKeywordTypeKind::TsVoidKeyword
                    | TsKeywordTypeKind::TsUndefinedKeyword
                    | TsKeywordTypeKind::TsNullKeyword
                    | TsKeywordTypeKind::TsUnknownKeyword
                    | TsKeywordTypeKind::TsObjectKeyword => {
                        return Ok(Type::Keyword(KeywordType {
                            span,
                            kind: TsKeywordTypeKind::TsNeverKeyword,
                            metadata: Default::default(),
                        }));
                    }
                    TsKeywordTypeKind::TsNumberKeyword | TsKeywordTypeKind::TsBooleanKeyword | TsKeywordTypeKind::TsStringKeyword => {
                        let name = match kind {
                            TsKeywordTypeKind::TsNumberKeyword => {
                                js_word!("Number")
                            }
                            TsKeywordTypeKind::TsBooleanKeyword => {
                                js_word!("Boolean")
                            }
                            TsKeywordTypeKind::TsStringKeyword => {
                                js_word!("String")
                            }
                            _ => unreachable!(),
                        };
                        return self
                            .keyof(
                                span,
                                &Type::Ref(Ref {
                                    span,
                                    type_name: RTsEntityName::Ident(RIdent::new(name, DUMMY_SP)),
                                    type_args: None,
                                    metadata: Default::default(),
                                }),
                            )
                            .context("tried to get keys of builitin interface types");
                    }

                    TsKeywordTypeKind::TsBigIntKeyword => {}
                    TsKeywordTypeKind::TsSymbolKeyword => {}
                    TsKeywordTypeKind::TsNeverKeyword => {
                        return Ok(Type::Union(Union {
                            span,
                            types: vec![
                                Type::Keyword(KeywordType {
                                    span,
                                    kind: TsKeywordTypeKind::TsStringKeyword,
                                    metadata: Default::default(),
                                }),
                                Type::Keyword(KeywordType {
                                    span,
                                    kind: TsKeywordTypeKind::TsNumberKeyword,
                                    metadata: Default::default(),
                                }),
                                Type::Keyword(KeywordType {
                                    span,
                                    kind: TsKeywordTypeKind::TsSymbolKeyword,
                                    metadata: Default::default(),
                                }),
                            ],
                            metadata: Default::default(),
                        }))
                    }
                    TsKeywordTypeKind::TsIntrinsicKeyword => {}
                },

                Type::TypeLit(l) => {
                    return Ok(try_cache!(self.data.cache.keyof_type_lit, ty.clone().into_owned(), {
                        let mut types = vec![];
                        for member in &l.members {
                            match member {
                                TypeElement::Property(PropertySignature { key, .. }) | TypeElement::Method(MethodSignature { key, .. }) => {
                                    if !key.is_computed() {
                                        types.push(key.ty().into_owned());
                                    }
                                }

                                TypeElement::Index(i) => {
                                    // TODO(kdy1): Check if this is correct.
                                    if let Some(p) = i.params.first() {
                                        types.push(*p.ty.clone());
                                    }
                                }

                                TypeElement::Call(_) | TypeElement::Constructor(_) => {}
                            }
                        }
                        Ok(Type::new_union(span, types))
                    }));
                }

                Type::Class(Class { def, .. }) => {
                    // TODO(kdy1): Add `KeyOfOpts` to control thi
                    //
                    // Class instance cannot be operand, but it can be passed as argument while
                    // normalizing.
                    return self.keyof(span, &Type::ClassDef(*def.clone()));
                }

                Type::ClassDef(cls) => {
                    let mut key_types = vec![];
                    for member in &cls.body {
                        match member {
                            ClassMember::Property(ClassProperty { key, .. }) | ClassMember::Method(Method { key, .. }) => {
                                if !key.is_computed() {
                                    key_types.push(key.ty().into_owned());
                                }
                            }
                            ClassMember::Constructor(_) => {}
                            ClassMember::IndexSignature(i) => {
                                if let Some(p) = i.params.first() {
                                    key_types.push(*p.ty.clone());
                                }
                            }
                        }
                    }

                    if key_types.is_empty() {
                        return Ok(Type::never(span, Default::default()));
                    }

                    return Ok(Type::Union(Union {
                        span,
                        types: key_types,

                        metadata: Default::default(),
                    }));
                }

                Type::Array(..) | Type::Tuple(..) => {
                    return self
                        .keyof(
                            span,
                            &Type::Ref(Ref {
                                span,
                                type_name: RTsEntityName::Ident(RIdent::new(js_word!("Array"), DUMMY_SP)),
                                type_args: None,
                                metadata: Default::default(),
                            }),
                        )
                        .context("tried to get keys of Array (builtin)");
                }

                Type::Interface(..) | Type::Enum(..) => {
                    let ty = self
                        .convert_type_to_type_lit(span, ty)?
                        .map(Cow::into_owned)
                        .map(Type::TypeLit)
                        .unwrap();

                    return self
                        .keyof(span, &ty)
                        .context("tried to evaluate `keyof` for type literal created with type_to_type_lit");
                }

                Type::Intersection(i) => {
                    // We return union of keys.
                    let types: Vec<_> = i
                        .types
                        .iter()
                        .map(|ty| {
                            self.keyof(span, ty)
                                .context("tried to get keys of an element of an intersection type")
                        })
                        .collect::<Result<_, _>>()?;

                    return Ok(Type::new_union(span, types));
                }

                Type::Union(u) => {
                    // We return intersection of keys.
                    let key_types = u
                        .types
                        .iter()
                        .map(|ty| self.keyof(span, ty).context("tried to get keys of an element of a union type"))
                        .collect::<Result<Vec<_>, _>>()?;

                    if key_types.iter().all(is_str_lit_or_union) {
                        let mut keys = key_types
                            .into_iter()
                            .map(|ty| match ty.foldable() {
                                Type::Union(ty) => ty.types.into_iter().map(|ty| ty.foldable().lit().unwrap()).collect_vec(),
                                Type::Lit(l) => vec![l],
                                _ => {
                                    unreachable!()
                                }
                            })
                            .collect_vec();

                        keys[0].dedup_type();

                        let actual_keys = keys[0]
                            .iter()
                            .filter(|&key| keys[1..].iter().all(|keys| keys.iter().any(|other_key| key.type_eq(other_key))))
                            .cloned()
                            .map(Type::Lit)
                            .collect_vec();

                        return Ok(Type::new_union_without_dedup(span, actual_keys));
                    }

                    return Ok(Type::new_union(span, key_types));
                }

                Type::Param(..) => {
                    return Ok(Type::Keyword(KeywordType {
                        span,
                        kind: TsKeywordTypeKind::TsStringKeyword,
                        metadata: Default::default(),
                    }))
                }

                Type::Mapped(m) => {
                    //
                    if let Some(ty) = m.type_param.constraint.as_deref() {
                        return self.keyof(span, ty);
                    }
                }

                _ => {}
            }

            unimplemented!("keyof: {}", dump_type_as_string(&self.cm, &ty));
        })()?;

        ty.assert_valid();

        Ok(ty)
    }
}
