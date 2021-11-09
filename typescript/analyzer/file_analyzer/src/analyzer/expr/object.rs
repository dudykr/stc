use crate::{
    analyzer::{Analyzer, ScopeKind},
    validator::ValidateWith,
    ValidationResult,
};
use rnode::VisitMutWith;
use stc_ts_ast_rnode::{RObjectLit, RPropOrSpread, RSpreadElement};
use stc_ts_errors::{DebugExt, Error};
use stc_ts_file_analyzer_macros::validator;
use stc_ts_type_ops::{union_normalization::UnionNormalizer, Fix};
use stc_ts_types::{
    Accessor, Key, MethodSignature, PropertySignature, Type, TypeElement, TypeLit, Union, UnionMetadata,
};
use stc_utils::cache::Freeze;
use std::{borrow::Cow, time::Instant};
use swc_common::{Spanned, SyntaxContext, TypeEq};
use swc_ecma_ast::TsKeywordTypeKind;
use tracing::{debug, instrument};

#[validator]
impl Analyzer<'_, '_> {
    fn validate(&mut self, node: &RObjectLit, type_ann: Option<&Type>) -> ValidationResult {
        let type_ann = self.expand_type_ann(node.span, type_ann)?;
        debug_assert_eq!(node.span.ctxt, SyntaxContext::empty());

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

            a.validate_type_literals(&ret, false);

            Ok(ret)
        })
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
    #[instrument(skip(self, ty, preserve_specified))]
    pub(super) fn normalize_union(&mut self, ty: &mut Type, preserve_specified: bool) {
        let start = Instant::now();
        ty.visit_mut_with(&mut UnionNormalizer { preserve_specified });

        let end = Instant::now();

        debug!("Normlaized unions (time = {:?})", end - start);
    }

    #[instrument(skip(self, ty, is_type_ann))]
    pub(crate) fn validate_type_literals(&mut self, ty: &Type, is_type_ann: bool) {
        match ty.normalize() {
            Type::Union(ty) => {
                for ty in &ty.types {
                    self.validate_type_literals(ty, is_type_ann);
                }
            }
            Type::TypeLit(ty) => {
                self.report_errors_for_mixed_optional_method_signatures(&ty.members);
            }
            _ => {}
        }
    }

    #[instrument(skip(self, elems))]
    pub(crate) fn report_errors_for_mixed_optional_method_signatures(&mut self, elems: &[TypeElement]) {
        let mut keys: Vec<(&Key, bool)> = vec![];
        for elem in elems {
            match elem {
                TypeElement::Method(MethodSignature { key, optional, .. }) => {
                    if let Some(prev) = keys.iter().find(|v| v.0.type_eq(key)) {
                        if *optional != prev.1 {
                            self.storage
                                .report(Error::OptionalAndNonOptionalMethodPropertyMixed { span: key.span() });
                            continue;
                        }
                    }

                    keys.push((key, *optional));
                }
                _ => {}
            }
        }
    }

    #[instrument(skip(self, known_keys, to, prop, object_type))]
    fn append_prop_or_spread_to_type(
        &mut self,
        known_keys: &mut Vec<Key>,
        to: Type,
        prop: &RPropOrSpread,
        object_type: Option<&Type>,
    ) -> ValidationResult {
        match prop {
            RPropOrSpread::Spread(RSpreadElement { expr, .. }) => {
                let prop_ty: Type = expr.validate_with_default(self)?.freezed();
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
                            let key_ty = key.ty();
                            let key = key.normalize().into_owned();

                            let span = key.span();

                            // Check if duplicate key exists.
                            // We show errors on the second key and latters.
                            //
                            // See: es6/Symbols/symbolProperty36.ts

                            // TODO(kdy1): Exclude types which is not valid for computed key
                            if !key.is_computed()
                                && known_keys.iter().any(|prev_key| {
                                    // TODO(kdy1): Use
                                    // self.key_matches(span, prev_key, key, false)
                                    prev_key.type_eq(&key)
                                })
                            {
                                self.storage.report(Error::DuplicateProperty { span })
                            } else {
                                known_keys.push(key);
                            }
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
    #[instrument(skip(self, to, rhs))]
    fn append_type(&mut self, to: Type, mut rhs: Type) -> ValidationResult<Type> {
        if to.is_any() || to.is_unknown() {
            return Ok(to);
        }

        match to.normalize() {
            Type::Function(..) => {
                // objectSpead.ts says
                //
                //
                // functions result in { }
                return Ok(to);
            }
            _ => {}
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

        if rhs.is_kwd(TsKeywordTypeKind::TsObjectKeyword) {
            return Ok(to);
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
                if let Some(rhs) = self.convert_type_to_type_lit(rhs.span(), Cow::Borrowed(&rhs))? {
                    return self.append_type(to, Type::TypeLit(rhs.into_owned()));
                }
            }

            _ => {}
        }

        let mut to = to.foldable();
        // TODO(kdy1): PERF

        match to {
            Type::TypeLit(ref mut lit) => {
                lit.metadata.inexact = true;
                let common_metadata = lit.metadata.common;

                rhs = rhs.foldable();

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
                            metadata: UnionMetadata {
                                common: common_metadata,
                                ..Default::default()
                            },
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
                    metadata: to.metadata,
                })
                .fixed())
            }

            _ => {}
        }

        unimplemented!("append_type:\n{:?}\n{:?}", to, rhs)
    }

    #[instrument(skip(self, to, rhs))]
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
                // TODO(kdy1): Remove previous member with same key.

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
                metadata: to.metadata,
            })
            .fixed()),
            _ => {
                unimplemented!("append_type_element\n{:?}\n{:?}", to, rhs)
            }
        }
    }
}
