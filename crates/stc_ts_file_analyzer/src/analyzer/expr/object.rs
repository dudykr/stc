use std::{borrow::Cow, time::Instant};

use rnode::VisitMutWith;
use stc_ts_ast_rnode::{RObjectLit, RPropOrSpread, RSpreadElement};
use stc_ts_errors::{DebugExt, ErrorKind};
use stc_ts_file_analyzer_macros::validator;
use stc_ts_type_ops::{union_normalization::ObjectUnionNormalizer, Fix};
use stc_ts_types::{Accessor, Key, MethodSignature, PropertySignature, Type, TypeElement, TypeLit, TypeParam, Union, UnionMetadata};
use stc_utils::{cache::Freeze, dev_span};
use swc_common::{Span, Spanned, SyntaxContext, TypeEq};
use swc_ecma_ast::TsKeywordTypeKind;
use tracing::debug;

use crate::{
    analyzer::{Analyzer, NormalizeTypeOpts, ScopeKind},
    validator::ValidateWith,
    VResult,
};

#[derive(Debug, Clone, Copy, Default)]
pub struct AppendTypeOpts {
    pub do_not_check_for_undefined: bool,
}

#[validator]
impl Analyzer<'_, '_> {
    fn validate(&mut self, node: &RObjectLit, type_ann: Option<&Type>) -> VResult<Type> {
        let type_ann = self.expand_type_ann(node.span, type_ann)?;
        debug_assert_eq!(node.span.ctxt, SyntaxContext::empty());

        self.with_child(ScopeKind::ObjectLit, Default::default(), |a: &mut Analyzer| {
            let mut ret = Type::TypeLit(TypeLit {
                span: node.span,
                members: vec![],
                metadata: Default::default(),
                tracker: Default::default(),
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
    pub(super) fn normalize_union(&mut self, ty: &mut Type, preserve_specified: bool) {
        let _tracing = dev_span!("normalize_union");

        let start = Instant::now();
        ty.visit_mut_with(&mut ObjectUnionNormalizer { preserve_specified });

        let end = Instant::now();

        debug!("Normalized unions (time = {:?})", end - start);
    }

    pub(crate) fn validate_type_literals(&mut self, ty: &Type, is_type_ann: bool) {
        let _tracing = dev_span!("validate_type_literals");

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

    pub(crate) fn report_errors_for_mixed_optional_method_signatures(&mut self, elems: &[TypeElement]) {
        let _tracing = dev_span!("report_errors_for_mixed_optional_method_signatures");

        let mut keys: Vec<(&Key, bool)> = vec![];
        for elem in elems {
            if let TypeElement::Method(MethodSignature { key, optional, .. }) = elem {
                if let Some(prev) = keys.iter().find(|v| v.0.type_eq(key)) {
                    if *optional != prev.1 {
                        self.storage
                            .report(ErrorKind::OptionalAndNonOptionalMethodPropertyMixed { span: key.span() }.into());
                        continue;
                    }
                }

                keys.push((key, *optional));
            }
        }
    }

    fn append_prop_or_spread_to_type(
        &mut self,
        known_keys: &mut Vec<Key>,
        to: Type,
        prop: &RPropOrSpread,
        object_type: Option<&Type>,
    ) -> VResult<Type> {
        let _tracing = dev_span!("append_prop_or_spread_to_type");

        if let Some(object_type) = object_type {
            object_type.assert_clone_cheap();
        }

        match prop {
            RPropOrSpread::Spread(RSpreadElement { dot3_token, expr, .. }) => {
                let prop_ty: Type = expr.validate_with_default(self)?.freezed();

                if prop_ty.normalize().is_unknown() {
                    self.storage.report(
                        ErrorKind::NonObjectInSpread {
                            span: Span {
                                lo: dot3_token.span().lo,
                                hi: prop_ty.span().hi,
                                ctxt: SyntaxContext::empty(),
                            },
                            ty: box prop_ty.clone(),
                        }
                        .into(),
                    )
                }

                self.append_type(*dot3_token, to, prop_ty, Default::default())
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
                            // We show errors on the second key and the latter.
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
                                self.storage.report(ErrorKind::DuplicateProperty { span }.into())
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

    pub(crate) fn is_always_undefined(&mut self, ty: &Type) -> bool {
        if ty.is_undefined() {
            return true;
        }
        match ty.normalize() {
            Type::Union(ty) => ty.types.iter().all(|ty| self.is_always_undefined(ty)),
            Type::Intersection(ty) => ty.types.iter().any(|ty| self.is_always_undefined(ty)),
            Type::Param(TypeParam {
                constraint: Some(constraint),
                ..
            }) => self.is_always_undefined(constraint),
            _ => false,
        }
    }

    /// If rhs is an union type, return type will be union.
    ///
    /// `{ a: number } + ( {b: number} | { c: number } )` => `{ a: number, b:
    /// number } | { a: number, c: number }`
    pub(crate) fn append_type(&mut self, span: Span, to: Type, rhs: Type, opts: AppendTypeOpts) -> VResult<Type> {
        let _tracing = dev_span!("append_type");

        if to.is_any() || to.is_unknown() {
            return Ok(to);
        }

        if let Type::Function(..) = to.normalize() {
            // objectSpread.ts says
            //
            //
            // functions result in { }
            return Ok(to);
        }

        let mut rhs = self
            .normalize(
                Some(span),
                Cow::Owned(rhs),
                NormalizeTypeOpts {
                    preserve_intersection: true,
                    preserve_union: true,
                    preserve_global_this: true,
                    ..Default::default()
                },
            )?
            .into_owned();

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
            Type::Interface(..) | Type::Class(..) | Type::Intersection(..) | Type::Mapped(..) => {
                // Append as a type literal.
                if let Some(rhs) = self.convert_type_to_type_lit(rhs.span(), Cow::Borrowed(&rhs))? {
                    return self.append_type(span, to, Type::TypeLit(rhs.into_owned()), opts);
                }
            }

            _ => {}
        }

        if !opts.do_not_check_for_undefined && self.is_always_undefined(&rhs) {
            self.storage
                .report(ErrorKind::NonObjectInSpread { span, ty: box rhs.clone() }.into());
            return Ok(Type::any(to.span(), Default::default()));
        }

        let mut to = to.foldable();
        // TODO(kdy1): PERF

        match to {
            Type::TypeLit(ref mut lit) => {
                lit.metadata.inexact = true;
                let common_metadata = lit.metadata.common;

                rhs = rhs.foldable();

                match rhs {
                    Type::TypeLit(mut rhs) => {
                        remove_readonly(&mut rhs.members);
                        lit.members.extend(rhs.members);
                        return Ok(to);
                    }
                    Type::Union(rhs) => {
                        return Ok(Type::Union(Union {
                            span: lit.span,
                            types: rhs
                                .types
                                .into_iter()
                                .map(|rhs| {
                                    self.append_type(
                                        span,
                                        to.clone(),
                                        rhs,
                                        AppendTypeOpts {
                                            do_not_check_for_undefined: true,
                                            ..opts
                                        },
                                    )
                                })
                                .collect::<Result<_, _>>()?,
                            metadata: UnionMetadata {
                                common: common_metadata,
                                ..Default::default()
                            },
                            tracker: Default::default(),
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
                        .map(|to| {
                            self.append_type(
                                span,
                                to,
                                rhs.clone(),
                                AppendTypeOpts {
                                    do_not_check_for_undefined: true,
                                    ..opts
                                },
                            )
                        })
                        .collect::<Result<_, _>>()?,
                    metadata: to.metadata,
                    tracker: Default::default(),
                })
                .fixed())
            }

            _ => {}
        }

        Ok(Type::new_intersection(span, vec![to, rhs]))
    }

    pub(crate) fn append_type_element(&mut self, to: Type, rhs: TypeElement) -> VResult<Type> {
        let _tracing = dev_span!("append_type_element");

        if to.is_any() || to.is_unknown() {
            return Ok(to);
        }
        if to.is_kwd(TsKeywordTypeKind::TsObjectKeyword) {
            return Ok(to);
        }

        let mut to = if let Some(key) = rhs.key() {
            if to.is_type_lit() && !key.is_computed() {
                self.exclude_props(rhs.span(), &to, &[key.clone()])
                    .context("tried to exclude properties before appending a type element")?
            } else {
                to
            }
        } else {
            to
        };
        to.normalize_mut();

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
                tracker: Default::default(),
            })
            .fixed()),
            Type::Intersection(ref mut to_intersection) => {
                for to_ty in to_intersection.types.iter_mut().rev() {
                    if to_ty.is_type_lit() {
                        *to_ty = self.append_type_element(to_ty.clone(), rhs)?;
                        return Ok(to);
                    }
                }

                to_intersection.types.push(Type::TypeLit(TypeLit {
                    span: rhs.span(),
                    members: vec![rhs],
                    metadata: Default::default(),
                    tracker: Default::default(),
                }));

                Ok(to)
            }
            _ => Err(ErrorKind::Unimplemented {
                span: to.span(),
                msg: format!("append_type_element\n{:?}\n{:?}", to, rhs),
            }
            .into()),
        }
    }
}

fn remove_readonly(members: &mut [TypeElement]) {
    for member in members {
        match member {
            TypeElement::Property(el) => {
                el.readonly = false;
            }
            TypeElement::Method(el) => {
                el.readonly = false;
            }
            TypeElement::Index(el) => {
                el.readonly = false;
            }

            _ => {}
        }
    }
}
