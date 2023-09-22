use std::borrow::Cow;

use stc_arc_cow::ArcCow;
use stc_ts_ast_rnode::{RIdent, RTsEnumMemberId, RTsLit};
use stc_ts_errors::DebugExt;
use stc_ts_types::{
    Conditional, Enum, EnumVariant, Intersection, KeywordType, KeywordTypeMetadata, LitType, Type, TypeElement, TypeLit, TypeParam, Union,
};
use stc_utils::{cache::Freeze, ext::TypeVecExt};
use swc_atoms::JsWord;
use swc_common::{Span, TypeEq};
use swc_ecma_ast::TsKeywordTypeKind;

use super::NormalizeTypeOpts;
use crate::{analyzer::Analyzer, VResult};

impl Analyzer<'_, '_> {
    pub(crate) fn normalize_intersection_types(&mut self, span: Span, types: &[Type], opts: NormalizeTypeOpts) -> VResult<Option<Type>> {
        macro_rules! never {
            () => {{
                Ok(Some(Type::Keyword(KeywordType {
                    span,
                    kind: TsKeywordTypeKind::TsNeverKeyword,
                    metadata: KeywordTypeMetadata { ..Default::default() },
                    tracker: Default::default(),
                })))
            }};
        }

        let mut normalized_types = vec![];
        // set normalize all
        for el in types.iter() {
            if let Ok(res) = self.normalize(
                Some(span),
                Cow::Borrowed(el),
                NormalizeTypeOpts {
                    preserve_global_this: true,
                    ..opts
                },
            ) {
                let result = res.into_owned();

                match &result.normalize() {
                    Type::Keyword(KeywordType {
                        kind: TsKeywordTypeKind::TsUnknownKeyword,
                        ..
                    }) => {}
                    Type::Intersection(Intersection { types, .. }) => {
                        for ty in types {
                            normalized_types.push(ty.to_owned());
                        }
                    }
                    _ => {
                        normalized_types.push(result);
                    }
                }
            }
        }

        normalized_types.dedup_type();
        normalized_types.freeze();

        if normalized_types.len() == 1 {
            if let Some(ty) = normalized_types.pop() {
                return Ok(Some(ty));
            }
        }
        // has never; return never
        if normalized_types.iter().any(|ty| ty.is_never()) {
            return never!();
        }
        // has any, return any
        if normalized_types.iter().any(|ty| ty.is_any()) {
            return Ok(Some(Type::Keyword(KeywordType {
                span,
                kind: TsKeywordTypeKind::TsAnyKeyword,
                metadata: KeywordTypeMetadata { ..Default::default() },
                tracker: Default::default(),
            })));
        }

        let is_symbol = normalized_types.iter().any(|ty| ty.is_symbol());
        let is_str = normalized_types.iter().any(|ty| ty.is_str());
        let is_num = normalized_types.iter().any(|ty| ty.is_num());
        let is_bool = normalized_types.iter().any(|ty| ty.is_bool());
        let is_null = normalized_types.iter().any(|ty| ty.is_null());
        let is_undefined = normalized_types.iter().any(|ty| ty.is_undefined());
        let is_void = normalized_types.iter().any(|ty| ty.is_kwd(TsKeywordTypeKind::TsVoidKeyword));
        let is_object = normalized_types.iter().any(|ty| ty.is_kwd(TsKeywordTypeKind::TsObjectKeyword));
        let is_function = normalized_types.iter().any(|ty| ty.is_fn_type());
        let is_type_lit = normalized_types.iter().any(|ty| ty.is_type_lit());

        if (is_null || is_undefined) && is_type_lit {
            return never!();
        }

        let sum = u32::from(is_symbol)
            + u32::from(is_str)
            + u32::from(is_num)
            + u32::from(is_bool)
            + u32::from(is_null)
            + u32::from(is_undefined)
            + u32::from(is_void)
            + u32::from(is_object)
            + u32::from(is_function);

        if sum >= 2 {
            if sum == 2 && is_undefined && is_void {
                return Ok(Some(Type::undefined(span, Default::default())));
            }
            return never!();
        }

        if normalized_types.len() == 2 {
            let (a, b) = (&normalized_types[0], &normalized_types[1]);
            if ((a.is_str_lit() && b.is_str_lit()) || (a.is_num_lit() && b.is_num_lit()) || (a.is_bool_lit() && b.is_bool_lit()))
                && !a.type_eq(b)
            {
                return never!();
            }
            if let (Type::Conditional(c), other) | (other, Type::Conditional(c)) = (a.normalize(), b.normalize()) {
                return Ok(Some(
                    Type::Conditional(Conditional {
                        span,
                        check_type: c.check_type.clone(),
                        extends_type: c.extends_type.clone(),
                        true_type: Box::new(Type::new_intersection(span, vec![*(c.true_type).clone(), other.clone()])),
                        false_type: Box::new(Type::new_intersection(span, vec![*(c.false_type.clone()), other.clone()])),
                        metadata: c.metadata,
                        tracker: c.tracker,
                    })
                    .freezed(),
                ));
            }
        }

        let enum_variant_iter = normalized_types.iter().filter(|&t| t.is_enum_variant()).collect::<Vec<&Type>>();
        let enum_variant_len = enum_variant_iter.len();

        if enum_variant_len > 0 {
            if normalized_types.iter().any(|ty| matches!(ty.normalize(), Type::Lit(..))) {
                return never!();
            }
            if let Some(first_enum) = enum_variant_iter.first() {
                let mut enum_temp = first_enum.normalize();
                for elem in enum_variant_iter.into_iter() {
                    if let Type::EnumVariant(el) = elem.normalize() {
                        if let Type::EnumVariant(en) = enum_temp {
                            if let Type::EnumVariant(EnumVariant { name: None, .. }) = enum_temp {
                                enum_temp = elem;
                                continue;
                            }

                            if en.def.id != el.def.id {
                                return never!();
                            }

                            // eq two argument enum_name
                            if let Ok(el_lit) = self.expand_enum_variant(elem.clone()) {
                                if let Ok(etl) = self.expand_enum_variant(enum_temp.clone()) {
                                    if !etl.type_eq(&el_lit) {
                                        return never!();
                                    }
                                }
                            }
                        }
                    }
                }
            }

            #[inline]
            fn enum_variant(span: Span, def: ArcCow<Enum>, name: JsWord) -> Type {
                Type::EnumVariant(EnumVariant {
                    span,
                    def,
                    name: Some(name),
                    metadata: Default::default(),
                    tracker: Default::default(),
                })
            }
            for elem in normalized_types.iter() {
                if let Type::EnumVariant(ref ev) = elem.normalize() {
                    match &ev.name {
                        Some(variant_name) => {
                            // enumVariant is enumMember
                            if enum_variant_len >= 2 {
                                let mut en = ev.clone();
                                en.name = None;
                                return Ok(Some(Type::EnumVariant(en).freezed()));
                            }

                            if let Ok(Type::Lit(LitType { .. })) = self.expand_enum_variant(elem.clone()) {
                                return Ok(Some(elem.clone().freezed()));
                            }
                        }
                        None => {
                            // enumVariant is Enum
                            let mut str_lits = vec![];
                            let mut num_lits = vec![];

                            ev.def.members.iter().for_each(|v| {
                                let key = match &v.id {
                                    RTsEnumMemberId::Ident(i) => i.clone(),
                                    RTsEnumMemberId::Str(s) => RIdent::new(s.value.clone(), s.span),
                                };
                                match &*v.val {
                                    Type::Lit(LitType { lit: RTsLit::Str(v), .. }) => {
                                        str_lits.push(enum_variant(v.span, ev.def.cheap_clone(), key.sym))
                                    }
                                    Type::Lit(LitType {
                                        lit: RTsLit::Number(v), ..
                                    }) => num_lits.push(enum_variant(v.span, ev.def.cheap_clone(), key.sym)),
                                    _ => {}
                                }
                            });

                            if str_lits.is_empty() && is_str {
                                return never!();
                            }
                            if num_lits.is_empty() && is_num {
                                return never!();
                            }
                            if str_lits.is_empty() && is_num || num_lits.is_empty() && is_str {
                                return Ok(Some(elem.clone().freezed()));
                            }

                            let mut ty = Type::new_union(
                                span,
                                if is_str {
                                    str_lits
                                } else if is_num {
                                    num_lits
                                } else {
                                    return never!();
                                },
                            );

                            ty.reposition(ev.def.span);
                            return Ok(Some(ty).freezed());
                        }
                    }
                }
            }
        }

        {
            let mut property_types = vec![];

            for elem in types.iter() {
                let elem = self
                    .normalize(
                        Some(span),
                        Cow::Borrowed(elem),
                        NormalizeTypeOpts {
                            preserve_global_this: true,
                            ..opts
                        },
                    )
                    .context("failed to normalize types while intersecting properties")?
                    .freezed()
                    .into_owned()
                    .freezed();

                if let Type::TypeLit(TypeLit { members, .. }) = elem.normalize_instance() {
                    if let Some(ty) = self.normalize_intersection_of_type_elements(span, members, &mut property_types, opts)? {
                        return Ok(Some(ty));
                    }
                }
            }
        }

        self.flat_intersection_type(span, normalized_types)
    }

    fn flat_intersection_type(&mut self, span: Span, mut normalized_types: Vec<Type>) -> VResult<Option<Type>> {
        macro_rules! never {
            () => {{
                Ok(Some(Type::never(span, Default::default())))
            }};
        }

        let normalized_len = normalized_types.len();
        normalized_types.freeze();

        let mut type_iter = normalized_types.clone().into_iter();
        let mut acc_type = type_iter
            .next()
            .unwrap_or_else(|| {
                Type::Keyword(KeywordType {
                    span,
                    kind: TsKeywordTypeKind::TsNeverKeyword,
                    metadata: KeywordTypeMetadata { ..Default::default() },
                    tracker: Default::default(),
                })
            })
            .freezed();

        for elem in type_iter {
            let mut new_types = vec![];
            match (acc_type.normalize(), elem.normalize()) {
                (
                    another @ Type::Param(TypeParam {
                        constraint:
                            Some(box Type::Keyword(KeywordType {
                                kind: TsKeywordTypeKind::TsUnknownKeyword,
                                ..
                            })),
                        ..
                    }),
                    other,
                )
                | (
                    other,
                    another @ Type::Param(TypeParam {
                        constraint:
                            Some(box Type::Keyword(KeywordType {
                                kind: TsKeywordTypeKind::TsUnknownKeyword,
                                ..
                            })),
                        ..
                    }),
                ) => {
                    new_types.push(Type::new_intersection(span, [other.clone(), another.clone()]).freezed());
                }

                (
                    Type::Param(TypeParam {
                        constraint: Some(other), ..
                    }),
                    Type::Param(TypeParam {
                        constraint: Some(another), ..
                    }),
                ) => {
                    if let Some(tp) =
                        self.normalize_intersection_types(span, &[*other.to_owned(), *another.to_owned()], Default::default())?
                    {
                        new_types.push(tp);
                    }
                }

                (
                    Type::Param(TypeParam {
                        constraint: Some(another), ..
                    }),
                    other,
                )
                | (
                    other,
                    Type::Param(TypeParam {
                        constraint: Some(another), ..
                    }),
                ) => {
                    if let Some(tp) =
                        self.normalize_intersection_types(span, &[other.to_owned(), *another.to_owned()], Default::default())?
                    {
                        // We should preserve `T & {}`
                        if match other.normalize() {
                            Type::TypeLit(ty) => ty.members.is_empty(),
                            _ => false,
                        } && tp.is_interface()
                        {
                            new_types.push(acc_type.clone());
                            new_types.push(elem.clone());
                        } else {
                            new_types.push(tp);
                        }
                    }
                }
                (Type::Union(Union { types: a_types, .. }), Type::Union(Union { types: b_types, .. })) => {
                    for a_ty in a_types {
                        for b_ty in b_types {
                            if let Some(tp) =
                                self.normalize_intersection_types(span, &[a_ty.to_owned(), b_ty.to_owned()], Default::default())?
                            {
                                new_types.push(tp);
                            }
                        }
                    }
                }
                (Type::Union(Union { types, .. }), other) | (other, Type::Union(Union { types, .. })) => {
                    for ty in types {
                        if let Some(tp) = self.normalize_intersection_types(span, &[ty.to_owned(), other.to_owned()], Default::default())? {
                            new_types.push(tp);
                        }
                    }
                }
                (Type::Intersection(Intersection { types, .. }), other) | (other, Type::Intersection(Intersection { types, .. })) => {
                    let mut temp_vec = vec![];
                    temp_vec.append(&mut types.to_owned());
                    temp_vec.push(other.to_owned());

                    acc_type = Type::new_intersection(span, temp_vec).freezed();
                    continue;
                }

                (other, another) => {
                    acc_type = Type::new_intersection(span, vec![other.to_owned(), another.to_owned()]).freezed();
                    continue;
                }
            };

            new_types.retain(|ty| !ty.is_never());

            acc_type = match new_types.len() {
                0 => return never!(),
                1 => {
                    if let Some(ty) = new_types.pop() {
                        ty
                    } else {
                        return never!();
                    }
                }
                _ => Type::new_union(span, new_types).freezed(),
            };
        }

        if matches!(acc_type.normalize(), Type::Union(Union { types: u_types, .. }) if normalized_len < u_types.len()) {
            return Ok(Some(Type::new_intersection(span, normalized_types).freezed()));
        }

        Ok(Some(acc_type))
    }

    fn normalize_intersection_of_type_elements(
        &mut self,
        span: Span,
        elements: &[TypeElement],
        property_types: &mut Vec<TypeElement>,
        opts: NormalizeTypeOpts,
    ) -> VResult<Option<Type>> {
        macro_rules! never {
            () => {{
                Ok(Some(Type::Keyword(KeywordType {
                    span,
                    kind: TsKeywordTypeKind::TsNeverKeyword,
                    metadata: KeywordTypeMetadata { ..Default::default() },
                    tracker: Default::default(),
                })))
            }};
        }

        // Intersect property types
        'outer: for e in elements.iter() {
            if let TypeElement::Property(p) = e {
                for prev in property_types.iter_mut() {
                    if let TypeElement::Property(prev) = prev {
                        if prev.key.type_eq(&p.key) {
                            let prev_type = prev
                                .type_ann
                                .clone()
                                .map(|v| *v)
                                .unwrap_or_else(|| Type::any(span, KeywordTypeMetadata { ..Default::default() }));
                            let other = p
                                .type_ann
                                .clone()
                                .map(|v| *v)
                                .unwrap_or_else(|| Type::any(span, KeywordTypeMetadata { ..Default::default() }));

                            let new = self.normalize_intersection_types(span, &[prev_type, other], opts)?;

                            if let Some(new) = new {
                                if new.is_never() {
                                    return never!();
                                }
                                prev.type_ann = Some(Box::new(new));
                                continue 'outer;
                            }
                        }
                    }
                }
            }

            property_types.push(e.clone());
        }

        Ok(None)
    }
}
