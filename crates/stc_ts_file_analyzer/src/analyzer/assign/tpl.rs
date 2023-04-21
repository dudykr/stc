#![allow(clippy::if_same_then_else)]

use stc_ts_ast_rnode::{RStr, RTsLit};
use stc_ts_errors::{debug::force_dump_type_as_string, ErrorKind};
use stc_ts_types::{Id, IntrinsicKind, KeywordType, LitType, StringMapping, TplElem, TplType, Type, Union};
use stc_utils::dev_span;
use swc_atoms::{js_word, Atom, JsWordStaticSet};
use swc_common::{Span, TypeEq, DUMMY_SP};
use swc_ecma_ast::{TsKeywordType, TsKeywordTypeKind};

use crate::{
    analyzer::{
        assign::{AssignData, AssignOpts},
        Analyzer,
    },
    VResult,
};

impl Analyzer<'_, '_> {
    /// # Implementation notes
    ///
    /// We split string based on the literals.
    ///
    ///
    /// `:${string}:::${string}:` = ":1:sss:s:s:s:s::s:s:"
    ///
    /// For the code above, we try to find `:`, `:::`, `:`, while preserving
    /// orders.
    ///
    /// After splitting, we can check if each element is assignable.
    pub(crate) fn assign_to_tpl(&mut self, data: &mut AssignData, l: &TplType, r_ty: &Type, opts: AssignOpts) -> VResult<()> {
        let span = opts.span;
        let r_ty = r_ty.normalize();
        dbg!(&l, &r_ty);

        let inference = self.infer_types_from_tpl_lit_type(span, r_ty, l)?;

        let inference = match inference {
            Some(inference) => inference,
            None => return Err(ErrorKind::SimpleAssignFailed { span, cause: None }.context("tried to infer")),
        };

        dbg!(&inference);

        for (i, ty) in inference.iter().enumerate() {
            if !self.is_valid_type_for_tpl_lit_placeholder(span, ty, &l.types[i])? {
                return Err(ErrorKind::SimpleAssignFailed { span, cause: None }.context(format!(
                    "verified types:\nsource = {}\ntarget = {}",
                    force_dump_type_as_string(ty),
                    force_dump_type_as_string(&l.types[i])
                )));
            }
        }

        Ok(())
    }

    /// Ported from `isValidTypeForTemplateLiteralPlaceholder` of `tsc`
    pub(crate) fn is_valid_type_for_tpl_lit_placeholder(&mut self, span: Span, source: &Type, target: &Type) -> VResult<bool> {
        let _tracing = dev_span!(
            "is_valid_type_for_tpl_lit_placeholder",
            source = tracing::field::display(&force_dump_type_as_string(source)),
            target = tracing::field::display(&force_dump_type_as_string(target)),
        );

        if source.type_eq(target) || target.is_any() || target.is_kwd(TsKeywordTypeKind::TsStringKeyword) {
            return Ok(true);
        }
        dbg!(&target);

        match source.normalize() {
            Type::Lit(LitType {
                lit: RTsLit::Str(value), ..
            }) => {
                // TODO: Validate literal value correctly
                if target.is_num() && self.is_valid_num_str(&value.value, false)
                    || target.is_bigint() && self.is_valid_big_int_str(&value.value, false)
                {
                    return Ok(true);
                }

                // TODO: Check for `source`
                match &*value.value {
                    "true" | "false" | "null" | "undefined" => return Ok(true),
                    _ => {}
                }

                if let Type::StringMapping(StringMapping {
                    kind: IntrinsicKind::Capitalize | IntrinsicKind::Uncapitalize | IntrinsicKind::Uppercase | IntrinsicKind::Lowercase,
                    ..
                }) = target.normalize()
                {
                    return self.is_member_of_string_mapping(span, source, target);
                }

                // TODO(kdy1): Return `Ok(false)` instead
            }

            Type::Tpl(source) => {
                if source.quasis.len() == 2 && source.quasis[0].value == "" && source.quasis[1].value == "" {
                    // TODO(kdy1): Return `Ok(self.is_type_assignable_to(span, &source.types[0],
                    // target))` instead

                    // let x = self.get_template_literal_type(span, source.quasis, source.types);
                    if self.is_type_assignable_to(span, &source.types[0], target) {
                        return Ok(true);
                    }
                } else {
                    return Ok(false);
                }
            }

            _ => {}
        }

        Ok(self.is_type_assignable_to(span, source, target))
    }

    /// Ported from `templateLiteralTypesDefinitelyUnrelated` of `tsc`.
    pub(crate) fn tpl_lit_type_definitely_unrelated(&mut self, span: Span, source: &TplType, target: &TplType) -> VResult<bool> {
        // Two template literal types with differences in their starting or ending text
        // spans are definitely unrelated.

        let source_start = &source.quasis[0].value;
        let target_start = &target.quasis[0].value;
        let source_end = &source.quasis[source.quasis.len() - 1].value;
        let target_end = &target.quasis[target.quasis.len() - 1].value;
        let start_len = source_start.len().min(target_start.len());
        let end_len = source_end.len().min(target_end.len());

        Ok(source_start[0..start_len] != target_start[0..start_len]
            || source_end[(source_end.len() - end_len)..] != target_end[(target_end.len() - end_len)..])
    }

    pub(crate) fn replace_element(&self, types: Vec<Type>, index: usize, value: Type) -> Vec<Type> {
        let mut v = types.clone();
        v[index] = value;
        v
    }

    // pub crate fn s(&mut self, f: fn(a: Type) -> Type, span: Span, texts:
    // Vec<TplElem>, types: Vec<Type>) -> fn(a: Type) -> Type  {     return
    // }

    pub(crate) fn inline_map(
        &mut self,
        ty: Type,
        span: Span,
        types: Vec<Type>,
        texts: Vec<TplElem>,
        index: usize,
        f: impl Fn(&mut Analyzer, Type) -> Type,
    ) -> impl Fn(&mut Analyzer, Type) -> Type {
        let x = self.replace_element(types, index, ty);

        // let f = s()

        return f;
    }

    pub(crate) fn is_unary_tuple(&mut self, ty: Type) -> bool {
        if let Type::Tuple(t) = ty.normalize() {
            t.elems.len() == 1
        } else {
            false
        }
    }

    // pub(crate) fn get_implied_constraint(&mut self, ty: Type, check_ty: Type,
    // extends_ty: Type) -> Type {     if self.is_unary_tuple(check_ty) &&
    // self.is_unary_tuple(extends_ty) {         return
    // self.get_implied_constraint(             ty,
    //             check_ty.as_tuple().unwrap().elems[0].ty.normalize().clone(),
    //             extends_ty.as_tuple().unwrap().elems[0].ty.normalize().clone(),
    //         );
    //     } else {
    //     }
    // }

    pub(crate) fn get_conditional_flow_type_of_type(&mut self, ty: Type, orig_ty: Type) {
        let constraints: Vec<Type> = vec![];
        let mut covariant = true;

        if let Type::Conditional(c) = orig_ty.normalize() {
            if ty.type_eq(&c.true_type) {}
        }
    }

    // TODO(481): getConditionalFlowTypeOfType
    // function getConditionalFlowTypeOfType(type: Type, node: Node) {
    //     let constraints: Type[] | undefined;
    //     let covariant = true;
    //     while (node && !isStatement(node) && node.kind !== SyntaxKind.JSDoc) {
    //         const parent = node.parent;
    //         // only consider variance flipped by parameter locations - `keyof`
    // types would usually be considered variance inverting, but         //
    // often get used in indexed accesses where they behave sortof invariantly, but
    // our checking is lax         if (parent.kind === SyntaxKind.Parameter) {
    //             covariant = !covariant;
    //         }
    //         // Always substitute on type parameters, regardless of variance,
    // since even         // in contravariant positions, they may rely on
    // substituted constraints to be valid         if (
    //             (covariant || type.flags & TypeFlags.TypeVariable) &&
    //             parent.kind === SyntaxKind.ConditionalType &&
    //             node === (parent as ConditionalTypeNode).trueType
    //         ) {
    //             const constraint = getImpliedConstraint(
    //                 type,
    //                 (parent as ConditionalTypeNode).checkType,
    //                 (parent as ConditionalTypeNode).extendsType
    //             );
    //             if (constraint) {
    //                 constraints = append(constraints, constraint);
    //             }
    //         }
    //         // Given a homomorphic mapped type { [K in keyof T]: XXX }, where T
    // is constrained to an array or tuple type, in the         // template type
    // XXX, K has an added constraint of number | `${number}`.         else if (
    //             type.flags & TypeFlags.TypeParameter &&
    //             parent.kind === SyntaxKind.MappedType &&
    //             node === (parent as MappedTypeNode).type
    //         ) {
    //             const mappedType = getTypeFromTypeNode(
    //                 parent as TypeNode
    //             ) as MappedType;
    //             if (
    //                 getTypeParameterFromMappedType(mappedType) ===
    //                 getActualTypeVariable(type)
    //             ) {
    //                 const typeParameter =
    //                     getHomomorphicTypeVariable(mappedType);
    //                 if (typeParameter) {
    //                     const constraint =
    //                         getConstraintOfTypeParameter(typeParameter);
    //                     if (
    //                         constraint &&
    //                         everyType(constraint, isArrayOrTupleType)
    //                     ) {
    //                         constraints = append(
    //                             constraints,
    //                             getUnionType([numberType, numericStringType])
    //                         );
    //                     }
    //                 }
    //             }
    //         }
    //         node = parent;
    //     }
    //     return constraints
    //         ? getSubstitutionType(type, getIntersectionType(constraints))
    //         : type;
    // }

    // TODO(481): getTypeFromTypeNode
    // function getTypeFromTypeNode(node: TypeNode): Type {
    //     return getConditionalFlowTypeOfType(
    //         getTypeFromTypeNodeWorker(node),
    //         node
    //     );
    // }

    // TODO(481): getTypeFromTemplateTypeNode
    // function getTypeFromTemplateTypeNode(node: TemplateLiteralTypeNode) {
    //     const links = getNodeLinks(node);

    //     if (!links.resolvedType) {
    //         links.resolvedType = getTemplateLiteralType(
    //             [
    //                 node.head.text,
    //                 ...map(node.templateSpans, (span) => span.literal.text),
    //             ],
    //             map(node.templateSpans, (span) =>
    //                 getTypeFromTypeNode(span.type)
    //             )
    //         );
    //     }

    //     return links.resolvedType;
    // }

    pub(crate) fn get_template_literal_type(&mut self, span: Span, texts: Vec<TplElem>, types: Vec<Type>) -> Type {
        let union_index = types.clone().into_iter().position(|t| t.is_union_type() || t.is_never());
        dbg!(&union_index);
        if let Some(union_index) = union_index {
            let cross_product_union = self.check_cross_product_union(types.clone());
            let x = types[union_index].clone();
            if cross_product_union {
                return self.map_type(
                    x,
                    |child: &mut Analyzer, t: Type| {
                        child.get_template_literal_type(span, texts.clone(), child.replace_element(types.clone(), union_index, t))
                    },
                    false,
                );
            }
        }

        if types.clone().into_iter().any(|v| v.is_any()) {
            return Type::any(DUMMY_SP, Default::default());
        }

        let mut new_types: Vec<Type> = vec![];
        let mut new_texts: Vec<TplElem> = vec![];
        let text = &mut texts[0].clone();

        if !self.add_spans(texts.clone(), types.clone(), text, &mut new_texts, &mut new_types) {
            return Type::Keyword(KeywordType {
                span,
                kind: TsKeywordTypeKind::TsStringKeyword,
                metadata: Default::default(),
                tracker: Default::default(),
            });
        }

        if new_types.is_empty() {
            let t = text.clone();

            return Type::Lit(LitType {
                span,
                lit: RTsLit::Str(RStr {
                    span: t.span,
                    value: t.value.to_string().into(),
                    raw: Some(t.value),
                }),
                metadata: Default::default(),
                tracker: Default::default(),
            });
        }
        new_texts.push(text.clone());

        if new_texts.clone().into_iter().all(|v| v.value.is_empty()) {
            if new_types.clone().into_iter().all(|t| t.is_str()) {
                return Type::Keyword(KeywordType {
                    span,
                    kind: TsKeywordTypeKind::TsStringKeyword,
                    metadata: Default::default(),
                    tracker: Default::default(),
                });
            }

            if new_types.len() == 1 && new_types[0].is_pattern_literal() {
                return new_types[0].clone();
            }
        }

        Type::Tpl(TplType {
            span,
            quasis: new_texts,
            types: new_types,
            metadata: Default::default(),
            tracker: Default::default(),
        })
    }

    fn add_spans(
        &mut self,
        texts: Vec<TplElem>,
        types: Vec<Type>,
        text: &mut TplElem,
        new_texts: &mut Vec<TplElem>,
        new_types: &mut Vec<Type>,
    ) -> bool {
        let is_texts_array = texts.len() > 1;

        for (i, ty) in types.into_iter().enumerate() {
            let add_text;

            if is_texts_array {
                add_text = &texts[i + 1];
            } else {
                add_text = &texts[0];
            }

            if ty.is_lit() || ty.is_null() || ty.is_undefined() {
                text.value = [
                    &*text.value,
                    &*self.get_template_string_for_type(ty).unwrap_or(Atom::new("")),
                    &*add_text.value,
                ]
                .concat()
                .into();

                if !is_texts_array {
                    return true;
                }
            } else if ty.is_tpl() {
                let t = ty.as_tpl().unwrap();
                text.value = [&*text.value, &*t.quasis[0].value].concat().into();

                if !self.add_spans(t.clone().quasis, t.clone().types, text, new_texts, new_types) {
                    return false;
                }

                text.value = add_text.clone().value;
            } else if ty.is_generic_index() && ty.is_pattern_literal_placeholder() {
                new_types.push(ty);
                new_texts.push(text.clone());

                text.value = add_text.clone().value;
            } else if ty.is_intersection() {
                let added = self.add_spans(
                    vec![texts[i + 1].clone()],
                    ty.clone().as_intersection().unwrap().types.clone(),
                    text,
                    new_texts,
                    new_types,
                );

                if !added {
                    return false;
                }
            } else if !is_texts_array {
                return false;
            }
        }

        return true;
    }

    pub(crate) fn get_template_string_for_type(&mut self, ty: Type) -> Option<Atom> {
        match ty.normalize() {
            Type::Lit(lit) => match &lit.lit {
                RTsLit::Str(s) => s.clone().raw,
                RTsLit::Number(n) => n.clone().raw,
                RTsLit::BigInt(b) => b.clone().raw,
                _ => None,
            },
            _ => None, /* Type::Keyword(KeywordType {
                        *     kind: TsKeywordTypeKind::TsUndefinedKeyword,
                        *     ..
                        * }) => */
        }
    }

    fn check_cross_product_union(&mut self, types: Vec<Type>) -> bool {
        let size = self.check_cross_product_union_size(types.clone());
        dbg!(&size);
        if size >= 100000 {
            false
        } else {
            true
        }
    }

    pub(crate) fn check_cross_product_union_size(&mut self, types: Vec<Type>) -> usize {
        let mut init: usize = 1;

        for (i, ty) in types.into_iter().enumerate() {
            if let Type::Union(u) = ty.normalize() {
                init = init + u.types.len();
            } else if !ty.is_never() {
                init = init + i;
            }
        }

        init
    }

    pub(crate) fn map_type(&mut self, ty: Type, mapper: impl Fn(&mut Analyzer, Type) -> Type, no_reductions: bool) -> Type {
        dbg!(&ty);
        if ty.is_never() {
            return ty;
        }
        if !ty.is_union_type() {
            return mapper(self, ty);
        }
        let origin = ty.as_union_type().unwrap();
        let types = &origin.types;
        let mut mapped_types: Vec<Type> = vec![];
        let mut changed = false;

        for ty in types.into_iter() {
            let mapped;
            dbg!(&ty);
            if ty.is_union_type() {
                mapped = ty.clone();
                // mapped = self.map_type(ty.clone(), &mapper, no_reductions);
            } else {
                mapped = mapper(self, ty.clone())
            }

            if !ty.type_eq(&mapped) {
                changed = true;
            }

            mapped_types.push(mapped);
        }

        if changed {
            return ty.clone();
        } else {
            return ty.clone();
        }

        ty
    }

    pub(crate) fn get_union_type(
        &mut self,
        types: Vec<Type>,
        union_reduction: UnionReduction,
        alias_symbol: Option<Id>,
        alias_type_args: Option<Vec<Type>>,
        origin: Option<Type>,
    ) -> Type {
        let x = Type::never(DUMMY_SP, Default::default());
        if types.is_empty() {
            return x.clone();
        }
        if types.len() == 1 {
            return types[0].clone();
        }

        let mut type_set: Vec<Type> = vec![];
        self.add_types_to_union(&mut type_set, types);
        return x.clone();
    }

    //  pub(crate) fn add_types_to_union(&mut self, &mut types: Vec<Type>) {
    //     if let Type::Union(u)

    //     for ty in types {
    //         types.push(ty);
    //     }
    //  }

    pub(crate) fn add_types_to_union(&mut self, type_set: &mut Vec<Type>, types: Vec<Type>) {
        for ty in types.into_iter() {
            if let Type::Union(u) = ty.normalize() {
                self.add_types_to_union(type_set, u.clone().types);
            }

            if !ty.is_never() {
                type_set.push(ty);
            }
        }
    }
}

pub(crate) enum UnionReduction {
    None,
    Literal,
    Subtype,
}
