#![allow(non_upper_case_globals)]

use std::{
    borrow::Cow,
    collections::{hash_map::Entry, HashMap},
    iter::once,
};

use bitflags::bitflags;
use fxhash::FxHashMap;
use itertools::Itertools;
use stc_ts_ast_rnode::{RStr, RTsEntityName, RTsLit};
use stc_ts_errors::{debug::dump_type_as_string, DebugExt};
use stc_ts_generics::expander::InferTypeResult;
use stc_ts_type_ops::{generalization::prevent_generalize, Fix};
use stc_ts_types::{
    Array, ArrayMetadata, Class, ClassDef, ClassMember, Function, Id, Interface, KeywordType, KeywordTypeMetadata, LitType, Readonly, Ref,
    TplElem, TplType, Type, TypeElement, TypeLit, TypeParam, TypeParamMetadata, Union,
};
use stc_utils::{cache::Freeze, dev_span};
use swc_atoms::Atom;
use swc_common::{EqIgnoreSpan, Span, Spanned, SyntaxContext, TypeEq};
use swc_ecma_ast::TsKeywordTypeKind;
use tracing::{debug, error, info};

use crate::{
    analyzer::{assign::AssignOpts, generic::InferData, Analyzer},
    ty::TypeExt,
    util::unwrap_builtin_with_single_arg,
    VResult,
};

#[derive(Debug, Clone)]
pub(super) struct InferenceInfo {
    #[allow(unused)]
    pub type_param: Id,

    /// Candidates in covariant positions (or undefined)
    pub candidates: Vec<Type>,

    /// Candidates in contravariant positions (or undefined)
    pub contra_candidates: Vec<Type>,

    /// Cache for resolved inferred type
    ///
    /// TODO(kdy1): Make this `Option<Type>`, to match `tsc`.
    pub inferred_type: Type,

    /// Priority of current inference set
    pub priority: InferencePriority,
    /// True if all inferences are to top level occurrences
    pub top_level: bool,
    /// True if inferences are fixed
    pub is_fixed: bool,
    #[allow(unused)]
    pub implied_arity: Option<isize>,

    pub rest_index: Option<usize>,
}

/// # Default
///
/// All fields default to `false`.
#[derive(Debug, Clone, Copy, Default)]
pub(crate) struct InferTypeOpts {
    pub priority: InferencePriority,

    pub for_fn_assignment: bool,
    /// Defaults to false because
    ///
    /// ```ts
    /// function foo<T>(x: T, y: T) {
    ///     return x;
    /// }
    ///
    /// foo(1, '')
    /// ```
    ///
    /// the code above is error.
    ///
    ///
    /// This is `true` for array
    pub append_type_as_union: bool,

    /// If true, inference result can be `unknown`.
    pub use_error: bool,

    /// If we are inferring a type using another type, we should
    ///
    ///  - Prevent generalization of literals.
    ///
    /// because literals are present in the another type.
    pub is_type_ann: bool,

    /// Ignore `Object` builtin type.
    pub ignore_builtin_object_interface: bool,

    pub skip_initial_union_check: bool,

    /// If true, we are inferring a type from [Type::Rest]
    pub is_inferring_rest_type: bool,

    pub exclude_null_and_undefined: bool,

    pub index_tuple_with_param: bool,

    /// If this value is different, `is_inferring_rest_type` behaves
    /// differently, to avoid inferring `([...T], [...T])` & `(['a', 'b'], ['c',
    /// ['d'])`  as a tuple with 4 elements.
    pub rest_type_index: Option<usize>,
}

bitflags! {
    pub struct InferencePriority: i32 {
        const None = 0;
        /// Naked type variable in union or intersection type
        const NakedTypeVariable = 1 << 0;
        /// Speculative tuple inference
        const SpeculativeTuple = 1 << 1;
        /// Source of inference originated within a substitution type's substitute
        const SubstituteSource = 1 << 2;
        /// Reverse inference for homomorphic mapped type
        const HomomorphicMappedType = 1 << 3;
        /// Partial reverse inference for homomorphic mapped type
        const PartialHomomorphicMappedType = 1 << 4;
        /// Reverse inference for mapped type
        const MappedTypeConstraint = 1 << 5;
        /// Conditional type in contravariant position
        const ContravariantConditional = 1 << 6;
        /// Inference made from return type of generic function
        const ReturnType = 1 << 7;
        /// Inference made from a string literal to a keyof T
        const LiteralKeyof = 1 << 8;
        /// Don't infer from constraints of instantiable types
        const NoConstraints = 1 << 9;
        /// Always use strict rules for contravariant inferences
        const AlwaysStrict = 1 << 10;
        /// Seed for inference priority tracking
        const MaxValue = 1 << 11;

        /// Inference circularity (value less than all other priorities)
        const Circularity = -1;

        /// These priorities imply that the resulting type should be a combination
        /// of all candidates
        const PriorityImpliesCombination =
            Self::ReturnType.bits | Self::MappedTypeConstraint.bits | Self::LiteralKeyof.bits;
    }
}

impl Default for InferencePriority {
    fn default() -> Self {
        Self::None
    }
}

impl Analyzer<'_, '_> {
    /// Ported from `inferFromMatchingTypes` of `tsc`.
    pub(super) fn infer_from_matching_types(
        &mut self,
        span: Span,
        inferred: &mut InferData,
        sources: &[Type],
        targets: &[Type],
        matches: impl Fn(&mut Analyzer, &Type, &Type) -> bool,
        opts: InferTypeOpts,
    ) -> VResult<(Vec<Type>, Vec<Type>)> {
        let _tracing = dev_span!("infer_from_matching_types");

        let mut matched_sources: Vec<Type> = vec![];
        let mut matched_targets: Vec<Type> = vec![];

        for t in targets {
            for s in sources {
                if matches(self, t, s) {
                    self.infer_type(span, inferred, t, s, opts)?;

                    if matched_sources.iter().all(|ty| !ty.type_eq(s)) {
                        matched_sources.push(s.clone());
                    }
                    if matched_targets.iter().all(|ty| !ty.type_eq(t)) {
                        matched_targets.push(t.clone());
                    }
                }
            }
        }

        let sources = if matched_sources.is_empty() {
            sources.to_vec()
        } else {
            sources
                .iter()
                .filter(|s| !matched_sources.iter().any(|ty| ty.type_eq(s)))
                .cloned()
                .collect()
        };

        let targets = if matched_targets.is_empty() {
            targets.to_vec()
        } else {
            targets
                .iter()
                .filter(|t| !matched_targets.iter().any(|ty| ty.type_eq(t)))
                .cloned()
                .collect()
        };

        Ok((sources, targets))
    }

    /// Ported from `tsc`.
    pub(super) fn infer_type_using_union(
        &mut self,
        span: Span,
        inferred: &mut InferData,
        param: &Union,
        arg: &Type,
        opts: InferTypeOpts,
    ) -> VResult<()> {
        let _tracing = dev_span!("infer_type_using_union");

        let (mut temp_sources, mut temp_targets) = self.infer_from_matching_types(
            span,
            inferred,
            &once(arg).flat_map(|v| v.iter_union()).cloned().collect_vec(),
            &param.types,
            |this, s, t| this.is_type_or_base_identical_to(s, t),
            opts,
        )?;

        // TODO(kdy1): Remove these freeze calls
        temp_sources.freeze();
        temp_targets.freeze();

        let (sources, targets) = self.infer_from_matching_types(
            span,
            inferred,
            &temp_sources,
            &temp_targets,
            |this, s, t| this.is_type_closely_matched_by(s, t),
            opts,
        )?;

        if targets.is_empty() {
            return Ok(());
        }

        let target = Type::new_union(span, targets).freezed();

        if sources.is_empty() {
            return self.infer_from_types(
                span,
                inferred,
                arg,
                &target,
                InferTypeOpts {
                    // Prevent infinite recursion
                    skip_initial_union_check: true,
                    ..opts
                },
            );
        }

        let source = Type::new_union(span, sources).freezed();

        self.infer_from_types(
            span,
            inferred,
            &source,
            &target,
            InferTypeOpts {
                // Prevent infinite recursion
                skip_initial_union_check: true,
                ..opts
            },
        )
    }

    /// Ported from `inferToMultipleTypes` of `tsc`.
    pub(super) fn infer_to_multiple_types(
        &mut self,
        span: Span,
        inferred: &mut InferData,
        source: &Type,
        targets: &[Type],
        is_target_union: bool,
        opts: InferTypeOpts,
    ) -> VResult<()> {
        let mut type_var_count = 0;

        if is_target_union {
            let mut naked_type_var = None;

            let sources = if let Type::Union(source) = source.normalize() {
                Cow::Borrowed(&source.types)
            } else {
                Cow::Owned(vec![source.clone()])
            };
            let mut matched = vec![false; sources.len()];
            let mut inference_circularity = false;

            // First infer to types that are not naked type variables. For each
            // source type we track whether inferences were made from that
            // particular type to some target with equal priority (i.e. of equal
            // quality) to what we would infer for a naked type parameter.

            for t in targets {
                if let Some(..) = self.get_inference_info_for_type(inferred, t) {
                    naked_type_var = Some(t.clone());
                    type_var_count += 1;
                } else {
                    for (i, source) in sources.iter().enumerate() {
                        let saved_inference_priority = inferred.priority;
                        inferred.priority = InferencePriority::MaxValue;
                        self.infer_type(span, inferred, t, source, opts)?;
                        if inferred.priority == opts.priority {
                            matched[i] = true;
                        }

                        inference_circularity |= inferred.priority == InferencePriority::Circularity;
                        inferred.priority = InferencePriority::min(inferred.priority, saved_inference_priority);
                    }
                }
            }

            if type_var_count == 0 {
                // If every target is an intersection of types containing a
                // single naked type variable, make a lower
                // priority inference to that type variable. This handles
                // inferring from 'A | B' to 'T & (X | Y)' where
                // we want to infer 'A | B' for T.

                let intersection_type_var = self.get_single_type_variable_from_intersection_types(span, inferred, targets);

                if let Some(intersection_type_var) = intersection_type_var {
                    self.infer_with_priority(
                        span,
                        inferred,
                        source,
                        &intersection_type_var,
                        InferencePriority::NakedTypeVariable,
                        opts,
                    )?;
                }
                return Ok(());
            }

            // If the target has a single naked type variable and no inference
            // circularities were encountered above (meaning we
            // explored the types fully), create a union of the source
            // types from which no inferences have been made so far and infer
            // from that union to the naked type variable.

            if type_var_count == 0 && !inference_circularity {
                let unmatched = sources
                    .iter()
                    .enumerate()
                    .filter(|(i, _)| !matched[*i])
                    .map(|(_, t)| t.clone())
                    .collect_vec();
                if !unmatched.is_empty() {
                    return self.infer_from_types(span, inferred, &Type::new_union(span, unmatched), &naked_type_var.unwrap(), opts);
                }
            }
        } else {
            // We infer from types that are not naked type variables first so
            // that inferences we make from nested naked type
            // variables and given slightly higher priority by virtue
            // of being first in the candidates array.

            for t in targets {
                if self.get_inference_info_for_type(inferred, t).is_some() {
                    type_var_count += 1;
                } else {
                    self.infer_from_types(span, inferred, source, t, opts)?;
                }
            }
        }

        if if !is_target_union {
            type_var_count == 1
        } else {
            type_var_count > 0
        } {
            for t in targets {
                if let Some(..) = self.get_inference_info_for_type(inferred, t) {
                    self.infer_with_priority(span, inferred, source, t, InferencePriority::NakedTypeVariable, opts)?;
                }
            }
        }

        Ok(())
    }

    /// Ported from `inferFromTypes` of `tsc`.
    pub(super) fn infer_from_types(
        &mut self,
        span: Span,
        inferred: &mut InferData,
        source: &Type,
        target: &Type,
        opts: InferTypeOpts,
    ) -> VResult<()> {
        self.infer_type(span, inferred, target, source, opts)
    }

    pub(super) fn infer_with_priority(
        &mut self,
        span: Span,
        inferred: &mut InferData,
        source: &Type,
        target: &Type,
        new_priority: InferencePriority,
        mut opts: InferTypeOpts,
    ) -> VResult<()> {
        let saved_priority = opts.priority;
        opts.priority |= new_priority;
        self.infer_from_types(span, inferred, source, target, opts)?;

        Ok(())
    }

    /// `inferToMultipleTypesWithPriority`
    pub(super) fn infer_to_multiple_types_with_priority(
        &mut self,
        span: Span,
        inferred: &mut InferData,
        source: &Type,
        targets: &[Type],
        new_priority: InferencePriority,
        is_target_union: bool,
        mut opts: InferTypeOpts,
    ) -> VResult<()> {
        let _tracing = dev_span!("infer_to_multiple_types_with_priority");

        let saved_priority = opts.priority;
        opts.priority |= new_priority;
        self.infer_to_multiple_types(span, inferred, source, targets, is_target_union, opts)?;

        Ok(())
    }

    /// Ported from `inferFromContravariantTypes` of `tsc`.
    pub(super) fn infer_from_contravariant_types(
        &mut self,
        span: Span,
        inferred: &mut InferData,
        source: &Type,
        target: &Type,
        opts: InferTypeOpts,
    ) -> VResult<()> {
        let old = inferred.contravariant;
        inferred.contravariant = true;
        let res = self.infer_from_types(span, inferred, source, target, opts);
        inferred.contravariant = old;

        res
    }

    /// Ported from `getInferenceInfoForType` of `tsc`.
    fn get_inference_info_for_type<'a>(&mut self, inferred: &'a mut InferData, ty: &Type) -> Option<&'a mut InferenceInfo> {
        if let Type::Param(param) = ty {
            return inferred.type_params.get_mut(&param.name);
        }

        None
    }

    /// Ported from `getSingleTypeVariableFromIntersectionTypes` of `tsc`.
    fn get_single_type_variable_from_intersection_types(&mut self, span: Span, inferred: &mut InferData, types: &[Type]) -> Option<Type> {
        let mut type_var: Option<Type> = None;

        for ty in types {
            if let Type::Intersection(t) = ty.normalize() {
                if let Some(t) = t.types.iter().find(|t| self.get_inference_info_for_type(inferred, ty).is_some()) {
                    if let Some(type_var) = type_var {
                        if !type_var.type_eq(t) {
                            return None;
                        }
                    }
                    type_var = Some(t.clone());
                } else {
                    return None;
                }
            } else {
                return None;
            }
        }

        type_var
    }

    /// Ported from `inferToTemplateLiteralType` of `tsc`.
    pub(super) fn infer_to_tpl_lit_type(
        &mut self,
        span: Span,
        inferred: &mut InferData,
        source: &Type,
        target: &TplType,
        opts: InferTypeOpts,
    ) -> VResult<()> {
        let mut matches = self.infer_types_from_tpl_lit_type(span, source, target)?;
        matches.freeze();

        let _tracing = dev_span!("infer_to_tpl_lit_type", matches = matches.as_ref().map_or(0, |v| v.len()));

        // When the target template literal contains only placeholders (meaning that
        // inference is intended to extract single characters and remainder
        // strings) and inference fails to produce matches, we want to infer 'never' for
        // each placeholder such that instantiation with the inferred value(s) produces
        // 'never', a type for which an assignment check will fail. If we make
        // no inferences, we'll likely end up with the constraint 'string' which,
        // upon instantiation, would collapse all the placeholders to just 'string', and
        // an assignment check might succeed. That would be a pointless and
        // confusing outcome.
        if matches.is_some() || target.quasis.iter().all(|v| v.value.len() == 0) {
            for (i, target) in target.types.iter().enumerate() {
                let source = matches
                    .as_ref()
                    .map(|matches| matches[i].clone())
                    .unwrap_or_else(|| Type::never(span, Default::default()));

                // If we are inferring from a string literal type to a type
                // variable whose constraint includes one of the
                // allowed template literal placeholder types, infer from a
                // literal type corresponding to the constraint.
                if source.is_str_lit() && target.is_type_param() {
                    // TODO: Implement logic
                    error!("unimplemented: infer_to_tpl_lit_type");
                }

                self.infer_from_types(span, inferred, &source, target, opts)?;
            }
        }

        Ok(())
    }

    /// Ported from `inferTypesFromTemplateLiteralType` of `tsc`.
    pub(crate) fn infer_types_from_tpl_lit_type(&mut self, span: Span, source: &Type, target: &TplType) -> VResult<Option<Vec<Type>>> {
        match source.normalize() {
            Type::Lit(LitType {
                lit: RTsLit::Str(source), ..
            }) => self.infer_from_lit_parts_to_tpl_lit(span, &[source.value.clone().into()], &[], target),
            Type::Tpl(source) => {
                if (*source.quasis).eq_ignore_span(&*target.quasis) {
                    Ok(Some(
                        source
                            .types
                            .iter()
                            .map(|ty| self.get_string_like_type_for_type(ty).into_owned())
                            .collect(),
                    ))
                } else {
                    self.infer_from_lit_parts_to_tpl_lit(
                        span,
                        &source.quasis.iter().map(|v| v.value.clone()).collect_vec(),
                        &source.types,
                        target,
                    )
                }
            }
            _ => Ok(None),
        }
    }

    /// Ported from `inferFromLiteralPartsToTemplateLiteral` of `tsc`.
    ///
    /// This function infers from the text parts and type parts of a source
    /// literal to a target template literal. The number of text parts is
    /// always one more than the number of type parts, and a source string
    /// literal is treated as a source with one text part and zero type
    /// parts. The function returns an array of inferred string or template
    /// literal types corresponding to the placeholders in the target
    /// template literal, or undefined if the source doesn't match the target.
    ///
    /// We first check that the starting source text part matches the starting
    /// target text part, and that the ending source text part ends matches
    /// the ending target text part. We then iterate through the remaining
    /// target text parts, finding a match for each in the source and
    /// inferring string or template literal types created from the segments of
    /// the source that occur between the matches. During this iteration,
    /// seg holds the index of the current text part in the sourceTexts
    /// array and pos holds the current character position in the current text
    /// part.
    //
    /// Consider inference from type `<<${string}>.<${number}-${number}>>` to
    /// type `<${string}.${string}>`, i.e.   sourceTexts = ['<<', '>.<',
    /// '-', '>>']   sourceTypes = [string, number, number]
    ///   target.texts = ['<', '.', '>']
    /// We first match '<' in the target to the start of '<<' in the source and
    /// '>' in the target to the end of '>>' in the source. The first match
    /// for the '.' in target occurs at character 1 in the source text part at
    /// index 1, and thus the first inference is the template literal type
    /// `<${string}>`. The remainder of the source makes up the second
    /// inference, the template literal type `<${number}-${number}>`.
    #[allow(unused_assignments)]
    #[allow(clippy::needless_range_loop)]
    fn infer_from_lit_parts_to_tpl_lit(
        &mut self,
        span: Span,
        source_texts: &[Atom],
        source_types: &[Type],
        target: &TplType,
    ) -> VResult<Option<Vec<Type>>> {
        let last_source_index = source_texts.len() - 1;
        let source_start_text = &source_texts[0];
        let source_end_text = &source_texts[last_source_index];
        let target_texts = &target.quasis;
        let last_target_index = target_texts.len() - 1;
        let target_start_text = &target_texts[0].value;
        let target_end_text = &target_texts[last_target_index].value;

        if last_source_index == 0 && source_start_text.len() < target_start_text.len() + target_end_text.len()
            || !source_start_text.starts_with(&**target_start_text)
            || !source_end_text.ends_with(&**target_end_text)
        {
            return Ok(None);
        }

        let remaining_end_text = &source_end_text[0..source_end_text.len() - target_end_text.len()];
        let mut matches = Vec::<Type>::new();
        let mut seg = 0;
        let mut pos = target_start_text.len();

        let get_source_text = |index: usize| {
            if index < last_source_index {
                &*source_texts[index]
            } else {
                remaining_end_text
            }
        };

        macro_rules! add_match {
            ($s:expr, $p:expr) => {{
                let match_type = if $s == seg {
                    let value = source_texts[seg][pos..$p].into();
                    Type::Lit(LitType {
                        span,
                        lit: RTsLit::Str(RStr { span, raw: None, value }),
                        metadata: Default::default(),
                        tracker: Default::default(),
                    })
                } else {
                    Type::Tpl(TplType {
                        span,
                        quasis: std::iter::once(source_texts[seg][pos..].into())
                            .chain(source_texts[seg + 1..$s].iter().cloned())
                            .chain(std::iter::once(get_source_text($s)[0..$p].into()))
                            .map(|value: Atom| TplElem { span, value })
                            .collect(),
                        types: source_types[seg..$s].iter().map(|v| v.clone()).collect(),
                        metadata: Default::default(),
                        tracker: Default::default(),
                    })
                };

                matches.push(match_type);
                seg = $s;
                pos = $p;
            }};
        }

        for i in 1..last_target_index {
            let delim = &target_texts[i].value;

            if delim.len() > 0 {
                let mut s = seg;
                let mut p = pos as isize;

                loop {
                    match get_source_text(s)[p as usize..].find(&**delim).map(|v| v as isize) {
                        Some(v) => {
                            p += v;
                        }
                        None => {
                            p = -1;
                        }
                    };
                    if p >= 0 {
                        break;
                    }
                    s += 1;
                    if s == source_texts.len() {
                        return Ok(None);
                    }
                    p = 0;
                }

                add_match!(s, p as usize);
                pos += delim.len();
            } else if pos < get_source_text(seg).len() {
                add_match!(seg, pos + 1)
            } else if seg < last_source_index {
                add_match!(seg + 1, 0)
            } else {
                return Ok(None);
            }
        }

        add_match!(last_source_index, get_source_text(last_source_index).len());

        Ok(Some(matches))
    }

    pub(super) fn insert_inferred(
        &mut self,
        span: Span,
        inferred: &mut InferData,
        tp: &TypeParam,
        ty: Cow<Type>,
        opts: InferTypeOpts,
    ) -> VResult<()> {
        self.insert_inferred_raw(span, inferred, tp.name.clone(), ty, opts)
    }

    /// # Rules
    ///
    /// ## Type literal
    ///
    /// If one of type literal is `specified` according to the metadata, type
    /// inference is done.
    ///
    /// See:
    ///
    /// ```ts
    /// declare function f<T>(...items: T[]): T;
    /// declare let data: { a: 1, b: "abc", c: true };
    /// declare let data2: { b: "foo", c: true };
    ///
    /// // Not specified
    /// let e1 = f({ a: 1, b: 2 }, { a: "abc" }, {});
    /// let e2 = f({}, { a: "abc" }, { a: 1, b: 2 });
    ///
    /// // Type inference is done if at least one element is specified.
    /// let e3 = f(data, { a: 2 }); // Error
    /// let e4 = f({ a: 2 }, data); // Error
    /// let e5 = f(data, data2); // Error
    /// ```
    pub(super) fn insert_inferred_raw(
        &mut self,
        span: Span,
        inferred: &mut InferData,
        name: Id,
        ty: Cow<Type>,
        opts: InferTypeOpts,
    ) -> VResult<()> {
        let marks = self.marks();

        info!("Inferred {} as {}", name, dump_type_as_string(&ty));

        if let Type::Param(ty) = ty.normalize() {
            if name == ty.name {
                return Ok(());
            }
        }

        if ty.is_any() && self.is_implicitly_typed(&ty) {
            if inferred.type_params.contains_key(&name) {
                return Ok(());
            }

            match inferred.defaults.entry(name.clone()) {
                Entry::Occupied(..) => {}
                Entry::Vacant(e) => {
                    e.insert(Type::Param(TypeParam {
                        span: ty.span(),
                        name,
                        constraint: None,
                        default: None,
                        metadata: TypeParamMetadata {
                            common: ty.metadata(),
                            ..Default::default()
                        },
                        tracker: Default::default(),
                    }));
                }
            }

            //
            return Ok(());
        }

        self.upsert_inferred(span, inferred, name, &ty, opts)
    }

    pub(super) fn upsert_inferred(
        &mut self,
        span: Span,
        inferred: &mut InferData,
        name: Id,
        arg: &Type,
        opts: InferTypeOpts,
    ) -> VResult<()> {
        let arg = match arg.normalize() {
            Type::Union(arg) if opts.exclude_null_and_undefined => {
                Cow::Owned(Type::new_union(arg.span, arg.types.iter().filter(|ty| !ty.is_null_or_undefined()).cloned()).freezed())
            }
            _ => Cow::Borrowed(arg),
        };
        arg.assert_clone_cheap();

        // TODO(kdy1): Verify if this is correct
        if let Type::Param(arg) = arg.normalize() {
            if let Some(inverse) = inferred.type_params.get(&arg.name) {
                if inverse.priority < opts.priority {
                    return Ok(());
                }
            }
        }

        match inferred.type_params.entry(name.clone()) {
            Entry::Occupied(mut e) => {
                if e.get().is_fixed {
                    return Ok(());
                }

                let _tracing = dev_span!(
                    "infer_type: type param",
                    name = name.as_str(),
                    new = tracing::field::display(&dump_type_as_string(&arg)),
                    prev = tracing::field::display(&dump_type_as_string(&e.get().inferred_type))
                );

                if opts.priority < e.get().priority {
                    e.get_mut().candidates = Default::default();
                    e.get_mut().contra_candidates = Default::default();
                    e.get_mut().top_level = true;
                    e.get_mut().priority = opts.priority;
                    e.get_mut().rest_index = None;
                }

                if opts.priority == e.get().priority {
                    // Identical
                    if e.get().inferred_type.type_eq(&*arg) {
                        return Ok(());
                    }

                    if opts.append_type_as_union
                        || self
                            .assign_with_opts(
                                &mut Default::default(),
                                &arg.clone().into_owned().generalize_lit(),
                                &e.get().inferred_type.clone().generalize_lit(),
                                AssignOpts {
                                    span,
                                    do_not_convert_enum_to_string_nor_number: true,
                                    ignore_enum_variant_name: true,
                                    ignore_tuple_length_difference: true,
                                    ..Default::default()
                                },
                            )
                            .is_ok()
                    {
                        if (e.get().inferred_type.is_any() || e.get().inferred_type.is_unknown()) && !(arg.is_any() || arg.is_unknown()) {
                            return Ok(());
                        }

                        if opts.ignore_builtin_object_interface && arg.is_builtin_interface("Object") {
                            return Ok(());
                        }

                        debug!("Overriding");
                        let new = if self
                            .assign_with_opts(
                                &mut Default::default(),
                                &arg,
                                &e.get().inferred_type,
                                AssignOpts {
                                    span,
                                    do_not_convert_enum_to_string_nor_number: true,
                                    ..Default::default()
                                },
                            )
                            .is_ok()
                        {
                            arg.into_owned()
                        } else if opts.is_inferring_rest_type
                            && matches!(e.get().inferred_type.normalize(), Type::Tuple(..))
                            && match arg.normalize() {
                                Type::Tuple(tuple) => tuple.elems.len() == 1,
                                _ => false,
                            }
                        {
                            if opts.rest_type_index == e.get().rest_index {
                                // If both are tuples with length is 1, we merge
                                // them.
                                //
                                // ['a'] [1] => ['a', 1]
                                let mut prev = e.get().inferred_type.clone().expect_tuple();
                                prev.metadata.prevent_tuple_to_array = true;

                                let mut new_elem = arg.as_tuple().unwrap().elems[0].clone();
                                new_elem.ty = box new_elem.ty.generalize_lit();
                                prev.elems.push(new_elem);

                                Type::Tuple(prev).freezed()
                            } else {
                                e.get().inferred_type.clone()
                            }
                        } else {
                            Type::new_union(span, vec![e.get().inferred_type.clone(), arg.into_owned()]).freezed()
                        };
                        new.assert_clone_cheap();
                        e.get_mut().inferred_type = new;
                        return Ok(());
                    }

                    // If we inferred T as `number`, we don't need to add `1`.
                    if self
                        .assign_with_opts(
                            &mut Default::default(),
                            &e.get().inferred_type.clone().generalize_lit(),
                            &arg.clone().into_owned().generalize_lit(),
                            AssignOpts {
                                span,
                                ..Default::default()
                            },
                        )
                        .is_ok()
                    {
                        debug!("Ignoring the new type");

                        return Ok(());
                    }

                    debug!("Cannot append");
                    inferred.skip_generalization = true;

                    if opts.use_error {
                        inferred.errored.insert(name);
                    }
                }
            }
            Entry::Vacant(e) => {
                let mut arg = arg.clone();

                if opts.is_inferring_rest_type {
                    arg = Cow::Owned(arg.into_owned().generalize_lit().freezed());
                }

                let arg = arg.into_owned();

                arg.assert_clone_cheap();

                e.insert(InferenceInfo {
                    type_param: name,
                    candidates: Default::default(),
                    contra_candidates: Default::default(),
                    inferred_type: arg,
                    priority: opts.priority,
                    top_level: true,
                    is_fixed: false,
                    implied_arity: Default::default(),
                    rest_index: opts.rest_type_index,
                });
            }
        }

        Ok(())
    }

    /// Infer types, using `param` and `arg`.
    pub(crate) fn infer_type_with_types(
        &mut self,
        span: Span,
        type_params: &[TypeParam],
        param: &Type,
        arg: &Type,
        opts: InferTypeOpts,
    ) -> VResult<FxHashMap<Id, Type>> {
        let _tracing = dev_span!("infer_type_with_types");

        if cfg!(debug_assertions) {
            // Assertion for deep clone
            let _ = type_params.to_vec();
            param.assert_clone_cheap();
            arg.assert_clone_cheap();
        }

        let mut inferred = InferData::default();

        self.infer_type(span, &mut inferred, param, arg, InferTypeOpts { ..opts })
            .context("tried to infer type using two type")?;

        let map = self.finalize_inference(span, type_params, inferred);

        Ok(map.types)
    }

    /// Handle some special builtin types

    pub(super) fn infer_builtin(
        &mut self,
        span: Span,
        inferred: &mut InferData,
        param: &Type,
        arg: &Type,
        opts: InferTypeOpts,
    ) -> Option<VResult<()>> {
        let param = param.normalize();
        let arg = arg.normalize();

        if let Some(elem_type) = unwrap_builtin_with_single_arg(param, "ReadonlyArray").or_else(|| match param.normalize() {
            Type::Interface(Interface { name, body, .. }) => {
                if name == "ReadonlyArray" {
                    body.iter()
                        .filter_map(|v| match v {
                            TypeElement::Index(i) => i.type_ann.as_deref(),
                            _ => None,
                        })
                        .next()
                } else {
                    None
                }
            }
            _ => None,
        }) {
            return Some(self.infer_type(
                span,
                inferred,
                &Type::Array(Array {
                    span: param.span(),
                    elem_type: box elem_type.clone(),
                    metadata: ArrayMetadata {
                        common: param.metadata(),
                        ..Default::default()
                    },
                    tracker: Default::default(),
                }),
                arg,
                InferTypeOpts {
                    append_type_as_union: true,
                    ..opts
                },
            ));
        }

        if let Type::Array(Array { elem_type, .. }) = param.normalize() {
            match arg.normalize() {
                Type::Ref(Ref {
                    type_name: RTsEntityName::Ident(type_name),
                    type_args: Some(type_args),
                    ..
                }) if type_name.sym == *"ReadonlyArray" => {
                    return Some(self.infer_type(
                        span,
                        inferred,
                        elem_type,
                        &type_args.params[0],
                        InferTypeOpts {
                            append_type_as_union: true,
                            ..opts
                        },
                    ));
                }
                _ => {}
            }
        }

        None
    }

    pub(super) fn infer_type_using_interface(
        &mut self,
        span: Span,
        inferred: &mut InferData,
        param: &Interface,
        arg: &Type,
        opts: InferTypeOpts,
    ) -> VResult<()> {
        match arg.normalize() {
            Type::Interface(arg) => {
                self.infer_type_using_interface_and_interface(span, inferred, param, arg, opts)?;
            }
            Type::TypeLit(arg) => {
                self.infer_type_using_type_elements_and_type_elements(span, inferred, &param.body, &arg.members, opts)?;
            }
            _ => {
                // Convert to a type literal.
                if let Some(arg) = self.convert_type_to_type_lit(span, Cow::Borrowed(arg))? {
                    self.infer_type_using_type_elements_and_type_elements(span, inferred, &param.body, &arg.members, opts)?;
                } else {
                    unimplemented!()
                }
            }
        }

        for parent in &param.extends {
            let parent = self
                .type_of_ts_entity_name(span, &parent.expr, parent.type_args.as_deref())?
                .freezed();
            self.infer_type(span, inferred, &parent, arg, opts)?;
        }

        Ok(())
    }

    fn infer_type_using_interface_and_interface(
        &mut self,
        span: Span,
        inferred: &mut InferData,
        param: &Interface,
        arg: &Interface,
        opts: InferTypeOpts,
    ) -> VResult<()> {
        self.infer_type_using_type_elements_and_type_elements(span, inferred, &param.body, &arg.body, opts)?;

        Ok(())
    }

    /// Compare fields.
    pub(super) fn infer_type_using_type_lit_and_type_lit(
        &mut self,
        span: Span,
        inferred: &mut InferData,
        param: &TypeLit,
        arg: &TypeLit,
        opts: InferTypeOpts,
    ) -> VResult<()> {
        self.infer_type_using_type_elements_and_type_elements(span, inferred, &param.members, &arg.members, opts)
    }

    /// Returns `Ok(true)` if this method know how to infer types.
    pub(super) fn infer_type_by_converting_to_type_lit(
        &mut self,
        span: Span,
        inferred: &mut InferData,
        param: &Type,
        arg: &Type,
        opts: InferTypeOpts,
    ) -> VResult<bool> {
        let p = param.normalize();
        let a = arg.normalize();
        match (p, a) {
            (Type::Constructor(..), Type::Class(..)) | (Type::Function(..), Type::Function(..)) => return Ok(false),
            (Type::Constructor(..), _) | (Type::Function(..), _) => {
                let p = self.convert_type_to_type_lit(span, Cow::Borrowed(p))?;
                let a = self.convert_type_to_type_lit(span, Cow::Borrowed(a))?;
                if let Some(p) = p {
                    if let Some(a) = a {
                        self.infer_type_using_type_elements_and_type_elements(span, inferred, &p.members, &a.members, opts)?;
                        return Ok(true);
                    }
                }
            }
            _ => {}
        }

        Ok(false)
    }

    fn infer_type_using_type_elements_and_type_elements(
        &mut self,
        span: Span,
        inferred: &mut InferData,
        param: &[TypeElement],
        arg: &[TypeElement],
        opts: InferTypeOpts,
    ) -> VResult<()> {
        for p in param {
            for a in arg {
                let opts = match p {
                    TypeElement::Index(..) => InferTypeOpts {
                        append_type_as_union: true,
                        ..opts
                    },
                    _ => InferTypeOpts {
                        append_type_as_union: if opts.for_fn_assignment { false } else { opts.append_type_as_union },
                        ..opts
                    },
                };
                //

                match (p, a) {
                    (TypeElement::Property(p), TypeElement::Property(a)) => {
                        if self.assign(span, &mut Default::default(), &p.key.ty(), &a.key.ty()).is_ok() {
                            if let Some(pt) = &p.type_ann {
                                if let Some(at) = &a.type_ann {
                                    self.infer_type(span, inferred, pt, at, opts)?;
                                } else {
                                    dbg!((&p, &a));
                                }
                            } else {
                                dbg!((&p, &a));
                            }
                        }
                        continue;
                    }

                    (TypeElement::Property(p), TypeElement::Method(a)) => {
                        if self.key_matches(span, &p.key, &a.key, false) {
                            let span = span.with_ctxt(SyntaxContext::empty());

                            if let Some(pt) = &p.type_ann {
                                self.infer_type(
                                    span,
                                    inferred,
                                    pt,
                                    &Type::Function(Function {
                                        span,
                                        type_params: a.type_params.clone(),
                                        params: a.params.clone(),
                                        ret_ty: a.ret_ty.clone().unwrap_or_else(|| box Type::any(span, Default::default())),
                                        metadata: Default::default(),
                                        tracker: Default::default(),
                                    })
                                    .freezed(),
                                    opts,
                                )?;
                            }
                        }
                        continue;
                    }

                    (TypeElement::Method(p), TypeElement::Property(a)) => {
                        if self.key_matches(span, &a.key, &p.key, false) {
                            let span = span.with_ctxt(SyntaxContext::empty());

                            if let Some(at) = &a.type_ann {
                                self.infer_type(
                                    span,
                                    inferred,
                                    &Type::Function(Function {
                                        span,
                                        type_params: p.type_params.clone(),
                                        params: p.params.clone(),
                                        ret_ty: p.ret_ty.clone().unwrap_or_else(|| box Type::any(span, Default::default())),
                                        metadata: Default::default(),
                                        tracker: Default::default(),
                                    })
                                    .freezed(),
                                    at,
                                    opts,
                                )?;
                            }
                        }
                        continue;
                    }

                    (TypeElement::Index(p), TypeElement::Index(a)) => {
                        if p.params.type_eq(&a.params)
                            || a.params[0].ty.is_kwd(TsKeywordTypeKind::TsStringKeyword)
                                && p.params[0].ty.is_kwd(TsKeywordTypeKind::TsNumberKeyword)
                        {
                            if let Some(pt) = &p.type_ann {
                                if let Some(at) = &a.type_ann {
                                    self.infer_type(
                                        span,
                                        inferred,
                                        pt,
                                        at,
                                        InferTypeOpts {
                                            append_type_as_union: true,
                                            ..opts
                                        },
                                    )?;
                                } else {
                                    dbg!((&p, &a));
                                }
                            } else {
                                dbg!((&p, &a));
                            }
                        }
                        continue;
                    }

                    (TypeElement::Index(p), TypeElement::Property(a)) => {
                        assert_eq!(p.params.len(), 1, "Index signature should have exactly one parameter");

                        if self.assign(span, &mut Default::default(), &p.params[0].ty, &a.key.ty()).is_ok()
                            || p.params[0].ty.is_kwd(TsKeywordTypeKind::TsStringKeyword)
                        {
                            if let Some(p_ty) = &p.type_ann {
                                if let Some(arg_ty) = &a.type_ann {
                                    self.infer_type(
                                        span,
                                        inferred,
                                        p_ty,
                                        arg_ty,
                                        InferTypeOpts {
                                            append_type_as_union: true,
                                            ..opts
                                        },
                                    )?;
                                }
                            }
                        }

                        continue;
                    }

                    (TypeElement::Method(p), TypeElement::Method(a)) => {
                        if self.assign(span, &mut Default::default(), &p.key.ty(), &a.key.ty()).is_ok() {
                            self.infer_type_of_fn_params(span, inferred, &p.params, &a.params, opts)?;

                            if let Some(p_ret) = &p.ret_ty {
                                if let Some(a_ret) = &a.ret_ty {
                                    self.infer_type(span, inferred, p_ret, a_ret, opts)?;
                                }
                            }
                        }

                        continue;
                    }

                    (TypeElement::Constructor(p), TypeElement::Constructor(a)) => {
                        self.infer_type_of_fn_params(span, inferred, &p.params, &a.params, opts)?;

                        if let Some(p_ret) = &p.ret_ty {
                            if let Some(a_ret) = &a.ret_ty {
                                self.infer_type(span, inferred, p_ret, a_ret, opts)?;
                            }
                        }

                        continue;
                    }

                    (TypeElement::Call(p), TypeElement::Call(a)) => {
                        self.infer_type_of_fn_params(span, inferred, &p.params, &a.params, opts)?;

                        if let Some(p_ret) = &p.ret_ty {
                            if let Some(a_ret) = &a.ret_ty {
                                self.infer_type(span, inferred, p_ret, a_ret, opts)?;
                            }
                        }

                        continue;
                    }

                    (TypeElement::Call(..), _) | (TypeElement::Constructor(..), _) => {
                        // Prevent log
                        continue;
                    }

                    _ => {}
                }

                error!("unimplemented: type inference: type element:\nParam = {:#?}\nArg = {:#?}", p, a);
            }
        }

        Ok(())
    }

    pub(super) fn infer_type_using_readonly(
        &mut self,
        span: Span,
        inferred: &mut InferData,
        param: &Readonly,
        arg: &Type,
        opts: InferTypeOpts,
    ) -> VResult<()> {
        self.infer_type(span, inferred, &param.ty, arg, opts)
    }

    pub(super) fn infer_types_using_class(
        &mut self,
        span: Span,
        inferred: &mut InferData,
        param: &Class,
        arg: &Class,
        opts: InferTypeOpts,
    ) -> VResult<()> {
        self.infer_types_using_class_def(span, inferred, &param.def, &arg.def, opts)
    }

    pub(super) fn infer_types_using_class_def(
        &mut self,
        span: Span,
        inferred: &mut InferData,
        param: &ClassDef,
        arg: &ClassDef,
        opts: InferTypeOpts,
    ) -> VResult<()> {
        for pm in &param.body {
            for am in &arg.body {
                match (pm, am) {
                    (ClassMember::Property(p), ClassMember::Property(a)) if p.is_static == a.is_static => {
                        if self.key_matches(span, &p.key, &a.key, false) {
                            if let Some(p_ty) = &p.value {
                                if let Some(a_ty) = &a.value {
                                    self.infer_type(span, inferred, p_ty, a_ty, opts)?;
                                }
                            }
                        }
                    }

                    _ => {}
                }
            }
        }

        // TODO(kdy1): Check for parents.
        Ok(())
    }

    pub(super) fn finalize_inference(&self, span: Span, type_params: &[TypeParam], inferred: InferData) -> InferTypeResult {
        let mut map = HashMap::default();

        for (k, mut ty) in inferred.type_params {
            let tp = type_params.iter().find(|tp| tp.name == k);

            self.replace_null_or_undefined_while_defaulting_to_any(&mut ty.inferred_type);

            if !ty.top_level {
                if let Some(tp) = &tp {
                    if tp.constraint.is_none() {
                        // ty.inferred_type =
                        // ty.inferred_type.foldable().fold_with(&mut
                        // LitGeneralizer).fixed();
                    }
                }
            }

            ty.inferred_type.fix();
            ty.inferred_type.freeze();

            map.insert(k, ty.inferred_type);
        }

        InferTypeResult {
            types: map,
            errored: inferred.errored,
        }
    }

    /// TODO(kdy1): Handle union
    fn replace_null_or_undefined_while_defaulting_to_any(&self, ty: &mut Type) {
        if ty.is_kwd(TsKeywordTypeKind::TsUndefinedKeyword) {
            *ty = Type::any(
                ty.span(),
                KeywordTypeMetadata {
                    common: ty.metadata(),
                    ..Default::default()
                },
            );
            return;
        }

        if ty.is_kwd(TsKeywordTypeKind::TsNullKeyword) {
            *ty = Type::any(
                ty.span(),
                KeywordTypeMetadata {
                    common: ty.metadata(),
                    ..Default::default()
                },
            );
            return;
        }

        if let Type::Tuple(..) = ty.normalize() {
            match ty.normalize_mut() {
                Type::Tuple(ty) => {
                    for elem in ty.elems.iter_mut() {
                        self.replace_null_or_undefined_while_defaulting_to_any(&mut elem.ty);
                    }
                }
                _ => unreachable!(),
            }
        }
    }

    pub(super) fn prevent_generalization_of_top_level_types(
        &self,
        type_params: &[TypeParam],
        ret_ty: Option<&Type>,
        inferred: &mut InferData,
        is_from_type_ann: bool,
    ) {
        let _tracing = dev_span!("prevent_generalization_of_top_level_types");

        if is_from_type_ann {
            if let Some(ret_ty) = ret_ty {
                if let Some(ret_ty) = unwrap_builtin_with_single_arg(ret_ty, "Promise") {
                    self.prevent_generalization_of_top_level_types(type_params, Some(ret_ty), inferred, is_from_type_ann)
                }

                match ret_ty.normalize() {
                    Type::Param(ret_ry) => {
                        if let Some(ty) = inferred.type_params.get_mut(&ret_ry.name) {
                            // prevent_generalize(&mut ty.inferred_type);
                        }
                    }
                    Type::Union(ret_ty) => {
                        for ty in &ret_ty.types {
                            self.prevent_generalization_of_top_level_types(type_params, Some(ty), inferred, is_from_type_ann)
                        }
                    }
                    _ => (),
                }
            }
        }
    }

    /// Prevent generalizations if a type parameter extends literal.
    pub(super) fn prevent_generalization_of_inferred_types(
        &mut self,
        type_params: &[TypeParam],
        inferred: &mut InferData,
        is_from_type_ann: bool,
    ) {
        let _tracing = dev_span!("prevent_generalization_of_inferred_types");

        for type_param in type_params {
            if !inferred.skip_generalization {
                match type_param.constraint.as_deref() {
                    Some(Type::Lit(..)) => {}

                    Some(ty) => {
                        if !should_prevent_generalization(ty) {
                            continue;
                        }
                    }
                    _ => continue,
                }
            }

            if let Some(ty) = inferred.type_params.get_mut(&type_param.name) {
                prevent_generalize(&mut ty.inferred_type);
                ty.inferred_type.freeze()
            }
        }
    }
}

fn should_prevent_generalization(constraint: &Type) -> bool {
    match constraint.normalize() {
        Type::Lit(LitType {
            lit: RTsLit::Str(..) | RTsLit::Number(..) | RTsLit::Bool(..),
            ..
        })
        | Type::Keyword(KeywordType {
            kind: TsKeywordTypeKind::TsStringKeyword | TsKeywordTypeKind::TsNumberKeyword | TsKeywordTypeKind::TsBooleanKeyword,
            ..
        }) => true,
        Type::Index(..) => true,

        Type::Union(Union { ref types, .. }) => types.iter().all(should_prevent_generalization),
        _ => false,
    }
}
