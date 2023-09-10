use std::{borrow::Cow, time::Instant};

use itertools::Itertools;
use stc_ts_ast_rnode::{RArrayLit, RExpr, RExprOrSpread, RInvalid, RNumber, RTsLit};
use stc_ts_errors::{
    debug::{dump_type_as_string, force_dump_type_as_string},
    DebugExt, ErrorKind,
};
use stc_ts_type_ops::Fix;
use stc_ts_types::{
    type_id::SymbolId, Array, CommonTypeMetadata, ComputedKey, Intersection, Key, KeywordType, KeywordTypeMetadata, LitType, Symbol, Tuple,
    TupleElement, Type, TypeParam, TypeParamInstantiation, Union, UnionMetadata,
};
use stc_utils::{
    cache::Freeze,
    dev_span,
    ext::{SpanExt, TypeVecExt},
};
use swc_atoms::js_word;
use swc_common::{Span, Spanned, SyntaxContext};
use swc_ecma_ast::{EsVersion, TsKeywordTypeKind};
use tracing::debug;

use crate::{
    analyzer::{
        expr::{
            call_new::{ExtractKind, ReEvalMode},
            AccessPropertyOpts, CallOpts, IdCtx, TypeOfMode,
        },
        types::NormalizeTypeOpts,
        util::ResultExt,
        Analyzer,
    },
    ty::TypeExt,
    util::RemoveTypes,
    validator,
    validator::ValidateWith,
    VResult,
};

#[derive(Debug, Clone, Copy, Default)]
pub(crate) struct GetIteratorOpts {
    /// Defaults to `false`.
    pub disallow_str: bool,
}

#[validator]
impl Analyzer<'_, '_> {
    fn validate(
        &mut self,
        arr: &RArrayLit,
        mode: TypeOfMode,
        type_args: Option<&TypeParamInstantiation>,
        type_ann: Option<&Type>,
    ) -> VResult<Type> {
        let marks = self.marks();

        let span = arr.span;
        let elems = &arr.elems;

        let type_ann = self.expand_type_ann(span, type_ann)?;

        let mut iterator = type_ann
            .as_deref()
            .and_then(|ty| self.get_iterator(span, Cow::Borrowed(ty), Default::default()).ok());
        iterator.freeze();

        let prefer_tuple = self.ctx.prefer_tuple_for_array_lit || self.prefer_tuple(type_ann.as_deref());
        let is_empty = elems.is_empty();
        let mut can_be_tuple = self.ctx.prefer_tuple_for_array_lit || !self.ctx.array_lit_cannot_be_tuple;
        let mut elements = Vec::with_capacity(elems.len());

        for (idx, elem) in elems.iter().enumerate() {
            let span = elem.span();
            let ty = match elem {
                Some(RExprOrSpread { spread: None, ref expr }) => {
                    let elem_type_ann = iterator
                        .as_deref()
                        .and_then(|iterator| self.get_element_from_iterator(span, Cow::Borrowed(iterator), idx).ok())
                        .freezed();

                    let ty = expr.validate_with_args(self, (mode, type_args, elem_type_ann.as_deref()))?;
                    match ty.normalize() {
                        Type::TypeLit(..) => {
                            if !prefer_tuple {
                                can_be_tuple = false;
                            }
                        }
                        Type::Function(..) => {
                            if type_ann.is_none() {
                                can_be_tuple = false;
                            }
                        }
                        _ => {}
                    }
                    ty
                }
                Some(RExprOrSpread {
                    spread: Some(spread),
                    expr,
                }) => {
                    let mut element_type = expr.validate_with_default(self)?;
                    element_type.normalize_mut();

                    // TODO(kdy1): PERF

                    match element_type {
                        Type::Array(array) => {
                            can_be_tuple = false;
                            elements.push(TupleElement {
                                span,
                                label: None,
                                ty: array.elem_type,
                                tracker: Default::default(),
                            });
                        }
                        Type::Tuple(tuple) => {
                            if !prefer_tuple {
                                can_be_tuple = false;
                            }
                            elements.extend(tuple.elems);
                        }
                        Type::Keyword(KeywordType {
                            kind: TsKeywordTypeKind::TsAnyKeyword,
                            ..
                        }) => {
                            can_be_tuple = false;
                            elements.push(TupleElement {
                                span,
                                label: None,
                                ty: Box::new(element_type.clone()),
                                tracker: Default::default(),
                            });
                        }
                        _ => {
                            let elem_type = self
                                .get_iterator_element_type(span, Cow::Owned(element_type), false, Default::default())
                                .context("tried to calculated the element type of a iterable provided to spread")?
                                .into_owned();

                            can_be_tuple = false;
                            elements.push(TupleElement {
                                span,
                                label: None,
                                ty: Box::new(elem_type),
                                tracker: Default::default(),
                            });
                        }
                    }
                    continue;
                }
                None => Type::undefined(span, Default::default()),
            };
            elements.push(TupleElement {
                span,
                label: None,
                ty: Box::new(ty),
                tracker: Default::default(),
            });
        }

        if self.ctx.in_export_default_expr && elements.is_empty() {
            return Ok(Type::Array(Array {
                span,
                elem_type: Box::new(Type::any(span, Default::default())),
                metadata: Default::default(),
                tracker: Default::default(),
            }));
        }

        if !can_be_tuple || (type_ann.is_none() && elements.is_empty()) {
            elements.retain(|el| {
                if el.ty.is_kwd(TsKeywordTypeKind::TsNullKeyword) || el.ty.is_kwd(TsKeywordTypeKind::TsUndefinedKeyword) {
                    return false;
                }
                true
            });
            let mut types: Vec<_> = elements
                .into_iter()
                .map(|element| *element.ty)
                .map(|ty| if type_ann.is_none() { ty.generalize_lit() } else { ty })
                .collect();
            types.dedup_type();
            if types.is_empty() {
                types.push(if self.ctx.use_undefined_for_empty_array_lit && is_empty {
                    Type::undefined(span, Default::default())
                } else {
                    let span = span.with_ctxt(SyntaxContext::empty());
                    let implicit = !elems.is_empty();

                    Type::any(
                        span,
                        KeywordTypeMetadata {
                            common: CommonTypeMetadata {
                                implicit,
                                ..Default::default()
                            },
                            ..Default::default()
                        },
                    )
                });
            }

            let mut ty = Type::Array(
                Array {
                    span,
                    elem_type: Box::new(Type::new_union(span, types)),
                    metadata: Default::default(),
                    tracker: Default::default(),
                }
                .fixed(),
            );
            self.normalize_union(&mut ty, false);

            return Ok(ty);
        }

        let should_be_any = elements
            .iter()
            .all(|el| el.ty.is_kwd(TsKeywordTypeKind::TsNullKeyword) || el.ty.is_kwd(TsKeywordTypeKind::TsUndefinedKeyword));

        if should_be_any && !self.ctx.prefer_tuple_for_array_lit {
            elements.iter_mut().for_each(|el| {
                let span = el.ty.span().with_ctxt(SyntaxContext::empty());
                el.ty = Box::new(Type::any(
                    span,
                    KeywordTypeMetadata {
                        common: CommonTypeMetadata {
                            implicit: true,
                            ..Default::default()
                        },
                        ..Default::default()
                    },
                ));
            });
        }

        Ok(Type::Tuple(Tuple {
            span,
            elems: elements,
            metadata: Default::default(),
            tracker: Default::default(),
        }))
    }
}

impl Analyzer<'_, '_> {
    /// Get `n`th element from the `iterator`.
    pub(crate) fn get_element_from_iterator<'a>(&self, span: Span, iterator: Cow<'a, Type>, n: usize) -> VResult<Cow<'a, Type>> {
        debug!("Calculating element type of an iterator ({})", dump_type_as_string(&iterator));

        if iterator.is_any() {
            return Ok(iterator);
        }
        if iterator.is_kwd(TsKeywordTypeKind::TsStringKeyword) {
            return Ok(Cow::Owned(Type::Keyword(KeywordType {
                span,
                kind: TsKeywordTypeKind::TsStringKeyword,
                metadata: Default::default(),
                tracker: Default::default(),
            })));
        }

        match iterator.normalize() {
            Type::Ref(..) => {
                let iterator = self
                    .expand_top_ref(span, iterator, Default::default())
                    .context("tried to expand iterator to get nth element")?;

                return self
                    .get_element_from_iterator(span, iterator, n)
                    .context("tried to get element from an expanded iterator");
            }

            Type::Union(u) => {
                let can_use_undefined = u.types.iter().all(|ty| ty.is_tuple());

                let mut types = vec![];
                let mut errors = vec![];
                for (idx, iterator_elem) in u.types.iter().enumerate() {
                    let res = self
                        .get_element_from_iterator(span, Cow::Borrowed(iterator_elem), n)
                        .with_context(|| format!("failed to get element type from {}th element", idx))
                        .convert_err(|err| match err {
                            ErrorKind::TupleIndexError { span, .. } => ErrorKind::TupleTooShort { span },
                            _ => err,
                        })
                        .map(Cow::into_owned);
                    match res {
                        Ok(ty) => {
                            ty.assert_valid();
                            types.push(ty);
                        }
                        Err(err) => {
                            errors.push(err);
                        }
                    }
                }

                if !errors.is_empty() {
                    if can_use_undefined && errors.len() != u.types.len() {
                        types.push(Type::Keyword(KeywordType {
                            span,
                            kind: TsKeywordTypeKind::TsUndefinedKeyword,
                            metadata: Default::default(),
                            tracker: Default::default(),
                        }));
                        types.dedup_type();
                        return Ok(Cow::Owned(Type::new_union(span, types)));
                    }

                    return Err(ErrorKind::NoSuchProperty {
                        span,
                        obj: Some(Box::new(iterator.into_owned())),
                        prop: None,
                    }
                    .into());
                }

                types.dedup_type();

                return Ok(Cow::Owned(Type::new_union(span, types)));
            }

            Type::Array(..) | Type::Tuple(..) => {
                return self
                    .access_property(
                        span,
                        &iterator,
                        &Key::Num(RNumber {
                            span,
                            value: n as _,

                            raw: None,
                        }),
                        TypeOfMode::RValue,
                        IdCtx::Var,
                        Default::default(),
                    )
                    .map(Cow::Owned)
                    .context("tried to access property of a type to calculate element type");
            }
            _ => {}
        }

        let next_ret_ty = self
            .call_property(
                span,
                ExtractKind::Call,
                Default::default(),
                &iterator,
                &iterator,
                &Key::Normal { span, sym: "next".into() },
                None,
                &[],
                &[],
                &[],
                None,
                CallOpts { ..Default::default() },
            )
            .convert_err(|err| match err {
                ErrorKind::NoCallablePropertyWithName { span, .. }
                | ErrorKind::NoSuchProperty { span, .. }
                | ErrorKind::NoSuchPropertyInClass { span, .. } => {
                    if let Type::Union(iterator) = iterator.normalize() {
                        if iterator.types.iter().all(|ty| ty.is_tuple()) {
                            return ErrorKind::NoSuchProperty {
                                span,
                                obj: None,
                                prop: None,
                            };
                        }
                    }
                    ErrorKind::MustHaveSymbolIteratorThatReturnsIterator { span }
                }
                _ => err,
            })
            .context("tried calling `next()` to get element type of nth element of an iterator")?;

        let value_ty = self.get_value_type_from_iterator_result(span, &next_ret_ty)?;

        Ok(Cow::Owned(value_ty))
    }

    fn try_next_method_of_iterator(&self, span: Span, iterator: &Type, awaited: bool) -> VResult<Type> {
        let _tracing = dev_span!("try_next_method_of_iterator");

        let mut item = self
            .call_property(
                span,
                ExtractKind::Call,
                ReEvalMode::NoReEval,
                iterator,
                iterator,
                &Key::Normal { span, sym: "next".into() },
                Default::default(),
                Default::default(),
                Default::default(),
                Default::default(),
                Default::default(),
                CallOpts { ..Default::default() },
            )
            .context("tried to get the type of `next` of an iterator")?;

        if awaited {
            item = self
                .get_awaited_type(span, Cow::Owned(item), false)
                .context("tried to unwrap `Promise` to calculate the element type of an async iterator")?
                .into_owned()
        }

        let elem_ty = self
            .get_value_type_from_iterator_result(span, &item)
            .context("tried to get element type of an async iterator")?;

        Ok(elem_ty)
    }

    pub(crate) fn get_async_iterator_element_type<'a>(
        &self,
        span: Span,
        ty: Cow<'a, Type>,
        try_next_method: bool,
    ) -> VResult<Cow<'a, Type>> {
        let ty = self
            .normalize(Some(span), ty, Default::default())
            .context("tried to normalize type to calculate element type of an async iterator")?;

        if ty.is_any() {
            return Ok(ty);
        }

        if !self.data.checked_for_async_iterator {
            self.data.checked_for_async_iterator = true;
            self.env.get_global_type(span, &"AsyncIterator".into()).report(&self.storage);
        }

        if let Ok(item) = self.try_next_method_of_iterator(span, &ty, true) {
            return Ok(Cow::Owned(item));
        }

        let async_iterator = self
            .call_property(
                span,
                ExtractKind::Call,
                Default::default(),
                &ty,
                &ty,
                &Key::Computed(ComputedKey {
                    span,
                    expr: Box::new(RExpr::Invalid(RInvalid { span })),
                    ty: Box::new(Type::Symbol(Symbol {
                        span,
                        id: SymbolId::async_iterator(),
                        metadata: Default::default(),
                        tracker: Default::default(),
                    })),
                }),
                None,
                &[],
                &[],
                &[],
                None,
                CallOpts {
                    disallow_optional_object_property: true,
                    do_not_use_any_for_computed_key: true,
                    ..Default::default()
                },
            )
            .map(Cow::Owned);

        if let Ok(async_iterator) = async_iterator {
            return Ok(Cow::Owned(self.try_next_method_of_iterator(span, &async_iterator, true)?));
        }

        let elem_ty = self
            .get_iterator_element_type(span, ty, true, Default::default())
            .context("tried to get element of iterator as a fallback logic for async iterator")
            .convert_err(|err| match err {
                ErrorKind::MustHaveSymbolIteratorThatReturnsIterator { span } => {
                    ErrorKind::MustHaveSymbolAsyncIteratorThatReturnsIterator { span }
                }
                _ => err,
            })?;

        if let Ok(elem_ty) = self.get_awaited_type(span, Cow::Borrowed(&elem_ty), false).map(Cow::into_owned) {
            return Ok(Cow::Owned(elem_ty));
        }

        Ok(Cow::Owned(elem_ty.into_owned()))
    }

    pub(crate) fn get_value_type_from_iterator_result(&self, span: Span, iterator_result: &Type) -> VResult<Type> {
        let iterator_result = self.normalize(
            Some(span),
            Cow::Borrowed(iterator_result),
            NormalizeTypeOpts {
                preserve_global_this: true,
                preserve_union: true,
                ..Default::default()
            },
        )?;

        if let Type::Union(u) = iterator_result.normalize() {
            let mut types = vec![];

            for ty in &u.types {
                let done_ty = self.access_property(
                    span,
                    ty,
                    &Key::Normal { span, sym: "done".into() },
                    TypeOfMode::RValue,
                    IdCtx::Var,
                    AccessPropertyOpts {
                        disallow_indexing_array_with_string: true,
                        disallow_creating_indexed_type_from_ty_els: true,
                        ..Default::default()
                    },
                );

                if let Ok(done_ty) = done_ty {
                    if done_ty.remove_truthy().is_never() {
                        continue;
                    }
                }

                types.push(ty.clone());
            }

            if types.len() == 1 {
                return self.get_value_type_from_iterator_result(span, &types[0]);
            }
        }

        let mut elem_ty = self
            .access_property(
                span,
                &iterator_result,
                &Key::Normal { span, sym: "value".into() },
                TypeOfMode::RValue,
                IdCtx::Var,
                AccessPropertyOpts {
                    disallow_indexing_array_with_string: true,
                    disallow_creating_indexed_type_from_ty_els: true,
                    ..Default::default()
                },
            )
            .context("tried to get the type of property named `value` to determine the type of an iterator")
            .convert_err(|err| ErrorKind::NextOfIteratorShouldReturnTypeWithPropertyValue { span: err.span() })?;

        elem_ty.metadata_mut().implicit = false;

        elem_ty.fix();

        Ok(elem_ty)
    }

    pub(crate) fn get_rest_elements<'a>(&self, span: Option<Span>, iterator: Cow<'a, Type>, start_index: usize) -> VResult<Cow<'a, Type>> {
        let mut iterator = self.normalize(span, iterator, NormalizeTypeOpts { ..Default::default() })?;

        if iterator.is_tuple() {
            iterator.freeze();
            let ty = iterator.into_owned().expect_tuple();

            // TODO: Handle [Type::Rest]

            return Ok(Cow::Owned(Type::Tuple(Tuple {
                elems: ty.elems.into_iter().skip(start_index).collect(),
                ..ty
            })));
        }

        match iterator.normalize() {
            // TODO
            Type::TypeLit(_) => {}

            // TODO
            Type::Union(_) => {}
            // TODO
            Type::Intersection(_) => {}
            // TODO
            Type::Rest(_) => {}

            _ => {}
        }

        Ok(Cow::Owned(iterator.into_owned()))
    }

    pub(crate) fn get_iterator<'a>(&self, span: Span, ty: Cow<'a, Type>, opts: GetIteratorOpts) -> VResult<Cow<'a, Type>> {
        let start = Instant::now();
        let iterator = self.get_iterator_inner(span, ty, opts).context("tried to get iterator");

        let end = Instant::now();

        debug!(kind = "perf", op = "get_iterator", "get_iterator (time = {:?}", end - start);

        let iterator = iterator?;

        if let Type::Class(..) = iterator.normalize() {
            if let Ok(return_prop_ty) = self.access_property(
                span,
                &iterator,
                &Key::Normal {
                    span,
                    sym: js_word!("return"),
                },
                TypeOfMode::RValue,
                IdCtx::Var,
                Default::default(),
            ) {
                if !return_prop_ty.is_fn_type() {
                    self.storage.report(ErrorKind::ReturnPropertyOfIteratorMustBeMethod { span }.into())
                }
            }
        }

        Ok(iterator)
    }

    fn get_iterator_inner<'a>(&self, span: Span, ty: Cow<'a, Type>, opts: GetIteratorOpts) -> VResult<Cow<'a, Type>> {
        let ty_str = force_dump_type_as_string(&ty);
        debug!("[exprs/array] get_iterator({})", ty_str);
        ty.assert_valid();

        let mut ty = self
            .normalize(
                Some(span),
                ty,
                NormalizeTypeOpts {
                    preserve_global_this: true,
                    preserve_intersection: true,
                    preserve_union: true,
                    ..Default::default()
                },
            )
            .context("tried to normalize type to get iterator")?;
        ty.freeze();

        let res: VResult<_> = (|| {
            if ty.is_str() {
                if !opts.disallow_str {
                    return Ok(ty);
                } else {
                    return Err(ErrorKind::NotArrayType { span }.into());
                }
            }

            match ty.normalize() {
                Type::Keyword(KeywordType {
                    kind: TsKeywordTypeKind::TsNumberKeyword,
                    ..
                })
                | Type::Keyword(KeywordType {
                    kind: TsKeywordTypeKind::TsBigIntKeyword,
                    ..
                })
                | Type::Keyword(KeywordType {
                    kind: TsKeywordTypeKind::TsBooleanKeyword,
                    ..
                })
                | Type::Lit(LitType {
                    lit: RTsLit::Number(..), ..
                })
                | Type::Lit(LitType {
                    lit: RTsLit::BigInt(..), ..
                })
                | Type::Lit(LitType { lit: RTsLit::Bool(..), .. }) => return Err(ErrorKind::NotArrayType { span }.into()),

                Type::Array(..) | Type::Tuple(..) => return Ok(ty),
                Type::Param(TypeParam {
                    constraint: Some(constraint),
                    ..
                }) => {
                    return self
                        .get_iterator(span, Cow::Borrowed(constraint), opts)
                        .map(Cow::into_owned)
                        .map(Cow::Owned)
                        .context("tried to get iterator from type parameter constraint");
                }
                Type::Union(u) => {
                    let types = u
                        .types
                        .iter()
                        .map(|v| self.get_iterator(span, Cow::Borrowed(v), opts))
                        .map(|res| res.map(Cow::into_owned))
                        .collect::<Result<_, _>>()
                        .convert_err(|err| match err {
                            ErrorKind::MustHaveSymbolIteratorThatReturnsIterator { span } => {
                                ErrorKind::MustHaveSymbolIteratorThatReturnsIteratorOrMustBeArray { span }
                            }
                            _ => err,
                        })?;
                    let new = Type::Union(Union {
                        span: u.span,
                        types,
                        metadata: u.metadata,
                        tracker: Default::default(),
                    });
                    return Ok(Cow::Owned(new));
                }
                Type::Intersection(i) => {
                    let types = i
                        .types
                        .iter()
                        .map(|v| self.get_iterator(v.span(), Cow::Borrowed(v), opts))
                        .map(|res| res.map(Cow::into_owned))
                        .collect::<Result<_, _>>()?;
                    let new = Type::Intersection(Intersection {
                        span: i.span,
                        types,
                        metadata: i.metadata,
                        tracker: Default::default(),
                    });
                    return Ok(Cow::Owned(new));
                }
                _ => {}
            }

            self.call_property(
                span,
                ExtractKind::Call,
                Default::default(),
                &ty,
                &ty,
                &Key::Computed(ComputedKey {
                    span,
                    expr: Box::new(RExpr::Invalid(RInvalid { span })),
                    ty: Box::new(Type::Symbol(Symbol {
                        span,
                        id: SymbolId::iterator(),
                        metadata: Default::default(),
                        tracker: Default::default(),
                    })),
                }),
                None,
                &[],
                &[],
                &[],
                None,
                CallOpts {
                    disallow_optional_object_property: true,
                    do_not_use_any_for_computed_key: true,
                    ..Default::default()
                },
            )
            .convert_err(|err| match err {
                ErrorKind::NoCallablePropertyWithName { span, .. }
                | ErrorKind::NoSuchPropertyInClass { span, .. }
                | ErrorKind::NoSuchProperty { span, .. } => ErrorKind::MustHaveSymbolIteratorThatReturnsIterator { span },
                _ => err,
            })
            .map(Cow::Owned)
            .context("tried to call `[Symbol.iterator]()`")
        })();

        res.with_context(|| format!("tried to convert a type ({}) to an iterator", ty_str))
    }

    /// # Parameters
    ///
    /// ## try_next_value
    ///
    /// If it's true, this method will try `ty.next().value`.
    pub(crate) fn get_iterator_element_type<'a>(
        &self,
        span: Span,
        ty: Cow<'a, Type>,
        try_next_value: bool,
        opts: GetIteratorOpts,
    ) -> VResult<Cow<'a, Type>> {
        let ty_str = dump_type_as_string(&ty);

        if try_next_value {
            if let Ok(ty) = self.get_next_value_type_of_iterator(span, Cow::Borrowed(&ty)) {
                return Ok(Cow::Owned(ty));
            }
        }

        let mut iterator = self
            .get_iterator(span, ty, opts)
            .with_context(|| format!("tried to get a type of an iterator to get the element type of it ({})", ty_str))?;
        iterator.freeze();

        if iterator.is_str() {
            return Ok(Cow::Owned(Type::Keyword(KeywordType {
                span: iterator.span(),
                kind: TsKeywordTypeKind::TsStringKeyword,
                metadata: Default::default(),
                tracker: Default::default(),
            })));
        }

        match iterator.normalize() {
            Type::Array(arr) => return Ok(Cow::Owned(*arr.elem_type.clone())),
            Type::Tuple(tuple) => {
                if tuple.elems.is_empty() {
                    return Ok(Cow::Owned(Type::any(
                        tuple.span,
                        KeywordTypeMetadata {
                            common: tuple.metadata.common,
                        },
                    )));
                }
                let types = tuple.elems.iter().map(|e| *e.ty.clone()).collect_vec();
                return Ok(Cow::Owned(
                    Type::Union(Union {
                        span: tuple.span,
                        types,
                        metadata: UnionMetadata {
                            common: tuple.metadata.common,
                        },
                        tracker: Default::default(),
                    })
                    .fixed(),
                ));
            }
            Type::Union(u) => {
                let types = u
                    .types
                    .iter()
                    .map(|iterator| {
                        self.get_iterator_element_type(
                            iterator.span().or_else(|| span),
                            Cow::Borrowed(iterator),
                            try_next_value,
                            Default::default(),
                        )
                    })
                    .map(|ty| ty.map(Cow::into_owned))
                    .collect::<Result<Vec<_>, _>>()?;

                return Ok(Cow::Owned(
                    Type::Union(Union {
                        span: u.span,
                        types,
                        metadata: u.metadata,
                        tracker: Default::default(),
                    })
                    .fixed(),
                ));
            }

            Type::Intersection(i) => {
                let mut types = i
                    .types
                    .iter()
                    .map(|iterator| {
                        self.get_iterator_element_type(iterator.span(), Cow::Borrowed(iterator), try_next_value, Default::default())
                    })
                    .map(|ty| ty.map(Cow::into_owned))
                    .collect::<Result<Vec<_>, _>>()?;
                types.dedup_type();

                return Ok(Cow::Owned(Type::Intersection(Intersection {
                    span: i.span,
                    types,
                    metadata: i.metadata,
                    tracker: Default::default(),
                })));
            }

            _ => {}
        }

        let elem_ty = self.get_next_value_type_of_iterator(span, iterator)?;

        Ok(Cow::Owned(elem_ty))
    }

    /// Returns the type of `iterator.next().value`.
    fn get_next_value_type_of_iterator(&self, span: Span, iterator: Cow<Type>) -> VResult<Type> {
        let next_ret_ty = self
            .call_property(
                span,
                ExtractKind::Call,
                Default::default(),
                &iterator,
                &iterator,
                &Key::Normal { span, sym: "next".into() },
                None,
                &[],
                &[],
                &[],
                None,
                Default::default(),
            )
            .convert_err(|err| match err {
                ErrorKind::NoCallablePropertyWithName { span, .. } => {
                    if self.env.target() <= EsVersion::Es2015 {
                        ErrorKind::MustBeArray { span }
                    } else {
                        ErrorKind::NoMethodNamedNext { span }
                    }
                }
                _ => err,
            })
            .context("tried calling `next()` to get element type of iterator")?;

        let elem_ty = self
            .get_value_type_from_iterator_result(span, &next_ret_ty)
            .context("tried to get type from `IteratorResult<T>`")?;

        Ok(elem_ty)
    }

    pub(crate) fn calculate_tuple_element_count(&self, span: Span, ty: &Type) -> VResult<Option<usize>> {
        let ty = self.normalize(
            Some(span),
            Cow::Borrowed(ty),
            NormalizeTypeOpts {
                preserve_global_this: true,
                ..Default::default()
            },
        )?;

        match ty.normalize() {
            Type::Rest(rest) => match ty.normalize() {
                Type::Tuple(tuple) => {
                    let mut sum = 0;
                    for elem in tuple.elems.iter() {
                        if let Some(v) = self.calculate_tuple_element_count(elem.span(), &elem.ty)? {
                            sum += v;
                        }
                    }
                    Ok(Some(sum))
                }
                Type::Union(u) => {
                    let val = self.calculate_tuple_element_count(span, &u.types[0])?;
                    let val = match val {
                        Some(v) => v,
                        None => return Ok(None),
                    };
                    for ty in u.types.iter() {
                        if let Some(v) = self.calculate_tuple_element_count(ty.span(), ty)? {
                            if v != val {
                                return Ok(None);
                            }
                        }
                    }
                    Ok(Some(val))
                }
                _ => Ok(None),
            },
            _ => Ok(Some(1)),
        }
    }
}
