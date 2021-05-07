use super::call_new::ExtractKind;
use super::IdCtx;
use super::TypeOfMode;
use crate::analyzer::types::NormalizeTypeOpts;
use crate::analyzer::Analyzer;
use crate::type_facts::TypeFacts;
use crate::util::type_ext::TypeVecExt;
use crate::validator;
use crate::validator::ValidateWith;
use crate::ValidationResult;
use itertools::Itertools;
use rnode::NodeId;
use stc_ts_ast_rnode::RArrayLit;
use stc_ts_ast_rnode::RExpr;
use stc_ts_ast_rnode::RExprOrSpread;
use stc_ts_ast_rnode::RExprOrSuper;
use stc_ts_ast_rnode::RIdent;
use stc_ts_ast_rnode::RMemberExpr;
use stc_ts_ast_rnode::RNumber;
use stc_ts_ast_rnode::RTsKeywordType;
use stc_ts_ast_rnode::RTsLit;
use stc_ts_ast_rnode::RTsLitType;
use stc_ts_errors::debug::dump_type_as_string;
use stc_ts_errors::DebugExt;
use stc_ts_errors::Error;
use stc_ts_file_analyzer_macros::context;
use stc_ts_type_ops::Fix;
use stc_ts_types::Array;
use stc_ts_types::ComputedKey;
use stc_ts_types::Intersection;
use stc_ts_types::Key;
use stc_ts_types::Tuple;
use stc_ts_types::TupleElement;
use stc_ts_types::Type;
use stc_ts_types::TypeParamInstantiation;
use stc_ts_types::Union;
use std::borrow::Cow;
use swc_common::Span;
use swc_common::Spanned;
use swc_common::SyntaxContext;
use swc_ecma_ast::TsKeywordTypeKind;

#[validator]
impl Analyzer<'_, '_> {
    fn validate(
        &mut self,
        arr: &RArrayLit,
        mode: TypeOfMode,
        type_args: Option<&TypeParamInstantiation>,
        type_ann: Option<&Type>,
    ) -> ValidationResult {
        let span = arr.span;
        let elems = &arr.elems;

        let type_ann = self.expand_type_ann(type_ann)?;

        let iterator = type_ann
            .as_deref()
            .and_then(|ty| self.get_iterator(span, Cow::Borrowed(ty)).ok());

        let prefer_tuple = self.prefer_tuple(type_ann.as_deref());
        let mut can_be_tuple = !self.ctx.cannot_be_tuple;
        let mut elements = Vec::with_capacity(elems.len());

        for (idx, elem) in elems.iter().enumerate() {
            let span = elem.span();
            let ty = match elem {
                Some(RExprOrSpread { spread: None, ref expr }) => {
                    let elem_type_ann = iterator
                        .as_deref()
                        .and_then(|iterator| self.get_element_from_iterator(span, Cow::Borrowed(iterator), idx).ok());

                    let ty = expr.validate_with_args(self, (mode, type_args, elem_type_ann.as_deref()))?;
                    match ty.normalize() {
                        Type::TypeLit(..) | Type::Function(..) => {
                            can_be_tuple = false;
                        }
                        _ => {}
                    }
                    ty
                }
                Some(RExprOrSpread {
                    spread: Some(spread),
                    expr,
                }) => {
                    let element_type = expr.validate_with_default(self)?;
                    let element_type = element_type.foldable();

                    match element_type {
                        Type::Array(array) => {
                            can_be_tuple = false;
                            elements.push(TupleElement {
                                span,
                                label: None,
                                ty: array.elem_type,
                            });
                        }
                        Type::Tuple(tuple) => {
                            if !prefer_tuple {
                                can_be_tuple = false;
                            }
                            elements.extend(tuple.elems);
                        }
                        Type::Keyword(RTsKeywordType {
                            kind: TsKeywordTypeKind::TsAnyKeyword,
                            ..
                        }) => {
                            can_be_tuple = false;
                            elements.push(TupleElement {
                                span,
                                label: None,
                                ty: box element_type.clone(),
                            });
                        }
                        _ => {
                            let elem_type = self
                                .get_iterator_element_type(span, Cow::Owned(element_type), false)
                                .context("tried to calculated the element type of a iterable provided to spread")?
                                .into_owned();

                            can_be_tuple = false;
                            elements.push(TupleElement {
                                span,
                                label: None,
                                ty: box elem_type,
                            });
                        }
                    }
                    continue;
                }
                None => {
                    let ty = Type::undefined(span);
                    ty
                }
            };
            elements.push(TupleElement {
                span,
                label: None,
                ty: box ty,
            });
        }

        if self.ctx.in_export_default_expr && elements.is_empty() {
            return Ok(Type::Array(Array {
                span,
                elem_type: box Type::any(span),
            }));
        }

        if !can_be_tuple || (type_ann.is_none() && elements.is_empty()) {
            elements.retain(|el| {
                if el.ty.is_kwd(TsKeywordTypeKind::TsNullKeyword) || el.ty.is_kwd(TsKeywordTypeKind::TsUndefinedKeyword)
                {
                    return false;
                }
                true
            });
            let mut types: Vec<_> = elements.into_iter().map(|element| *element.ty).collect();
            types.dedup_type();
            if types.is_empty() {
                types.push(Type::any(span));
            }

            let mut ty = Type::Array(
                Array {
                    span,
                    elem_type: box Type::union(types),
                }
                .fixed(),
            );
            self.normalize_union(&mut ty, false);

            return Ok(ty);
        }

        let should_be_any = elements.iter().all(|el| {
            el.ty.is_kwd(TsKeywordTypeKind::TsNullKeyword) || el.ty.is_kwd(TsKeywordTypeKind::TsUndefinedKeyword)
        });

        if should_be_any {
            elements.iter_mut().for_each(|el| {
                el.ty = box Type::any(el.ty.span());
            });
        }

        return Ok(Type::Tuple(Tuple { span, elems: elements }));
    }
}

impl Analyzer<'_, '_> {
    /// Get `n`th element from the `iterator`.
    pub(crate) fn get_element_from_iterator<'a>(
        &mut self,
        span: Span,
        iterator: Cow<'a, Type>,
        n: usize,
    ) -> ValidationResult<Cow<'a, Type>> {
        slog::debug!(
            self.logger,
            "Caculating element type of an iterator ({})",
            dump_type_as_string(&self.cm, &iterator)
        );

        if iterator.is_any() {
            return Ok(iterator);
        }
        if iterator.is_kwd(TsKeywordTypeKind::TsStringKeyword) {
            return Ok(Cow::Owned(Type::Keyword(RTsKeywordType {
                span,
                kind: TsKeywordTypeKind::TsStringKeyword,
            })));
        }

        match iterator.normalize() {
            Type::Ref(..) => {
                let iterator = self
                    .expand_top_ref(span, iterator)
                    .context("tried to expand iterator to get nth element")?;

                return self
                    .get_element_from_iterator(span, iterator, n)
                    .context("tried to get element from an expanded iterator");
            }
            Type::Array(..) | Type::Tuple(..) => {
                return self
                    .access_property(
                        span,
                        iterator.clone().into_owned(),
                        &Key::Num(RNumber { span, value: n as _ }),
                        TypeOfMode::RValue,
                        IdCtx::Var,
                    )
                    .map(Cow::Owned)
                    .context("tried to access property of a type to calculate element type")
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
                &Key::Normal {
                    span,
                    sym: "next".into(),
                },
                None,
                &[],
                &[],
                &[],
                None,
            )
            .convert_err(|err| match err {
                Error::NoCallabelPropertyWithName { span, .. }
                | Error::NoSuchProperty { span, .. }
                | Error::NoSuchPropertyInClass { span, .. } => {
                    Error::MustHaveSymbolIteratorThatReturnsIterator { span }
                }
                _ => err,
            })
            .context("tried calling `next()` to get element type of nth element of an iterator")?;

        let mut elem_ty = self
            .access_property(
                span,
                next_ret_ty,
                &Key::Normal {
                    span,
                    sym: "value".into(),
                },
                TypeOfMode::RValue,
                IdCtx::Var,
            )
            .context(
                "tried to get the type of property named `value` to determine the type of nth element of an iterator",
            )?;

        // TODO: Remove `done: true` instead of removing `any` from value.
        match elem_ty.normalize_mut() {
            Type::Union(u) => {
                u.types.retain(|ty| !ty.is_any());
                if u.types.is_empty() {
                    u.types = vec![Type::any(u.span)]
                }
            }
            _ => {}
        }

        elem_ty = self.apply_type_facts_to_type(TypeFacts::Truthy, elem_ty);

        Ok(Cow::Owned(elem_ty))
    }

    pub(crate) fn get_lefting_elements<'a>(
        &mut self,
        span: Option<Span>,
        iterator: Cow<'a, Type>,
        start_index: usize,
    ) -> ValidationResult<Cow<'a, Type>> {
        let iterator = self.normalize(span, &iterator, NormalizeTypeOpts { ..Default::default() })?;

        if iterator.is_tuple() {
            let ty = iterator.into_owned().foldable().tuple().unwrap();

            return Ok(Cow::Owned(Type::Tuple(Tuple {
                span: ty.span,
                elems: ty.elems.into_iter().skip(start_index).collect(),
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

    pub(crate) fn get_iterator<'a>(&mut self, span: Span, ty: Cow<'a, Type>) -> ValidationResult<Cow<'a, Type>> {
        let ty_str = dump_type_as_string(&self.cm, &ty);
        slog::debug!(self.logger, "[exprs/array] get_iterator({})", ty_str);
        ty.assert_valid();

        let res: ValidationResult<_> = (|| {
            if ty.is_str() {
                return Ok(ty);
            }

            match ty.normalize() {
                Type::Ref(..) => {
                    let ty = self.expand_top_ref(span, ty)?;
                    return self.get_iterator(span, ty);
                }

                Type::Keyword(RTsKeywordType {
                    kind: TsKeywordTypeKind::TsNumberKeyword,
                    ..
                })
                | Type::Keyword(RTsKeywordType {
                    kind: TsKeywordTypeKind::TsBigIntKeyword,
                    ..
                })
                | Type::Keyword(RTsKeywordType {
                    kind: TsKeywordTypeKind::TsBooleanKeyword,
                    ..
                })
                | Type::Lit(RTsLitType {
                    lit: RTsLit::Number(..),
                    ..
                })
                | Type::Lit(RTsLitType {
                    lit: RTsLit::BigInt(..),
                    ..
                })
                | Type::Lit(RTsLitType {
                    lit: RTsLit::Bool(..), ..
                }) => return Err(Error::NotArrayType { span }),

                Type::Array(..) | Type::Tuple(..) => return Ok(ty),
                Type::Union(u) => {
                    let types = u
                        .types
                        .iter()
                        .map(|v| self.get_iterator(span, Cow::Borrowed(v)))
                        .map(|res| res.map(Cow::into_owned))
                        .collect::<Result<_, _>>()
                        .convert_err(|err| match err {
                            Error::MustHaveSymbolIteratorThatReturnsIterator { span } => {
                                Error::MustHaveSymbolIteratorThatReturnsIteratorOrMustBeArray { span }
                            }
                            _ => err,
                        })?;
                    let new = Type::Union(Union { span: u.span, types });
                    return Ok(Cow::Owned(new));
                }
                Type::Intersection(i) => {
                    let types = i
                        .types
                        .iter()
                        .map(|v| self.get_iterator(v.span(), Cow::Borrowed(v)))
                        .map(|res| res.map(Cow::into_owned))
                        .collect::<Result<_, _>>()?;
                    let new = Type::Intersection(Intersection { span: i.span, types });
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
                    expr: box RExpr::Member(RMemberExpr {
                        node_id: NodeId::invalid(),
                        span,
                        obj: RExprOrSuper::Expr(box RExpr::Ident(RIdent::new(
                            "Symbol".into(),
                            span.with_ctxt(SyntaxContext::empty()),
                        ))),
                        computed: false,
                        prop: box RExpr::Ident(RIdent::new("iterator".into(), span.with_ctxt(SyntaxContext::empty()))),
                    }),
                    ty: box Type::Keyword(RTsKeywordType {
                        span,
                        kind: TsKeywordTypeKind::TsSymbolKeyword,
                    }),
                }),
                None,
                &[],
                &[],
                &[],
                None,
            )
            .convert_err(|err| match err {
                Error::NoCallabelPropertyWithName { span, .. }
                | Error::NoSuchPropertyInClass { span, .. }
                | Error::NoSuchProperty { span, .. } => Error::MustHaveSymbolIteratorThatReturnsIterator { span },
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
        &mut self,
        span: Span,
        ty: Cow<'a, Type>,
        try_next_value: bool,
    ) -> ValidationResult<Cow<'a, Type>> {
        let ty_str = dump_type_as_string(&self.cm, &ty);

        if try_next_value {
            if let Ok(ty) = self.get_next_value_type_of_iterator(span, Cow::Borrowed(&ty)) {
                return Ok(Cow::Owned(ty));
            }
        }

        let iterator = self.get_iterator(span, ty).with_context(|| {
            format!(
                "tried to get a type of an iterator to get the element type of it ({})",
                ty_str
            )
        })?;

        if iterator.is_str() {
            return Ok(Cow::Owned(Type::Keyword(RTsKeywordType {
                span: iterator.span(),
                kind: TsKeywordTypeKind::TsStringKeyword,
            })));
        }

        match iterator.normalize() {
            Type::Array(arr) => return Ok(Cow::Owned(*arr.elem_type.clone())),
            Type::Tuple(tuple) => {
                if tuple.elems.is_empty() {
                    return Ok(Cow::Owned(Type::any(tuple.span)));
                }
                let mut types = tuple.elems.iter().map(|e| *e.ty.clone()).collect_vec();
                types.dedup_type();
                return Ok(Cow::Owned(Type::union(types)));
            }
            Type::Union(u) => {
                let mut types = u
                    .types
                    .iter()
                    .map(|iterator| {
                        self.get_iterator_element_type(iterator.span(), Cow::Borrowed(iterator), try_next_value)
                    })
                    .map(|ty| ty.map(Cow::into_owned))
                    .collect::<Result<Vec<_>, _>>()?;
                types.dedup_type();

                return Ok(Cow::Owned(Type::Union(Union { span: u.span, types })));
            }

            Type::Intersection(i) => {
                let mut types = i
                    .types
                    .iter()
                    .map(|iterator| {
                        self.get_iterator_element_type(iterator.span(), Cow::Borrowed(iterator), try_next_value)
                    })
                    .map(|ty| ty.map(Cow::into_owned))
                    .collect::<Result<Vec<_>, _>>()?;
                types.dedup_type();

                return Ok(Cow::Owned(Type::Intersection(Intersection { span: i.span, types })));
            }

            _ => {}
        }

        let elem_ty = self.get_next_value_type_of_iterator(span, iterator)?;

        Ok(Cow::Owned(elem_ty))
    }

    /// Returns the type of `iterator.next().value`.
    #[context("tried to get type of `type.next().value`")]
    fn get_next_value_type_of_iterator(&mut self, span: Span, iterator: Cow<Type>) -> ValidationResult<Type> {
        let next_ret_ty = self
            .call_property(
                span,
                ExtractKind::Call,
                Default::default(),
                &iterator,
                &iterator,
                &Key::Normal {
                    span,
                    sym: "next".into(),
                },
                None,
                &[],
                &[],
                &[],
                None,
            )
            .context("tried calling `next()` to get element type of iterator")?;

        let mut elem_ty = self
            .access_property(
                span,
                next_ret_ty,
                &Key::Normal {
                    span,
                    sym: "value".into(),
                },
                TypeOfMode::RValue,
                IdCtx::Var,
            )
            .context("tried to get the type of property named `value` to determine the type of an iterator")?;

        // TODO: Remove `done: true` instead of removing `any` from value.
        match elem_ty.normalize_mut() {
            Type::Union(u) => {
                u.types.retain(|ty| !ty.is_any());
                if u.types.is_empty() {
                    u.types = vec![Type::any(u.span)]
                }
            }
            _ => {}
        }

        elem_ty = self.apply_type_facts_to_type(TypeFacts::Truthy, elem_ty);

        elem_ty
    }
}
