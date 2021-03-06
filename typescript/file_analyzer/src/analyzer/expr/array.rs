use super::call_new::ExtractKind;
use super::IdCtx;
use super::TypeOfMode;
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
use stc_ts_errors::DebugExt;
use stc_ts_errors::Error;
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

        let prefer_tuple = self.prefer_tuple(type_ann);
        let mut can_be_tuple = true;
        let mut elements = Vec::with_capacity(elems.len());

        for elem in elems.iter() {
            let span = elem.span();
            let ty = match elem {
                Some(RExprOrSpread { spread: None, ref expr }) => {
                    let ty = expr.validate_with_default(self)?;
                    match &ty {
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
                                .get_iterator_element_type(span, Cow::Owned(element_type))
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

        if !can_be_tuple {
            let mut types: Vec<_> = elements.into_iter().map(|element| *element.ty).collect();
            types.dedup_type();

            let mut ty = Type::Array(Array {
                span,
                elem_type: box Type::union(types),
            });
            self.normalize_union(&mut ty, false);

            return Ok(ty);
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
    pub(crate) fn get_iterator<'a>(&mut self, span: Span, ty: Cow<'a, Type>) -> ValidationResult<Cow<'a, Type>> {
        if ty.is_str() {
            return Ok(ty);
        }

        match ty.normalize() {
            Type::Ref(..) => {
                let ty = self.expand_top_ref(span, ty)?;
                return self.get_iterator(span, ty);
            }
            Type::Array(..) | Type::Tuple(..) => return Ok(ty),
            Type::Union(u) => {
                let types = u
                    .types
                    .iter()
                    .map(|v| self.get_iterator(v.span(), Cow::Borrowed(v)))
                    .map(|res| res.map(Cow::into_owned))
                    .collect::<Result<_, _>>()?;
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
        .context("tried to call `[Symbol.iterator]()` to convert a type to an iterator")
    }
    pub(crate) fn get_iterator_element_type<'a>(
        &mut self,
        span: Span,
        ty: Cow<'a, Type>,
    ) -> ValidationResult<Cow<'a, Type>> {
        let iterator = self
            .get_iterator(span, ty)
            .context("tried to get a type of an iterator to get the element type of it")?;

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
                    .map(|iterator| self.get_iterator_element_type(iterator.span(), Cow::Borrowed(iterator)))
                    .map(|ty| ty.map(Cow::into_owned))
                    .collect::<Result<Vec<_>, _>>()?;
                types.dedup_type();

                return Ok(Cow::Owned(Type::Union(Union { span: u.span, types })));
            }

            Type::Intersection(i) => {
                let mut types = i
                    .types
                    .iter()
                    .map(|iterator| self.get_iterator_element_type(iterator.span(), Cow::Borrowed(iterator)))
                    .map(|ty| ty.map(Cow::into_owned))
                    .collect::<Result<Vec<_>, _>>()?;
                types.dedup_type();

                return Ok(Cow::Owned(Type::Intersection(Intersection { span: i.span, types })));
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

        Ok(Cow::Owned(elem_ty))
    }
}
