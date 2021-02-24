use super::call_new::ExtractKind;
use super::IdCtx;
use super::TypeOfMode;
use crate::analyzer::Analyzer;
use crate::ty::type_facts::TypeFactsHandler;
use crate::type_facts::TypeFacts;
use crate::util::type_ext::TypeVecExt;
use crate::validator;
use crate::validator::ValidateWith;
use crate::ValidationResult;
use rnode::FoldWith;
use rnode::NodeId;
use stc_ts_ast_rnode::RArrayLit;
use stc_ts_ast_rnode::RExpr;
use stc_ts_ast_rnode::RExprOrSpread;
use stc_ts_ast_rnode::RExprOrSuper;
use stc_ts_ast_rnode::RIdent;
use stc_ts_ast_rnode::RMemberExpr;
use stc_ts_ast_rnode::RTsKeywordType;
use stc_ts_errors::DebugExt;
use stc_ts_errors::Error;
use stc_ts_types::Array;
use stc_ts_types::ComputedKey;
use stc_ts_types::Key;
use stc_ts_types::Tuple;
use stc_ts_types::TupleElement;
use stc_ts_types::Type;
use stc_ts_types::TypeParamInstantiation;
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
                        Type::TypeLit(..) => {
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
            self.normalize_union_of_objects(&mut ty, false);

            return Ok(ty);
        }

        return Ok(Type::Tuple(Tuple { span, elems: elements }));
    }
}

impl Analyzer<'_, '_> {
    pub(crate) fn get_iterator_element_type<'a>(
        &mut self,
        span: Span,
        ty: Cow<'a, Type>,
    ) -> ValidationResult<Cow<'a, Type>> {
        match ty.normalize() {
            // TODO
            Type::Array(..) | Type::Tuple(..) => return Ok(ty),
            _ => {}
        }

        let iterator = self
            .call_property(
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
            .map_err(|err| {
                err.convert(|err| match err {
                    Error::NoCallabelPropertyWithName { span, .. } => {
                        Error::MustHaveSymbolIteratorThatReturnsIterator { span }
                    }
                    _ => err,
                })
            })
            .context("tried to call `[Symbol.iterator]()` to convert a type to an iterator")?;

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

        let elem_ty = self
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

        let elem_ty = elem_ty.fold_with(&mut TypeFactsHandler {
            facts: TypeFacts::Truthy,
            analyzer: self,
        });

        Ok(Cow::Owned(elem_ty))
    }
}
