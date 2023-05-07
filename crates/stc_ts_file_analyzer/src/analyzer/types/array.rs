use std::borrow::Cow;

use stc_ts_ast_rnode::RPat;
use stc_ts_types::{FnParam, RestType, TupleElement, Type, TypeOrSpread};
use stc_utils::cache::Freeze;
use swc_common::Span;

use super::NormalizeTypeOpts;
use crate::{
    analyzer::{expr::GetIteratorOpts, Analyzer},
    VResult,
};

impl Analyzer<'_, '_> {
    pub(crate) fn expand_spread_likes<T>(&mut self, span: Span, elems: &[T]) -> VResult<Vec<TypeOrSpread>>
    where
        T: SpreadLike,
    {
        let mut result = vec![];

        for elem in elems {
            let elem_span = elem.span();
            let spread = elem.spread();
            let label = elem.label();
            let e_ty = elem.ty();

            let ty = self
                .normalize(
                    Some(elem_span),
                    Cow::Borrowed(&e_ty),
                    NormalizeTypeOpts {
                        preserve_global_this: true,
                        ..Default::default()
                    },
                )?
                .freezed();

            if let Some(spread) = spread {
                match ty.normalize() {
                    Type::Tuple(tuple) => {
                        let expanded = self.expand_spread_likes(tuple.span, &tuple.elems)?;

                        result.extend(expanded);
                    }

                    _ => {
                        let e_ty = self.get_iterator_element_type(
                            span,
                            Cow::Borrowed(&e_ty),
                            false,
                            GetIteratorOpts {
                                disallow_str: true,
                                ..Default::default()
                            },
                        )?;

                        result.push(TypeOrSpread {
                            span: elem_span,
                            spread: Some(spread),
                            ty: Box::new(Type::Rest(RestType {
                                span: spread,
                                ty: Box::new(ty.into_owned()),
                                metadata: Default::default(),
                                tracker: Default::default(),
                            })),
                        });
                        continue;
                    }
                }
            }

            result.push(TypeOrSpread {
                span: elem_span,
                spread: None,
                ty: Box::new(ty.into_owned()),
            });
        }

        Ok(result)
    }
}

#[auto_impl::auto_impl(&)]
pub(crate) trait SpreadLike {
    fn span(&self) -> Span;
    fn label(&self) -> Option<&RPat>;
    fn spread(&self) -> Option<Span>;
    fn ty(&self) -> &Type;
}

impl SpreadLike for TupleElement {
    fn span(&self) -> Span {
        self.span
    }

    fn label(&self) -> Option<&RPat> {
        self.label.as_ref()
    }

    fn spread(&self) -> Option<Span> {
        None
    }

    fn ty(&self) -> &Type {
        &self.ty
    }
}

impl SpreadLike for FnParam {
    fn span(&self) -> Span {
        self.span
    }

    fn label(&self) -> Option<&RPat> {
        Some(&self.pat)
    }

    fn spread(&self) -> Option<Span> {
        match &self.pat {
            RPat::Rest(p) => Some(p.dot3_token),
            _ => None,
        }
    }

    fn ty(&self) -> &Type {
        &self.ty
    }
}

impl SpreadLike for TypeOrSpread {
    fn span(&self) -> Span {
        self.span
    }

    fn label(&self) -> Option<&RPat> {
        None
    }

    fn spread(&self) -> Option<Span> {
        self.spread
    }

    fn ty(&self) -> &Type {
        &self.ty
    }
}
