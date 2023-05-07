use std::borrow::Cow;

use stc_ts_ast_rnode::RPat;
use stc_ts_types::{RestType, Tuple, TupleElement, Type, TypeOrSpread};
use stc_utils::cache::Freeze;
use swc_common::Span;

use super::NormalizeTypeOpts;
use crate::{
    analyzer::{expr::GetIteratorOpts, Analyzer},
    VResult,
};

impl Analyzer<'_, '_> {
    pub(crate) fn expand_spread_likes<T>(&mut self, span: Span, elems: &[T]) -> VResult<Tuple>
    where
        T: SpreadLike,
    {
        let mut result = Tuple {
            span,
            elems: Default::default(),
            metadata: Default::default(),
            tracker: Default::default(),
        };

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

                        result.elems.extend(expanded.elems);
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

                        result.elems.push(TupleElement {
                            span: elem_span,
                            label: label.cloned(),
                            ty: Box::new(Type::Rest(RestType {
                                span: spread,
                                ty: Box::new(ty.into_owned()),
                                metadata: Default::default(),
                                tracker: Default::default(),
                            })),
                            tracker: Default::default(),
                        });
                        continue;
                    }
                }
            }

            result.elems.push(TupleElement {
                span: elem_span,
                label: label.cloned(),
                ty: Box::new(ty.into_owned()),
                tracker: Default::default(),
            });
        }

        Ok(result)
    }
}

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
