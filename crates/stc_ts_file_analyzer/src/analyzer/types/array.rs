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
            let e = elem.as_spread_and_type();
            let label = elem.label();

            let ty = self
                .normalize(
                    Some(e.span),
                    Cow::Borrowed(&e.ty),
                    NormalizeTypeOpts {
                        preserve_global_this: true,
                        ..Default::default()
                    },
                )?
                .freezed();

            if let Some(spread) = e.spread {
                match ty.normalize() {
                    Type::Tuple(tuple) => {}

                    _ => {
                        let elem_type = self.get_iterator_element_type(
                            span,
                            Cow::Borrowed(&e.ty),
                            false,
                            GetIteratorOpts {
                                disallow_str: true,
                                ..Default::default()
                            },
                        )?;

                        result.elems.push(TupleElement {
                            span: e.span,
                            label,
                            ty: Box::new(Type::Rest(RestType {
                                span: spread,
                                ty: Box::new(ty.into_owned()),
                                metadata: Default::default(),
                                tracker: Default::default(),
                            })),
                            tracker: Default::default(),
                        });
                    }
                }
            }
        }

        Ok(result)
    }
}

pub(crate) trait SpreadLike {
    fn label(&self) -> Option<RPat>;
    fn as_spread_and_type(&self) -> TypeOrSpread;
}
