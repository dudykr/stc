use std::borrow::Cow;

use stc_ts_types::{Tuple, TupleElement, Type, TypeOrSpread};
use stc_utils::cache::Freeze;
use swc_common::Span;

use super::NormalizeTypeOpts;
use crate::{analyzer::Analyzer, VResult};

impl Analyzer<'_, '_> {
    pub(crate) fn expand_spread_likes<T>(&mut self, span: Span, elems: &[T]) -> VResult<Tuple>
    where
        T: SpreadLike,
    {
        let mut tuple = Tuple {
            span,
            elems: Default::default(),
            metadata: Default::default(),
            tracker: Default::default(),
        };

        for elem in elems {
            let e = elem.as_spread_and_type();

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

            if let Some(spread) = e.spread {}
        }

        Ok(tuple)
    }
}

pub(crate) trait SpreadLike {
    fn as_spread_and_type(&self) -> TypeOrSpread;
}
