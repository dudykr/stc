use stc_ts_types::Type;
use swc_common::Span;

use crate::{analyzer::Analyzer, VResult};

impl Analyzer<'_, '_> {
    pub(crate) fn expand_spread_likes<T>(&mut self, span: Span, elems: &[T]) -> VResult<Type>
    where
        T: SpreadLike,
    {
    }
}

pub(crate) trait SpreadLike {}
