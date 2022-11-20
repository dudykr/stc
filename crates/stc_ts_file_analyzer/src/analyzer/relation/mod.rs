use stc_ts_types::Type;
use stc_ts_utils::Ternary;
use swc_common::Span;

use crate::analyzer::Analyzer;

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord)]
pub struct IsRelatedOpts {
    /// If this is [Some], errors are reported with this span. Otherwise, errors
    /// are ignored.
    pub span: Option<Span>,
}

impl Analyzer<'_, '_> {
    pub fn check_type_related_to(&mut self, src: &Type, target: &Type) -> bool {}

    fn is_related(&mut self, src: &Type, target: &Type) -> Ternary {}
}
