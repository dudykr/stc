use stc_ts_types::Type;
use stc_ts_utils::Ternary;
use swc_common::Span;

use crate::analyzer::Analyzer;

#[derive(Debug, Default, Clone, PartialEq, Eq, PartialOrd, Ord)]
pub struct IsRelatedOpts {
    pub span: Span,

    pub kind: Relation,

    /// If this is [Some], errors are reported with this span. Otherwise, errors
    /// are ignored.
    pub error_span: Option<Span>,
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord)]
pub enum Relation {
    Assignability,
    Comparibility,
    Subtype,
}

impl Default for Relation {
    fn default() -> Self {
        Self::Assignability
    }
}

impl Analyzer<'_, '_> {
    pub fn check_type_related_to(&mut self, src: &Type, target: &Type, opts: IsRelatedOpts) -> bool {}

    fn is_related(&mut self, src: &Type, target: &Type, opts: IsRelatedOpts) -> Ternary {}
}
