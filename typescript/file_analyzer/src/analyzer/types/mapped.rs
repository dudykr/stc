use crate::analyzer::Analyzer;
use crate::ValidationResult;
use stc_ts_types::Mapped;
use swc_common::Span;

impl Analyzer<'_, '_> {
    /// Required because mapped type can specified by user, like

    ///
    /// ```ts
    /// declare const a: Partial<Foo>;
    /// ```
    pub(crate) fn expand_mapped(&mut self, span: Span, m: &Mapped) -> ValidationResult {
        unimplemented!("expand_mapped: {:#?}", m)
    }
}
