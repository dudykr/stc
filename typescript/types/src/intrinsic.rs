use is_macro::Is;
use stc_visit::Visit;
use swc_common::{EqIgnoreSpan, Span, Spanned};

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, Spanned, EqIgnoreSpan, Visit)]
pub struct Intrinsic {
    pub span: Span,
    pub kind: IntrinsicKind,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, Is, EqIgnoreSpan, Visit)]
pub enum IntrinsicKind {
    Uppercase,
    Lowercase,
    Capitalize,
}
