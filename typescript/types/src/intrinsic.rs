use is_macro::Is;
use stc_visit::Visit;
use swc_common::{EqIgnoreSpan, Span, Spanned, TypeEq};

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, Spanned, EqIgnoreSpan, TypeEq, Visit)]
pub struct Intrinsic {
    pub span: Span,
    pub kind: IntrinsicKind,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, Is, EqIgnoreSpan, TypeEq, Visit)]
pub enum IntrinsicKind {
    Uppercase,
    Lowercase,
    Capitalize,
    Uncapitalize,
}

impl From<&'_ str> for IntrinsicKind {
    fn from(s: &str) -> Self {
        match s {
            "Uppercase" => Self::Uppercase,
            "Lowercase" => Self::Lowercase,
            "Capitalize" => Self::Capitalize,
            "Uncapitalize" => Self::Uncapitalize,
            _ => {
                unreachable!("unknown intrinsic type `{}`", s)
            }
        }
    }
}
