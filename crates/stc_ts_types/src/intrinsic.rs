use is_macro::Is;
use serde::{Deserialize, Serialize};
use stc_visit::Visit;
use swc_common::{EqIgnoreSpan, Span, Spanned, TypeEq};

use crate::{IntrinsicMetadata, TypeParamInstantiation};

#[derive(Debug, Clone, PartialEq, Spanned, EqIgnoreSpan, TypeEq, Visit, Serialize, Deserialize)]
pub struct Intrinsic {
    pub span: Span,
    pub kind: IntrinsicKind,
    pub type_args: TypeParamInstantiation,
    pub metadata: IntrinsicMetadata,
}

#[derive(
    Debug,
    Clone,
    PartialEq,
    Eq,
    PartialOrd,
    Ord,
    Hash,
    Is,
    EqIgnoreSpan,
    TypeEq,
    Visit,
    Serialize,
    Deserialize,
)]
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
