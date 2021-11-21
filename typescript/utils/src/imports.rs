use swc_atoms::JsWord;
use swc_common::{
    comments::{CommentKind, Comments},
    Span,
};

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum ImportRef {
    /// path="foo"
    Path(JsWord),
    /// type="foo"
    Types(JsWord),

    /// ES6 import.
    Normal(JsWord),
}

impl ImportRef {
    pub fn to_path(self) -> JsWord {
        match self {
            ImportRef::Path(s) => format!("./{}", s).into(),
            ImportRef::Types(s) => s,
            ImportRef::Normal(s) => s,
        }
    }
}

pub fn find_imports_in_comments<C>(comments: C, span: Span) -> Vec<ImportRef>
where
    C: Comments,
{
    let mut deps = vec![];

    comments.with_leading(span.lo, |comments| {
        for c in comments {
            if c.kind != CommentKind::Line {
                continue;
            }
            if let Some(cmt_text) = c
                .text
                .trim()
                .strip_prefix("/")
                .map(|s| s.trim())
                .and_then(|s| s.strip_prefix("<reference"))
                .and_then(|s| s.strip_suffix("\" />"))
                .map(|s| s.trim())
            {
                if let Some(path) = cmt_text.strip_prefix("path=\"") {
                    deps.push(ImportRef::Path(path.into()));
                } else if let Some(path) = cmt_text.strip_prefix("types=\"") {
                    deps.push(ImportRef::Types(path.into()));
                } else {
                    // TODO: Handle lib, types
                }
            }
        }
    });

    deps
}
