use swc_atoms::JsWord;
use swc_common::{
    comments::{CommentKind, Comments},
    Span,
};

pub fn find_imports_in_comments<C>(comments: C, span: Span) -> Vec<JsWord>
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
                    deps.push(format!("./{}", path).into());
                } else if let Some(path) = cmt_text.strip_prefix("types=\"") {
                    deps.push(path.into());
                } else {
                    // TODO: Handle lib, types
                }
            }
        }
    });

    deps
}
