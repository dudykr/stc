use dashmap::DashMap;
use fxhash::FxBuildHasher;
use std::sync::Arc;
use swc_common::{
    comments::{Comment, CommentKind, Comments},
    BytePos, DUMMY_SP,
};

type CommentMap = Arc<DashMap<BytePos, Vec<Comment>, FxBuildHasher>>;

/// Multi-threaded implementation of [Comments]
#[derive(Clone, Default)]
pub struct StcComments {
    pub leading: CommentMap,
    pub trailing: CommentMap,
}

impl Comments for StcComments {
    fn add_leading(&self, pos: BytePos, cmt: Comment) {
        self.leading.entry(pos).or_default().push(cmt);
    }

    fn add_leading_comments(&self, pos: BytePos, comments: Vec<Comment>) {
        self.leading.entry(pos).or_default().extend(comments);
    }

    fn has_leading(&self, pos: BytePos) -> bool {
        if let Some(v) = self.leading.get(&pos) {
            !v.is_empty()
        } else {
            false
        }
    }

    fn move_leading(&self, from: BytePos, to: BytePos) {
        let cmt = self.leading.remove(&from);

        if let Some(cmt) = cmt {
            self.leading.entry(to).or_default().extend(cmt.1);
        }
    }

    fn take_leading(&self, pos: BytePos) -> Option<Vec<Comment>> {
        self.leading.remove(&pos).map(|v| v.1)
    }

    fn get_leading(&self, pos: BytePos) -> Option<Vec<Comment>> {
        self.leading.get(&pos).map(|v| v.to_owned())
    }

    fn add_trailing(&self, pos: BytePos, cmt: Comment) {
        self.trailing.entry(pos).or_default().push(cmt)
    }

    fn add_trailing_comments(&self, pos: BytePos, comments: Vec<Comment>) {
        self.trailing.entry(pos).or_default().extend(comments)
    }

    fn has_trailing(&self, pos: BytePos) -> bool {
        if let Some(v) = self.trailing.get(&pos) {
            !v.is_empty()
        } else {
            false
        }
    }

    fn move_trailing(&self, from: BytePos, to: BytePos) {
        let cmt = self.trailing.remove(&from);

        if let Some(cmt) = cmt {
            self.trailing.entry(to).or_default().extend(cmt.1);
        }
    }

    fn take_trailing(&self, pos: BytePos) -> Option<Vec<Comment>> {
        self.trailing.remove(&pos).map(|v| v.1)
    }

    fn get_trailing(&self, pos: BytePos) -> Option<Vec<Comment>> {
        self.trailing.get(&pos).map(|v| v.to_owned())
    }

    fn add_pure_comment(&self, pos: BytePos) {
        let mut leading = self.leading.entry(pos).or_default();
        let pure_comment = Comment {
            kind: CommentKind::Block,
            span: DUMMY_SP,
            text: "#__PURE__".into(),
        };

        if !leading.iter().any(|c| c.text == pure_comment.text) {
            leading.push(pure_comment);
        }
    }
}
