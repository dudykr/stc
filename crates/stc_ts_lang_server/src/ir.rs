use std::sync::Arc;

use swc_common::FileName;

#[salsa::input]
pub struct SourceFile {
    #[no_eq]
    pub filename: Arc<FileName>,

    #[return_ref]
    pub content: String,
}
