use crate::LspBackend;
use lspower::{jsonrpc::Result, lsp::TextDocumentPositionParams};
use swc_common::BytePos;

/// Methods for converting types from [swc_common] and `lspower`.

impl LspBackend {
    pub(super) fn loc(&self, loc: &TextDocumentPositionParams) -> Result<BytePos> {
        loc.text_document
    }
}
