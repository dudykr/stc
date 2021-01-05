use crate::server::state::GlobalStateSnapshot;
use anyhow::bail;
use anyhow::Error;
use lsp_types::CompletionParams;
use lsp_types::CompletionResponse;

pub fn handler_completion(
    state: GlobalStateSnapshot,
    params: CompletionParams,
) -> Result<Option<CompletionResponse>, Error> {
    let _p = profile::span("handle_completion");
    let text_document_position = params.text_document_position.clone();
    let position = state.file_position(params.text_document_position)?;

    bail!("unimplemented")
}
