use lspower::{
    jsonrpc::{Error, Result},
    lsp::*,
    Client, LanguageServer,
};
use swc_common::{sync::Lrc, SourceMap};
use tracing::error;

mod swc;

pub struct LspBackend {
    pub cm: Lrc<SourceMap>,
    pub client: Client,
}

#[lspower::async_trait]
impl LanguageServer for LspBackend {
    async fn initialize(&self, _: InitializeParams) -> Result<InitializeResult> {
        Ok(InitializeResult {
            capabilities: ServerCapabilities {
                hover_provider: Some(HoverProviderCapability::Simple(true)),
                ..Default::default()
            },
            server_info: Some(ServerInfo {
                name: "stc".to_string(),
                version: None,
            }),
        })
    }

    async fn initialized(&self, _: InitializedParams) {
        self.client.log_message(MessageType::Info, "server initialized!").await;
    }

    async fn shutdown(&self) -> Result<()> {
        Ok(())
    }

    async fn hover(&self, params: HoverParams) -> Result<Option<Hover>> {
        let loc = params.text_document_position_params;

        error!("Got a textDocument/hover request, but it is not implemented");
        Err(Error::method_not_found())
    }
}
