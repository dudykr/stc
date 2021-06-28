use lspower::{jsonrpc::Result, lsp::*, Client, LanguageServer};

#[derive(Debug)]
pub struct LspBackend {
    pub client: Client,
}

#[lspower::async_trait]
impl LanguageServer for LspBackend {
    async fn initialize(&self, _: InitializeParams) -> Result<InitializeResult> {
        Ok(InitializeResult::default())
    }

    async fn initialized(&self, _: InitializedParams) {
        self.client.log_message(MessageType::Info, "server initialized!").await;
    }

    async fn shutdown(&self) -> Result<()> {
        Ok(())
    }
}
