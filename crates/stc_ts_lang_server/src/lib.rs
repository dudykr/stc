use clap::Args;
use lspower::{async_trait, jsonrpc, lsp};

#[derive(Debug, Args)]
pub struct LspCommand {}

pub struct StcLangServer {}

#[async_trait]
impl lspower::LanguageServer for StcLangServer {
    async fn initialize(&self, params: lsp::InitializeParams) -> jsonrpc::Result<lsp::InitializeResult> {
        Ok(lsp::InitializeResult {
            capabilities: lsp::ServerCapabilities { ..Default::default() },
            server_info: Some(lsp::ServerInfo {
                name: "stc-ts-lsp".to_string(),
                version: Some(env!("CARGO_PKG_VERSION").to_string()),
            }),
        })
    }

    async fn shutdown(&self) -> jsonrpc::Result<()> {
        Ok(())
    }
}
