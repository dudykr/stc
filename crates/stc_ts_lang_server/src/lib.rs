use clap::Args;
use tower_lsp::{
    async_trait,
    jsonrpc::{self, Result},
    lsp_types::*,
    Client, LanguageServer, LspService, Server,
};
use tracing::info;

#[derive(Debug, Args)]
pub struct LspCommand {}

impl LspCommand {
    pub async fn run(self) -> anyhow::Result<()> {
        info!("Starting server");

        let stdin = tokio::io::stdin();
        let stdout = tokio::io::stdout();

        let (service, socket) = LspService::new(|client| StcLangServer { client });
        Server::new(stdin, stdout, socket).serve(service).await;

        Ok(())
    }
}

pub struct StcLangServer {
    #[allow(unused)]
    client: Client,
}

#[async_trait]
impl LanguageServer for StcLangServer {
    async fn initialize(&self, _params: InitializeParams) -> jsonrpc::Result<InitializeResult> {
        Ok(InitializeResult {
            capabilities: ServerCapabilities {
                hover_provider: Some(HoverProviderCapability::Simple(true)),
                text_document_sync: Some(TextDocumentSyncCapability::Options(TextDocumentSyncOptions {
                    open_close: Some(true),
                    change: None,
                    ..Default::default()
                })),
                workspace: Some(WorkspaceServerCapabilities {
                    workspace_folders: Some(WorkspaceFoldersServerCapabilities {
                        supported: Some(true),
                        ..Default::default()
                    }),
                    ..Default::default()
                }),

                ..Default::default()
            },
            server_info: Some(ServerInfo {
                name: "stc-ts-lsp".to_string(),
                version: Some(env!("CARGO_PKG_VERSION").to_string()),
            }),
        })
    }

    async fn initialized(&self, params: InitializedParams) {
        dbg!("initialized", params);
    }

    async fn shutdown(&self) -> jsonrpc::Result<()> {
        dbg!("shutdown");
        Ok(())
    }

    async fn hover(&self, _params: HoverParams) -> jsonrpc::Result<Option<Hover>> {
        dbg!("HOVER!");
        Ok(Some(Hover {
            contents: HoverContents::Scalar(MarkedString::String("hover test".to_string())),
            range: None,
        }))
    }
}
