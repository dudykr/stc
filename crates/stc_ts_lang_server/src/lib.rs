use std::{path::PathBuf, sync::Arc};

use clap::Args;
use stc_ts_type_checker::Checker;
use stc_utils::AHashMap;
use tokio::sync::Mutex;
use tower_lsp::{
    async_trait,
    jsonrpc::{self},
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

        let (service, socket) = LspService::new(|client| StcLangServer {
            client,
            data: Default::default(),
        });
        Server::new(stdin, stdout, socket).serve(service).await;

        Ok(())
    }
}

pub struct StcLangServer {
    #[allow(unused)]
    client: Client,

    data: Mutex<Data>,
}

#[derive(Default)]
struct Data {
    /// A map of the parent directory of `package.json` to project data.
    projects: AHashMap<PathBuf, TsProject>,
}

/// A directory with `package.json` is treated as a package.
struct TsProject {
    checker: Checker,
}

#[async_trait]
impl LanguageServer for StcLangServer {
    async fn initialize(&self, _params: InitializeParams) -> jsonrpc::Result<InitializeResult> {
        Ok(InitializeResult {
            capabilities: ServerCapabilities {
                hover_provider: Some(HoverProviderCapability::Options(HoverOptions {
                    work_done_progress_options: WorkDoneProgressOptions {
                        work_done_progress: Some(false),
                    },
                })),
                ..Default::default()
            },
            server_info: Some(ServerInfo {
                name: "stc-ts-lsp".to_string(),
                version: Some(env!("CARGO_PKG_VERSION").to_string()),
            }),
        })
    }

    async fn shutdown(&self) -> jsonrpc::Result<()> {
        Ok(())
    }

    async fn did_open(&self, params: DidOpenTextDocumentParams) {}

    async fn hover(&self, _params: HoverParams) -> jsonrpc::Result<Option<Hover>> {
        Ok(Some(Hover {
            contents: HoverContents::Scalar(MarkedString::String("hover test".to_string())),
            range: None,
        }))
    }
}
