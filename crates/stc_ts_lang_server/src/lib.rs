use std::sync::Arc;

use clap::Args;
use stc_ts_env::StableEnv;
use stc_ts_module_loader::resolvers::node::NodeResolver;
use stc_ts_type_checker::{
    loader::{DefaultFileLoader, ModuleLoader},
    Checker,
};
use swc_common::{Globals, SourceMap, GLOBALS};
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

        let (service, socket) = LspService::new(|client| StcLangServer { client });
        let (service, socket) = LspService::new(|client| {
            let data = Data::default();

            let stable_env = GLOBALS.set(&data.globals, StableEnv::new);

            StcLangServer { client, data, stable_env }
        });
        Server::new(stdin, stdout, socket).serve(service).await;

        Ok(())
    }
}

pub struct StcLangServer {
    #[allow(unused)]
    client: Client,

    data: Data,
    stable_env: StableEnv,
}

#[derive(Default)]
struct Data {
    cm: Arc<SourceMap>,
    globals: Arc<Globals>,
}

impl StcLangServer {
    fn checker_for(&self, file_path: &TextDocumentItem) -> Checker {
        let env = Env::new();

        Checker::new(
            self.data.cm.clone(),
            handler,
            env.clone(),
            debugger,
            Box::new(ModuleLoader::new(self.data.cm.clone(), env, resolver, loader)),
        )
    }
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

    async fn shutdown(&self) -> jsonrpc::Result<()> {
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
