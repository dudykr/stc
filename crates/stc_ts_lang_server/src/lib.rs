use std::sync::Arc;

use clap::Args;
use dashmap::DashMap;
use stc_ts_env::{Env, StableEnv};
use stc_ts_module_loader::resolvers::node::NodeResolver;
use stc_ts_type_checker::{
    loader::{DefaultFileLoader, ModuleLoader},
    Checker,
};
use swc_common::{FileName, Globals, SourceMap, GLOBALS};
use tower_lsp::{
    async_trait,
    jsonrpc::{self},
    lsp_types::*,
    Client, LanguageServer, LspService, Server,
};
use tracing::info;

#[salsa::jar(db = Db)]
pub struct Jar();

pub trait Db: salsa::DbWithJar<Jar> {}

impl<DB> Db for DB where DB: ?Sized + salsa::DbWithJar<Jar> {}

#[derive(Default)]
#[salsa::db(crate::Jar)]
pub(crate) struct Database {
    storage: salsa::Storage<Self>,
}

impl salsa::Database for Database {}

impl salsa::ParallelDatabase for Database {
    fn snapshot(&self) -> salsa::Snapshot<Self> {
        salsa::Snapshot::new(Database {
            storage: self.storage.snapshot(),
        })
    }
}

#[derive(Debug, Args)]
pub struct LspCommand {}

impl LspCommand {
    pub async fn run(self) -> anyhow::Result<()> {
        info!("Starting server");

        let stdin = tokio::io::stdin();
        let stdout = tokio::io::stdout();

        let (service, socket) = LspService::new(|client| {
            let cm = Arc::default();
            let globals = Arc::default();

            let stable_env = GLOBALS.set(&globals, StableEnv::new);

            StcLangServer {
                shared: Arc::new(Shared {
                    client,
                    stable_env,
                    cm,
                    globals,
                }),
                projects: Default::default(),
            }
        });
        Server::new(stdin, stdout, socket).serve(service).await;

        Ok(())
    }
}

pub struct StcLangServer {
    shared: Arc<Shared>,

    /// dir: [Project]
    projects: DashMap<Arc<FileName>, Project>,
}

struct Shared {
    client: Client,
    cm: Arc<SourceMap>,
    globals: Arc<Globals>,
    stable_env: StableEnv,
}

/// One directory with `tsconfig.json`.
struct Project {
    shared: Arc<Shared>,

    dir: Arc<FileName>,

    module_loader: Arc<ModuleLoader<DefaultFileLoader, NodeResolver>>,
}

impl Project {
    fn new_checker_for(&self, file_path: &TextDocumentItem) -> Checker {
        let env = Env::new(self.shared.stable_env);

        Checker::new(
            self.shared.cm.clone(),
            handler,
            env.clone(),
            debugger,
            Box::new(self.module_loader.clone()),
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
