use std::{path::PathBuf, sync::Arc};

use clap::Args;
use stc_ts_builtin_types::Lib;
use stc_ts_env::{Env, ModuleConfig, Rule};
use stc_ts_file_analyzer::env::EnvFactory;
use stc_ts_type_checker::Checker;
use stc_utils::AHashMap;
use swc_common::{
    errors::{ColorConfig, Handler},
    SourceMap,
};
use swc_ecma_ast::EsVersion;
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

    open_cnt: usize,
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

    async fn did_open(&self, params: DidOpenTextDocumentParams) {
        let uri = params.text_document.uri;
        let path = uri.to_file_path().unwrap();
        let parent = path.parent().unwrap();
        let mut data = self.data.lock().await;
        let project = data.projects.entry(parent.to_path_buf()).or_insert_with(|| {
            let cm = Arc::new(SourceMap::default());
            let handler = Arc::new(Handler::with_tty_emitter(ColorConfig::Never, true, false, Some(cm.clone())));

            // TODO: Parse tsconfig.json
            let env = Env::simple(
                Rule { ..Default::default() },
                EsVersion::latest(),
                ModuleConfig::EsNext,
                &Lib::load("es2020"),
            );

            let checker = Checker::new(
                cm,
                handler,
                env,
                swc_ecma_parser::TsConfig {
                    // TODO
                    ..Default::default()
                },
                None,
                resolver,
            );
            TsProject { checker, open_cnt: 0 }
        });

        project.open_cnt += 1;
    }

    async fn hover(&self, _params: HoverParams) -> jsonrpc::Result<Option<Hover>> {
        Ok(Some(Hover {
            contents: HoverContents::Scalar(MarkedString::String("hover test".to_string())),
            range: None,
        }))
    }
}
