use std::{
    path::{Path, PathBuf},
    sync::Arc,
};

use anyhow::Context;
use clap::Args;
use once_cell::sync::Lazy;
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
use swc_ecma_loader::{resolve::Resolve, resolvers::node::NodeModulesResolver, TargetEnv};
use tokio::{spawn, sync::Mutex};
use tower_lsp::{
    async_trait,
    jsonrpc::{self},
    lsp_types::{notification::Progress, *},
    Client, LanguageServer, LspService, Server,
};
use tracing::info;

/// Cached node.js resolver
static NODE_RESOLVER: Lazy<Arc<dyn Resolve>> = Lazy::new(|| {
    //
    let r = NodeModulesResolver::new(TargetEnv::Node, Default::default(), false);
    let r = swc_ecma_loader::resolvers::lru::CachingResolver::new(1024, r);
    Arc::new(r)
});

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
    project_without_root: Option<TsProject>,

    /// A map of the parent directory of `tsconfig.json` to project data.
    projects: AHashMap<PathBuf, TsProject>,
}

/// A directory with `tsconfig.json` is treated as a package.
struct TsProject {
    checker: Arc<Checker>,

    open_cnt: usize,
}

impl StcLangServer {
    fn create_project(&self, project_root_dir: Option<&Path>) -> TsProject {
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
            // TODO: tsc resolver
            NODE_RESOLVER.clone(),
        );
        let checker = Arc::new(checker);

        if let Some(project_root_dir) = project_root_dir {
            spawn({
                let project_root_dir = project_root_dir.to_owned();
                let checker = checker.clone();
                // TODO: parse tsconfig.json
                async move { checker.load_typings(&project_root_dir, None, None) }
            });
        }

        TsProject { checker, open_cnt: 0 }
    }

    async fn with_project<F, R>(&self, uri: &Url, op: F) -> R
    where
        F: FnOnce(&mut TsProject) -> R,
    {
        let tsconfig_path = find_tsconfig_json(&uri.to_file_path().unwrap());
        let tsconfig_path = match tsconfig_path {
            Ok(v) => v,
            Err(e) => {
                log::warn!("{:?}", e);

                let mut p = self.data.lock().await;
                let project = p.project_without_root.get_or_insert_with(|| self.create_project(None));

                return op(project);
            }
        };
        let project_root_dir = tsconfig_path.parent().unwrap();
        let mut data = self.data.lock().await;
        let project = data
            .projects
            .entry(project_root_dir.to_path_buf())
            .or_insert_with(|| self.create_project(Some(&project_root_dir)));

        op(project)
    }
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

        self.with_project(&uri, |project| {
            project.open_cnt += 1;
        })
        .await;
    }

    async fn hover(&self, _params: HoverParams) -> jsonrpc::Result<Option<Hover>> {
        Ok(Some(Hover {
            contents: HoverContents::Scalar(MarkedString::String("hover test".to_string())),
            range: None,
        }))
    }
}

fn find_tsconfig_json(start: &Path) -> anyhow::Result<PathBuf> {
    run(|| {
        //

        let mut c = Some(start);

        while let Some(cur) = c {
            let tsconfig = cur.join("tsconfig.json");
            if tsconfig.is_file() {
                return Ok(tsconfig);
            }

            c = cur.parent();
        }

        anyhow::bail!("failed to find tsconfig.json")
    })
    .with_context(|| format!("failed to find tsconfig.json for `{}`", start.display()))
}

fn run<F, Ret>(op: F) -> anyhow::Result<Ret>
where
    F: FnOnce() -> anyhow::Result<Ret>,
{
    op()
}
