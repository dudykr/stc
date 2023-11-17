use std::sync::Arc;

use swc_common::FileName;
use tower_lsp::{
    async_trait,
    jsonrpc::{self},
    lsp_types::{InitializeResult, *},
    LanguageServer,
};

use crate::{Request, StcLangServer};

#[async_trait]
impl LanguageServer for StcLangServer {
    async fn initialize(&self, _params: InitializeParams) -> jsonrpc::Result<InitializeResult> {
        Ok(InitializeResult {
            capabilities: ServerCapabilities {
                text_document_sync: Some(TextDocumentSyncCapability::Options(TextDocumentSyncOptions {
                    open_close: Some(true),
                    change: None,
                    ..Default::default()
                })),
                hover_provider: Some(HoverProviderCapability::Options(HoverOptions {
                    work_done_progress_options: WorkDoneProgressOptions {
                        work_done_progress: Some(false),
                    },
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

    // async fn hover(&self, params: HoverParams) -> jsonrpc::Result<Option<Hover>>
    // {}

    async fn did_open(&self, params: DidOpenTextDocumentParams) {
        let filename = to_filename(params.text_document.uri);

        self.project
            .sender
            .lock()
            .await
            .send(Request::SetFileContent {
                filename: filename.clone(),
                content: params.text_document.text,
            })
            .expect("failed to send request");

        self.project
            .sender
            .lock()
            .await
            .send(Request::ValidateFile { filename })
            .expect("failed to send request");
    }

    async fn did_change(&self, params: DidChangeTextDocumentParams) {
        self.project
            .sender
            .lock()
            .await
            .send(Request::SetFileContent {
                filename: to_filename(params.text_document.uri),
                content: params.content_changes[0].text.clone(),
            })
            .expect("failed to send request");
    }
}

fn to_filename(uri: Url) -> Arc<FileName> {
    if let Ok(v) = uri.to_file_path() {
        return Arc::new(FileName::Real(v));
    }
    Arc::new(FileName::Url(uri))
}
