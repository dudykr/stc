use lspower::{LspService, Server};
use stc_ts_lang_server::LspBackend;
use structopt::StructOpt;
use tracing::{info, instrument};

#[derive(Debug, StructOpt)]
#[structopt(rename_all = "camel-case")]
pub struct LspCommand {}

impl LspCommand {
    #[instrument]
    pub async fn run(self) {
        info!("Starting server");

        let stdin = tokio::io::stdin();
        let stdout = tokio::io::stdout();

        let (service, messages) = LspService::new(|client| LspBackend { client });
        Server::new(stdin, stdout).interleave(messages).serve(service).await;
    }
}
