//! A command used for testing. This starts a language server using same code as
//! the release build.

use clap::Parser;
use stc_ts_lang_server::LspCommand;

#[derive(Debug, Parser)]
pub struct App {
    #[clap(flatten)]
    pub cmd: LspCommand,
}

#[tokio::main]
async fn main() {
    let app: App = Parser::parse();

    app.cmd.run().await.unwrap();
}
