//! A command used for testing. This starts a language server using same code as
//! the release build.

use clap::Parser;
use stc_ts_lang_server::LspCommand;
use tracing::subscriber::set_global_default;
use tracing_subscriber::EnvFilter;

#[derive(Debug, Parser)]
pub struct App {
    #[clap(flatten)]
    pub cmd: LspCommand,
}

#[tokio::main]
async fn main() {
    let sub = tracing_subscriber::FmtSubscriber::builder()
        .with_target(false)
        .with_ansi(true)
        .without_time()
        .with_env_filter(EnvFilter::new("RUST_LOG"))
        .pretty()
        .finish();

    set_global_default(sub).expect("set_global_default failed");

    let app: App = Parser::parse();

    env_logger::init();

    app.cmd.run().await.unwrap();
}
