use crate::{lsp::LspCommand, tsc::TscCommand};
use anyhow::Error;
use structopt::StructOpt;
use tracing::{info, span, Level};
use tracing_subscriber::util::SubscriberInitExt;

mod lsp;
mod tsc;

#[derive(Debug, StructOpt)]
#[structopt(
    name = "stc",
    about = "Super fast type checker for typescript",
    author,
    rename_all = "camel"
)]
enum Command {
    /// Compatibillity layer for `tsc` cli.
    Tsc(TscCommand),
    Lsp(LspCommand),
}

#[tokio::main]
async fn main() -> Result<(), Error> {
    tracing_subscriber::FmtSubscriber::builder()
        .with_ansi(true)
        .without_time()
        .set_default();

    let command = Command::from_args();

    match command {
        Command::Tsc(_) => todo!("tsc mode"),
        Command::Lsp(c) => {
            let _span = span!(Level::DEBUG, "command", "lsp");

            c.run().await
        }
    }

    Ok(())
}
