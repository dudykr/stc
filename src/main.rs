use crate::{lsp::LspCommand, tsc::TscCommand};
use anyhow::Error;
use std::io;
use structopt::StructOpt;
use tracing::Level;

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
    let sub = tracing_subscriber::FmtSubscriber::builder()
        .with_writer(io::stderr)
        .with_target(false)
        .with_ansi(true)
        .with_max_level(Level::TRACE)
        .without_time()
        .finish();

    tracing::subscriber::set_global_default(sub).unwrap();

    let command = Command::from_args();

    match command {
        Command::Tsc(_) => todo!("tsc mode"),
        Command::Lsp(c) => c.run().await,
    }

    Ok(())
}
