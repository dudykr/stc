use crate::{lsp::LspCommand, tsc::TscCommand};
use anyhow::Error;
use structopt::StructOpt;

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
    let command = Command::from_args();

    match command {
        Command::Tsc(_) => todo!("tsc mode"),
        Command::Lsp(c) => c.run().await,
    }

    Ok(())
}
