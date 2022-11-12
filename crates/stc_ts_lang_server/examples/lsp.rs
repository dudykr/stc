use clap::Parser;
use stc_ts_lang_server::LspCommand;

#[derive(Debug, Parser)]
pub struct App {
    #[clap(flatten)]
    pub cmd: LspCommand,
}

fn main() {}
