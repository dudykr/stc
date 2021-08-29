use std::{env, sync::Arc};

use crate::{lsp::LspCommand, tsc::TscCommand};
use anyhow::Error;
use stc_ts_builtin_types::Lib;
use stc_ts_file_analyzer::{
    env::{Env, ModuleConfig},
    Rule,
};
use stc_ts_module_loader::resolver::node::NodeResolver;
use stc_ts_type_checker::Checker;
use structopt::StructOpt;
use swc_common::{
    errors::{ColorConfig, Handler},
    SourceMap,
};
use swc_ecma_ast::EsVersion;
use swc_ecma_parser::TsConfig;

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
        .with_target(false)
        .with_ansi(true)
        .without_time()
        .finish();

    tracing::subscriber::set_global_default(sub).unwrap();

    let command = Command::from_args();

    let cm = Arc::new(SourceMap::default());
    let handler = Arc::new(Handler::with_tty_emitter(
        ColorConfig::Always,
        true,
        false,
        Some(cm.clone()),
    ));

    match command {
        Command::Tsc(c) => {
            let mut checker = Checker::new(
                cm.clone(),
                handler.clone(),
                Env::simple(
                    Rule { ..Default::default() },
                    EsVersion::latest(),
                    ModuleConfig::None,
                    &Lib::load(&env::var("STC_LIBS").unwrap_or("es5".into())),
                ),
                TsConfig { ..Default::default() },
                None,
                Arc::new(NodeResolver),
            );
        }
        Command::Lsp(c) => c.run().await,
    }

    Ok(())
}
