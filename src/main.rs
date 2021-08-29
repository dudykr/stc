use crate::{check::CheckCommand, lsp::LspCommand, tsc::TscCommand};
use anyhow::Error;
use stc_ts_builtin_types::Lib;
use stc_ts_file_analyzer::{
    env::{Env, ModuleConfig},
    Rule,
};
use stc_ts_module_loader::resolver::node::NodeResolver;
use stc_ts_type_checker::Checker;
use std::{env, path::PathBuf, sync::Arc};
use structopt::StructOpt;
use swc_common::{
    errors::{ColorConfig, Handler},
    SourceMap,
};
use swc_ecma_ast::EsVersion;
use swc_ecma_parser::TsConfig;

mod check;
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
    Check(CheckCommand),
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
        .pretty()
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
        Command::Check(cmd) => {
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

            let path = Arc::new(PathBuf::from(cmd.file));

            checker.check(path);

            for err in checker.take_errors() {
                err.emit(&handler);
            }
        }

        Command::Tsc(cmd) => {
            todo!("tsc")
        }
        Command::Lsp(c) => c.run().await,
    }

    Ok(())
}
