extern crate swc_node_base;

use std::{path::PathBuf, sync::Arc};

use anyhow::Error;
use clap::Parser;
use stc_ts_builtin_types::Lib;
use stc_ts_env::{Env, ModuleConfig, Rule};
use stc_ts_file_analyzer::env::EnvFactory;
use stc_ts_lang_server::LspCommand;
use stc_ts_module_loader::resolvers::node::NodeResolver;
use stc_ts_type_checker::{
    loader::{DefaultFileLoader, ModuleLoader},
    Checker,
};
use stc_utils::perf_timer::PerfTimer;
use swc_common::{
    errors::{ColorConfig, EmitterWriter, Handler},
    FileName, Globals, SourceMap, GLOBALS,
};
use swc_ecma_ast::EsVersion;
use tracing_subscriber::EnvFilter;

use crate::check::TestCommand;

mod check;

#[derive(Debug, Parser)]
#[command(name = "stc", about = "Super fast type checker for typescript", author, rename_all = "camel")]
enum Command {
    Test(TestCommand),
    Lsp(LspCommand),
}

#[tokio::main]
async fn main() -> Result<(), Error> {
    let timer = PerfTimer::log_info();

    env_logger::init();

    let sub = tracing_subscriber::FmtSubscriber::builder()
        .with_target(false)
        .with_ansi(true)
        .without_time()
        .with_env_filter(EnvFilter::new("STC_LOG"))
        .pretty()
        .finish();

    tracing::subscriber::set_global_default(sub).unwrap();

    let command = Command::parse();

    let cm = Arc::new(SourceMap::default());
    let handler = {
        let emitter = Box::new(EmitterWriter::stderr(ColorConfig::Always, Some(cm.clone()), false, false));
        Arc::new(Handler::with_emitter(true, false, emitter))
    };

    rayon::ThreadPoolBuilder::new().build_global().unwrap();

    timer.log("Initialization");

    let globals = Arc::<Globals>::default();

    match command {
        Command::Test(cmd) => {
            let libs = {
                let timer = PerfTimer::log_info();

                let mut libs = match cmd.libs {
                    Some(libs) => libs.iter().flat_map(|s| Lib::load(s)).collect::<Vec<_>>(),
                    None => Lib::load("es5"),
                };
                libs.sort();
                libs.dedup();

                timer.log("Loading builtin libraries");

                libs
            };

            let env = GLOBALS.set(&globals, || {
                Env::simple(Rule { ..Default::default() }, EsVersion::latest(), ModuleConfig::None, &libs)
            });

            let path = PathBuf::from(cmd.file);

            {
                let timer = PerfTimer::log_info();

                let checker = Checker::new(
                    cm.clone(),
                    handler.clone(),
                    env.clone(),
                    None,
                    Box::new(ModuleLoader::new(cm.clone(), env.clone(), NodeResolver, DefaultFileLoader)),
                );

                checker.load_typings(&path, None, cmd.types.as_deref());

                timer.log("Loading typing libraries");
            }

            let mut errors = vec![];

            GLOBALS.set(&globals, || {
                let mut checker = Checker::new(
                    cm.clone(),
                    handler.clone(),
                    env.clone(),
                    None,
                    Box::new(ModuleLoader::new(cm, env, NodeResolver, DefaultFileLoader)),
                );

                checker.check(Arc::new(FileName::Real(path)));

                errors.extend(checker.take_errors());
            });

            timer.log("Checking");

            {
                let timer = PerfTimer::log_info();

                for err in &errors {
                    err.emit(&handler);
                }

                log::info!("Found {} errors", errors.len());

                timer.log("Error reporting");
            }
        }
        Command::Lsp(cmd) => {
            cmd.run().await?;
        }
    }

    timer.log("Done");

    Ok(())
}
