extern crate swc_node_base;

use std::{path::PathBuf, sync::Arc, time::Instant};

use anyhow::Error;
use stc_ts_builtin_types::Lib;
use stc_ts_env::{Env, ModuleConfig, Rule};
use stc_ts_file_analyzer::env::EnvFactory;
use stc_ts_module_loader::resolvers::node::NodeResolver;
use stc_ts_type_checker::Checker;
use structopt::StructOpt;
use swc_common::{
    errors::{ColorConfig, EmitterWriter, Handler},
    FileName, SourceMap,
};
use swc_ecma_ast::EsVersion;
use swc_ecma_parser::TsConfig;
use tracing_subscriber::EnvFilter;

use crate::check::IterateCommand;

mod check;

#[derive(Debug, StructOpt)]
#[structopt(
    name = "stc",
    about = "Super fast type checker for typescript",
    author,
    rename_all = "camel"
)]
enum Command {
    Iterate(IterateCommand),
}

fn main() -> Result<(), Error> {
    let start = Instant::now();

    env_logger::init();

    let sub = tracing_subscriber::FmtSubscriber::builder()
        .with_target(false)
        .with_ansi(true)
        .without_time()
        .with_env_filter(EnvFilter::new("STC_LOG"))
        .pretty()
        .finish();

    tracing::subscriber::set_global_default(sub).unwrap();

    let command = Command::from_args();

    let cm = Arc::new(SourceMap::default());
    let handler = {
        let emitter = Box::new(EmitterWriter::stderr(
            ColorConfig::Always,
            Some(cm.clone()),
            false,
            false,
        ));
        Arc::new(Handler::with_emitter(true, false, emitter))
    };

    rayon::ThreadPoolBuilder::new().build_global().unwrap();

    {
        let end = Instant::now();

        log::info!("Initialization took {:?}", end - start);
    }

    match command {
        Command::Iterate(cmd) => {
            let libs = {
                let start = Instant::now();

                let mut libs = match cmd.libs {
                    Some(libs) => libs
                        .iter()
                        .map(|s| Lib::load(&s))
                        .flatten()
                        .collect::<Vec<_>>(),
                    None => Lib::load("es5"),
                };
                libs.sort();
                libs.dedup();

                let end = Instant::now();

                log::info!("Loading builtin libraries took {:?}", end - start);

                libs
            };

            let env = Env::simple(
                Rule {
                    ..Default::default()
                },
                EsVersion::latest(),
                ModuleConfig::None,
                &libs,
            );

            let path = PathBuf::from(cmd.file);

            {
                let start = Instant::now();

                let checker = Checker::new(
                    cm.clone(),
                    handler.clone(),
                    env.clone(),
                    TsConfig {
                        ..Default::default()
                    },
                    None,
                    Arc::new(NodeResolver),
                );

                checker.load_typings(&path, None, cmd.types.as_deref());

                let end = Instant::now();

                log::info!("Loading typing libraries took {:?}", end - start);
            }

            let mut errors = vec![];

            let start = Instant::now();
            for _ in 0..1 {
                let mut checker = Checker::new(
                    cm.clone(),
                    handler.clone(),
                    env.clone(),
                    TsConfig {
                        ..Default::default()
                    },
                    None,
                    Arc::new(NodeResolver),
                );

                checker.check(Arc::new(FileName::Real(path.clone())));

                errors.extend(checker.take_errors());
            }
            let end = Instant::now();

            log::info!("Checking 1000 times took {:?}", end - start);

            {
                let start = Instant::now();
                for err in errors {
                    err.emit(&handler);
                }

                let end = Instant::now();

                log::info!("Error reporting took {:?}", end - start);
            }
        }
    }

    let end = Instant::now();

    log::info!("Done in {:?}", end - start);

    Ok(())
}
