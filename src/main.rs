use anyhow::Error;
use slog::Discard;
use slog::Logger;
use stc_ts_builtin_types::Lib;
use stc_ts_file_analyzer::env::Env;
use stc_ts_file_analyzer::Rule;
use stc_ts_type_checker::Checker;
use std::path::PathBuf;
use std::sync::Arc;
use structopt::StructOpt;
use swc_common::errors::ColorConfig;
use swc_common::errors::Handler;
use swc_common::FilePathMapping;
use swc_common::SourceMap;
use swc_ecma_parser::JscTarget;
use swc_ecma_parser::TsConfig;

#[derive(Debug, StructOpt)]
#[structopt(
    name = "stc",
    about = "Super fast type checker for typescript",
    author,
    rename_all = "camel"
)]
struct CliOptions {
    /// Generates corresponding '.d.ts' file.
    #[structopt(short, long)]
    declaration: bool,

    /// Do not emit outputs.
    #[structopt(long)]
    no_emit: bool,

    inputs: Vec<PathBuf>,
}

fn main() -> Result<(), Error> {
    let cm = Arc::new(SourceMap::new(FilePathMapping::empty()));
    let handler = Arc::new(Handler::with_tty_emitter(
        ColorConfig::Auto,
        true,
        false,
        Some(cm.clone()),
    ));
    let cli_options = CliOptions::from_args();

    let checker = Checker::new(
        Logger::root(Discard, slog::o!()),
        cm.clone(),
        handler,
        Env::simple(
            Rule::default(),
            JscTarget::Es2020,
            &Lib::load("es2020.full"),
        ),
        TsConfig {
            ..Default::default()
        },
        None,
    );

    if cli_options.declaration {
        for input in cli_options.inputs {
            let id = checker.check(Arc::new(input));
            let _dts_module = checker.take_dts(id);
        }
    }

    Ok(())
}
