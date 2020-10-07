use anyhow::Context;
use anyhow::Error;
use slog::Discard;
use slog::Logger;
use stc_checker::env::Env;
use stc_checker::Checker;
use stc_checker::Lib;
use stc_devtools::check_with_tsc;
use std::path::PathBuf;
use std::sync::Arc;
use structopt::StructOpt;
use swc_common::errors::ColorConfig;
use swc_common::errors::Handler;
use swc_common::FilePathMapping;
use swc_common::SourceMap;
use swc_ecma_parser::lexer::Lexer;
use swc_ecma_parser::JscTarget;
use swc_ecma_parser::Parser;
use swc_ecma_parser::StringInput;
use swc_ecma_parser::Syntax;
use swc_ecma_parser::TsConfig;

#[derive(Debug, StructOpt)]
#[structopt(
    name = "dev-stc",
    about = "Devtools for stc",
    author,
    rename_all = "camel"
)]
enum Command {
    Compare { files: Vec<PathBuf> },
}

fn main() -> Result<(), Error> {
    let command = Command::from_args();
    let cm = Arc::new(SourceMap::new(FilePathMapping::empty()));
    let handler = Arc::new(Handler::with_tty_emitter(
        ColorConfig::Auto,
        true,
        false,
        Some(cm.clone()),
    ));

    let _checker = Checker::new(
        Logger::root(Discard, slog::o!()),
        cm.clone(),
        handler,
        Env::simple(Default::default(), JscTarget::Es2020, &Lib::load("es2020")),
        TsConfig {
            ..Default::default()
        },
    );

    match command {
        Command::Compare { files } => {
            //
            for file in files {
                eprintln!("Processing {}", file.display());

                let fm = cm
                    .load_file(&file)
                    .with_context(|| format!("failed to load file `{}`", file.display()))?;

                let lexer = Lexer::new(
                    Syntax::Typescript(TsConfig {
                        ..Default::default()
                    }),
                    JscTarget::Es2020,
                    StringInput::from(&*fm),
                    None,
                );
                let mut parser = Parser::new_from(lexer);
                let module = parser.parse_module().unwrap();

                check_with_tsc(module)?;
            }
        }
    }

    Ok(())
}
