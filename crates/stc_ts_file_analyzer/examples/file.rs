#![feature(box_syntax)]

use std::{env, path::Path, sync::Arc};

use rnode::{NodeIdGenerator, RNode};
use stc_testing::logger;
use stc_ts_ast_rnode::RModule;
use stc_ts_builtin_types::Lib;
use stc_ts_env::Env;
use stc_ts_errors::{debug::debugger::Debugger, ErrorKind};
use stc_ts_file_analyzer::{
    analyzer::{Analyzer, NoopLoader},
    env::EnvFactory,
    validator::ValidateWith,
};
use stc_ts_storage::{ErrorStore, Single};
use stc_ts_testing::conformance::parse_conformance_test;
use stc_ts_types::module_id;
use stc_ts_utils::StcComments;
use swc_common::{input::SourceFileInput, FileName, SyntaxContext};
use swc_ecma_ast::EsVersion;
use swc_ecma_parser::{lexer::Lexer, Parser, Syntax, TsConfig};
use swc_ecma_transforms::resolver;
use swc_ecma_visit::FoldWith;
use tracing::Level;

fn profile_file(file_name: &Path) {
    let filename = file_name.display().to_string();
    println!("{}", filename);
    let want_error = true;

    for case in parse_conformance_test(file_name).unwrap() {
        testing::Tester::new()
            .print_errors(|cm, handler| -> Result<(), _> {
                let handler = Arc::new(handler);
                let fm = cm.load_file(file_name).unwrap();
                let mut libs = vec![];
                let ls = &[
                    "es2022.full",
                    "es2021.full",
                    "es2020.full",
                    "es2019.full",
                    "es2018.full",
                    "es2017.full",
                    "es2016.full",
                    "es2015.full",
                ];
                for s in ls {
                    libs.extend(Lib::load(s))
                }
                libs.sort();
                libs.dedup();

                let env = Env::simple(case.rule, case.target, case.module_config, &libs);
                let stable_env = env.shared().clone();
                let generator = module_id::ModuleIdGenerator::default();
                let path = Arc::new(FileName::Real(file_name.to_path_buf()));

                let (module_id, top_level_mark) = generator.generate(&path);

                let mut storage = Single {
                    parent: None,
                    id: module_id,
                    top_level_ctxt: SyntaxContext::empty().apply_mark(top_level_mark),
                    path,
                    is_dts: false,
                    info: Default::default(),
                };

                let mut node_id_gen = NodeIdGenerator::default();
                let comments = StcComments::default();

                let lexer = Lexer::new(
                    Syntax::Typescript(TsConfig {
                        tsx: filename.contains("tsx"),
                        decorators: true,
                        ..Default::default()
                    }),
                    EsVersion::Es2020,
                    SourceFileInput::from(&*fm),
                    Some(&comments),
                );
                let mut parser = Parser::new_from(lexer);
                let module = parser.parse_module().unwrap();
                let module = module.fold_with(&mut resolver(stable_env.marks().unresolved_mark(), top_level_mark, true));
                let module = RModule::from_orig(&mut node_id_gen, module);
                {
                    let mut analyzer = Analyzer::root(
                        env,
                        cm.clone(),
                        Default::default(),
                        box &mut storage,
                        &NoopLoader,
                        if want_error {
                            None
                        } else {
                            Some(Debugger {
                                cm,
                                handler: handler.clone(),
                            })
                        },
                    );

                    let log_sub = logger(Level::DEBUG);

                    let _guard = tracing::subscriber::set_default(log_sub);

                    module.validate_with(&mut analyzer).unwrap();
                }

                if want_error {
                    let errors = storage.take_errors();
                    let errors = ErrorKind::flatten(errors.into());

                    for err in errors {
                        err.emit(&handler);
                    }
                }

                Err(())
            })
            .unwrap_err();
    }
}

fn main() {
    match env::args().nth(1) {
        Some(input) => profile_file(Path::new(&input)),
        None => {
            println!("Input file not found.\nPlease input the file path.");
        }
    };
}
