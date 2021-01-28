//! Visual testing for dts.
#![allow(incomplete_features)]
#![feature(box_syntax)]
#![feature(specialization)]

use rnode::NodeIdGenerator;
use rnode::RNode;
use stc_testing::logger;
use stc_ts_ast_rnode::RModule;
use stc_ts_builtin_types::Lib;
use stc_ts_errors::debug::debugger::Debugger;
use stc_ts_file_analyzer::analyzer::Analyzer;
use stc_ts_file_analyzer::analyzer::NoopLoader;
use stc_ts_file_analyzer::env::Env;
use stc_ts_file_analyzer::validator::ValidateWith;
use stc_ts_storage::Single;
use stc_ts_types::module_id;
use stc_ts_utils::StcComments;
use std::path::PathBuf;
use std::sync::Arc;
use swc_common::input::SourceFileInput;
use swc_common::GLOBALS;
use swc_ecma_parser::lexer::Lexer;
use swc_ecma_parser::JscTarget;
use swc_ecma_parser::Parser;
use swc_ecma_parser::Syntax;
use swc_ecma_parser::TsConfig;
use swc_ecma_transforms::resolver::ts_resolver;
use swc_ecma_visit::FoldWith;

#[testing::fixture("visualize/**/*.ts", exclude(".*\\.\\.d.\\.ts"))]
fn visualize(file_name: PathBuf) {
    let fname = file_name.display().to_string();
    println!("{}", fname);

    let res = testing::Tester::new()
        .print_errors(|cm, handler| -> Result<(), _> {
            let handler = Arc::new(handler);
            let fm = cm.load_file(&file_name).unwrap();
            let mut libs = vec![];
            let ls = &[
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
            let env = Env::simple(Default::default(), JscTarget::Es2020, &libs);
            let stable_env = env.shared().clone();
            let generator = module_id::Generator::default();
            let path = Arc::new(file_name.clone());

            let mut storage = Single {
                parent: None,
                id: generator.generate(&path).1,
                path,
                info: Default::default(),
            };

            let mut node_id_gen = NodeIdGenerator::default();
            let comments = StcComments::default();

            let lexer = Lexer::new(
                Syntax::Typescript(TsConfig {
                    tsx: fname.contains("tsx"),
                    decorators: true,
                    ..Default::default()
                }),
                JscTarget::Es2020,
                SourceFileInput::from(&*fm),
                Some(&comments),
            );
            let mut parser = Parser::new_from(lexer);
            let log = logger();
            let module = parser.parse_module().unwrap();
            let module = GLOBALS.set(stable_env.swc_globals(), || {
                module.fold_with(&mut ts_resolver(stable_env.marks().top_level_mark()))
            });
            let module = RModule::from_orig(&mut node_id_gen, module);
            {
                let mut analyzer = Analyzer::root(
                    log.logger,
                    env,
                    cm.clone(),
                    box &mut storage,
                    &NoopLoader,
                    Some(Debugger {
                        cm: cm.clone(),
                        handler: handler.clone(),
                    }),
                );
                GLOBALS.set(stable_env.swc_globals(), || {
                    module.validate_with(&mut analyzer).unwrap();
                });
            }

            Err(())
        })
        .unwrap_err();

    res.compare_to_file(&file_name.with_extension("stdout")).unwrap();
}
