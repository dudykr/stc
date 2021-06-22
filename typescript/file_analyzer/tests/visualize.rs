//! Visual testing for dts.
#![cfg(debug_assertions)]
#![allow(incomplete_features)]
#![feature(box_syntax)]
#![feature(specialization)]

use once_cell::sync::Lazy;
use rnode::{NodeIdGenerator, RNode};
use stc_testing::{load_txt, logger};
use stc_ts_ast_rnode::RModule;
use stc_ts_builtin_types::Lib;
use stc_ts_errors::{debug::debugger::Debugger, Error};
use stc_ts_file_analyzer::{
    analyzer::{Analyzer, NoopLoader},
    env::{Env, ModuleConfig},
    validator::ValidateWith,
    Rule,
};
use stc_ts_storage::{ErrorStore, Single};
use stc_ts_types::module_id;
use stc_ts_utils::StcComments;
use std::{
    env,
    path::{Path, PathBuf},
    sync::Arc,
};
use swc_common::{input::SourceFileInput, GLOBALS};
use swc_ecma_parser::{lexer::Lexer, JscTarget, Parser, Syntax, TsConfig};
use swc_ecma_transforms::resolver::ts_resolver;
use swc_ecma_visit::FoldWith;
use testing::NormalizedOutput;

fn should_run(input: &Path) -> bool {
    static GOLDENS: Lazy<Vec<String>> = Lazy::new(|| load_txt("tests/golden.txt"));

    if env::var("GOLDEN_ONLY").unwrap_or_default() == "1" {
        return GOLDENS.iter().any(|golden| input.to_string_lossy().contains(&**golden));
    }

    true
}

/// If `for_error` is false, this function will run as type dump mode.
fn run_test(file_name: PathBuf, logger: slog::Logger, for_error: bool) -> Option<NormalizedOutput> {
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
            let mut rule = Rule {
                allow_unreachable_code: true,
                always_strict: false,
                no_implicit_any: true,
                allow_unused_labels: true,
                no_fallthrough_cases_in_switch: false,
                no_implicit_returns: false,
                no_implicit_this: false,
                no_strict_generic_checks: false,
                no_unused_locals: false,
                no_unused_parameters: false,
                strict_function_types: false,
                strict_null_checks: false,
                suppress_excess_property_errors: false,
                suppress_implicit_any_index_errors: false,
                use_define_property_for_class_fields: false,
            };

            for line in fm.src.lines() {
                if !line.starts_with("//@") {
                    continue;
                }
                let line = &line["//@".len()..];
                if line.starts_with("strict:") {
                    let value = line["strict:".len()..].parse::<bool>().unwrap();
                    rule.strict_function_types = value;
                    rule.strict_null_checks = value;
                    continue;
                }

                panic!("Invalid directive: {:?}", line)
            }

            let env = Env::simple(rule, JscTarget::Es2020, ModuleConfig::None, &libs);
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
            let module = parser.parse_module().unwrap();
            let module = GLOBALS.set(stable_env.swc_globals(), || {
                module.fold_with(&mut ts_resolver(stable_env.marks().top_level_mark()))
            });
            let module = RModule::from_orig(&mut node_id_gen, module);
            {
                GLOBALS.set(stable_env.swc_globals(), || {
                    let mut analyzer = Analyzer::root(
                        logger,
                        env,
                        cm.clone(),
                        box &mut storage,
                        &NoopLoader,
                        if for_error {
                            None
                        } else {
                            Some(Debugger {
                                cm: cm.clone(),
                                handler: handler.clone(),
                            })
                        },
                    );

                    module.validate_with(&mut analyzer).unwrap();
                });
            }

            if for_error {
                let errors = storage.take_errors();
                let errors = Error::flatten(errors.into());

                for err in errors {
                    err.emit(&handler);
                }
            }

            Err(())
        })
        .unwrap_err();

    if for_error {
        if res.trim().is_empty() {
            return None;
        }

        panic!("Failed to validate.\n{}\n{}", res, file_name.display())
    } else {
        return Some(res);
    }
}

#[testing::fixture("visualize/**/*.ts", exclude(".*\\.\\.d.\\.ts"))]
fn visualize(file_name: PathBuf) {
    if !should_run(&file_name) {
        return;
    }

    let log = logger();
    let res = run_test(file_name.clone(), log.logger, false).unwrap();
    res.compare_to_file(&file_name.with_extension("stdout")).unwrap();

    println!("[SUCCESS]{}", file_name.display())
}

#[testing::fixture("pass/**/*.ts", exclude(".*\\.\\.d.\\.ts"))]
fn pass(file_name: PathBuf) {
    if !should_run(&file_name) {
        return;
    }

    let null_logger = slog::Logger::root(slog::Discard, slog::o!());
    let res = run_test(file_name.clone(), null_logger, false).unwrap();
    println!("TYPES: {}", res);

    let log = logger();
    run_test(file_name.clone(), log.logger, true);

    res.compare_to_file(&file_name.with_extension("stdout")).unwrap();

    println!("[SUCCESS]{}", file_name.display())
}
