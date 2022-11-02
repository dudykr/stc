#![feature(box_syntax)]

use std::{
    path::{Path, PathBuf},
    process::Command,
    sync::Arc,
};

use itertools::Itertools;
use rnode::{NodeIdGenerator, RNode, VisitWith};
use stc_testing::logger;
use stc_ts_ast_rnode::RModule;
use stc_ts_builtin_types::Lib;
use stc_ts_env::{Env, ModuleConfig, Rule};
use stc_ts_errors::{debug::debugger::Debugger, Error};
use stc_ts_file_analyzer::{
    analyzer::{Analyzer, NoopLoader},
    env::EnvFactory,
    validator::ValidateWith,
};
use stc_ts_storage::{ErrorStore, Single};
use stc_ts_testing::tsc::TscError;
use stc_ts_types::module_id;
use stc_ts_utils::StcComments;
use swc_common::{errors::DiagnosticId, input::SourceFileInput, FileName, GLOBALS};
use swc_ecma_ast::EsVersion;
use swc_ecma_parser::{lexer::Lexer, Parser, Syntax, TsConfig};
use swc_ecma_transforms::resolver::ts_resolver;
use swc_ecma_visit::FoldWith;
use testing::{fixture, NormalizedOutput, Tester};
use tracing::Level;

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
struct StcError {
    line: usize,
    code: usize,
}

fn get_env() -> Env {
    let mut libs = vec![];
    let ls = &["es2017.full", "es2016.full", "es2015.full"];
    for s in ls {
        libs.extend(Lib::load(s))
    }
    libs.sort();
    libs.dedup();

    Env::simple(
        Rule {
            strict_function_types: true,
            ..Default::default()
        },
        EsVersion::latest(),
        ModuleConfig::None,
        &libs,
    )
}

fn validate(input: &Path) -> Vec<StcError> {
    let tester = Tester::new();
    let diagnostics = tester
        .errors(|cm, handler| {
            cm.new_source_file(FileName::Anon, "".to_string());

            let fm = cm.load_file(input).unwrap();

            let env = get_env();

            let generator = module_id::ModuleIdGenerator::default();
            let path = Arc::new(FileName::Real(input.to_path_buf()));

            let mut node_id_gen = NodeIdGenerator::default();
            let mut module = {
                let lexer = Lexer::new(
                    Syntax::Typescript(TsConfig { ..Default::default() }),
                    EsVersion::Es2021,
                    SourceFileInput::from(&*fm),
                    None,
                );
                let mut parser = Parser::new_from(lexer);

                parser.parse_module().unwrap()
            };
            module = GLOBALS.set(env.shared().swc_globals(), || {
                module.fold_with(&mut ts_resolver(env.shared().marks().top_level_mark()))
            });
            let module = RModule::from_orig(&mut node_id_gen, module);

            let mut storage = Single {
                parent: None,
                id: generator.generate(&path),
                path,
                is_dts: false,
                info: Default::default(),
            };

            {
                // Don't print logs from builtin modules.
                let _tracing = tracing::subscriber::set_default(logger(Level::DEBUG));

                let mut analyzer = Analyzer::root(env.clone(), cm.clone(), Default::default(), box &mut storage, &NoopLoader, None);
                module.visit_with(&mut analyzer);
            }

            let errors = ::stc_ts_errors::Error::flatten(storage.info.errors.into_iter().collect());

            GLOBALS.set(env.shared().swc_globals(), || {
                for e in errors {
                    e.emit(&handler);
                }
            });

            if false {
                return Ok(());
            }

            return Err(());
        })
        .expect_err("");

    diagnostics
        .into_iter()
        .map(|d| {
            let span = d.span.primary_span().unwrap();
            let cp = tester.cm.lookup_char_pos(span.lo());
            let code = d
                .code
                .clone()
                .expect("conformance testing: All errors should have proper error code");
            let code = match code {
                DiagnosticId::Error(err) => err,
                DiagnosticId::Lint(lint) => {
                    unreachable!("Unexpected lint '{}' found", lint)
                }
            };

            eprintln!("{}", d.message[0].0);

            StcError {
                line: cp.line,
                code: code.strip_prefix("TS").unwrap().parse().unwrap(),
            }
        })
        .collect()
}

#[fixture("tests/errors/**/*.ts")]
fn errors(input: PathBuf) {
    testing::run_test2(false, |cm, handler| {
        cm.new_source_file(FileName::Anon, "".to_string());

        let fm = cm.load_file(&input).unwrap();

        let env = get_env();

        let generator = module_id::ModuleIdGenerator::default();
        let path = Arc::new(FileName::Real(input.to_path_buf()));

        let mut node_id_gen = NodeIdGenerator::default();
        let mut module = {
            let lexer = Lexer::new(
                Syntax::Typescript(TsConfig { ..Default::default() }),
                EsVersion::Es2021,
                SourceFileInput::from(&*fm),
                None,
            );
            let mut parser = Parser::new_from(lexer);

            parser.parse_module().unwrap()
        };
        module = GLOBALS.set(env.shared().swc_globals(), || {
            module.fold_with(&mut ts_resolver(env.shared().marks().top_level_mark()))
        });
        let module = RModule::from_orig(&mut node_id_gen, module);

        let mut storage = Single {
            parent: None,
            id: generator.generate(&path),
            path,
            info: Default::default(),
            is_dts: false,
        };

        {
            // Don't print logs from builtin modules.
            let _tracing = tracing::subscriber::set_default(logger(Level::DEBUG));

            let mut analyzer = Analyzer::root(env.clone(), cm.clone(), Default::default(), box &mut storage, &NoopLoader, None);
            module.visit_with(&mut analyzer);
        }

        let errors = ::stc_ts_errors::Error::flatten(storage.info.errors.into_iter().collect());

        if errors.is_empty() {
            panic!("Should emit at least one error")
        }

        GLOBALS.set(env.shared().swc_globals(), || {
            for e in errors {
                e.emit(&handler);
            }
        });

        if false {
            return Ok(());
        }

        return Err(());
    })
    .unwrap_err();
}

#[fixture("tests/pass-only/**/*.ts")]
fn pass_only(input: PathBuf) {
    testing::run_test2(false, |cm, handler| {
        cm.new_source_file(FileName::Anon, "".to_string());

        let fm = cm.load_file(&input).unwrap();

        let env = get_env();

        let generator = module_id::ModuleIdGenerator::default();
        let path = Arc::new(FileName::Real(input.to_path_buf()));

        let mut node_id_gen = NodeIdGenerator::default();
        let mut module = {
            let lexer = Lexer::new(
                Syntax::Typescript(TsConfig { ..Default::default() }),
                EsVersion::Es2021,
                SourceFileInput::from(&*fm),
                None,
            );
            let mut parser = Parser::new_from(lexer);

            parser.parse_module().unwrap()
        };
        module = GLOBALS.set(env.shared().swc_globals(), || {
            module.fold_with(&mut ts_resolver(env.shared().marks().top_level_mark()))
        });
        let module = RModule::from_orig(&mut node_id_gen, module);

        let mut storage = Single {
            parent: None,
            id: generator.generate(&path),
            path,
            info: Default::default(),
            is_dts: false,
        };

        {
            // Don't print logs from builtin modules.
            let _tracing = tracing::subscriber::set_default(logger(Level::DEBUG));

            let mut analyzer = Analyzer::root(env.clone(), cm.clone(), Default::default(), box &mut storage, &NoopLoader, None);
            module.visit_with(&mut analyzer);
        }

        let errors = ::stc_ts_errors::Error::flatten(storage.info.errors.into_iter().collect());
        let ok = errors.is_empty();

        GLOBALS.set(env.shared().swc_globals(), || {
            for e in errors {
                e.emit(&handler);
            }
        });

        if !ok {
            return Err(());
        }

        return Ok(());
    })
    .unwrap();
}

// This invokes `tsc` to get expected result.
#[fixture("tests/tsc/**/*.ts")]
fn compare(input: PathBuf) {
    let mut actual = validate(&input);
    actual.sort();

    let tsc_result = invoke_tsc(&input);

    let mut expected = tsc_result
        .into_iter()
        .map(|err| StcError {
            line: err.line,
            code: err.code,
        })
        .collect_vec();
    expected.sort();

    assert_eq!(actual, expected);
}

fn invoke_tsc(input: &Path) -> Vec<TscError> {
    let output = Command::new("npx")
        .arg("tsc")
        .arg("--pretty")
        .arg("--noEmit")
        .arg("--lib")
        .arg("es2020")
        .arg(&input)
        .output()
        .expect("failed to invoke tsc");
    let stdout = String::from_utf8_lossy(&output.stdout);
    let stderr = String::from_utf8_lossy(&output.stderr);

    eprintln!("tsc output: \nStdout:\n{}\nStderr:\n{}", stdout, stderr);

    TscError::parse_all(&stdout)
}

/// If `for_error` is false, this function will run as type dump mode.
fn run_test(file_name: PathBuf, for_error: bool) -> Option<NormalizedOutput> {
    let filename = file_name.display().to_string();
    println!("{}", filename);

    let res = testing::Tester::new()
        .print_errors(|cm, handler| -> Result<(), _> {
            cm.new_source_file(FileName::Anon, "".to_string());

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
                let line = &line["//@".len()..].trim();
                if line.starts_with("strict:") {
                    let value = line["strict:".len()..].trim().parse::<bool>().unwrap();
                    rule.strict_function_types = value;
                    rule.strict_null_checks = value;
                    continue;
                }
                if line.to_ascii_lowercase().starts_with(&"allowUnreachableCode:".to_ascii_lowercase()) {
                    let value = line["allowUnreachableCode:".len()..].trim().parse::<bool>().unwrap();
                    rule.allow_unreachable_code = value;
                    continue;
                }

                panic!("Invalid directive: {:?}", line)
            }

            let env = Env::simple(rule, EsVersion::Es2020, ModuleConfig::None, &libs);
            let stable_env = env.shared().clone();
            let generator = module_id::ModuleIdGenerator::default();
            let path = Arc::new(FileName::Real(file_name.clone()));

            let mut storage = Single {
                parent: None,
                id: generator.generate(&path),
                path,
                info: Default::default(),
                is_dts: false,
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
            let module = GLOBALS.set(stable_env.swc_globals(), || {
                module.fold_with(&mut ts_resolver(stable_env.marks().top_level_mark()))
            });
            let module = RModule::from_orig(&mut node_id_gen, module);
            {
                GLOBALS.set(stable_env.swc_globals(), || {
                    let mut analyzer = Analyzer::root(
                        env,
                        cm.clone(),
                        Default::default(),
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

                    let log_sub = logger(Level::DEBUG);

                    let _guard = tracing::subscriber::set_default(log_sub);

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

        panic!("Failed to validate.\n{}\n{}", res.replace("$DIR/", "/"), file_name.display())
    } else {
        return Some(res);
    }
}

#[testing::fixture("tests/visualize/**/*.ts", exclude(".*\\.\\.d.\\.ts"))]
fn visualize(file_name: PathBuf) {
    let res = run_test(file_name.clone(), false).unwrap();
    res.compare_to_file(&file_name.with_extension("swc-stderr")).unwrap();

    println!("[SUCCESS]{}", file_name.display())
}

#[testing::fixture("tests/pass/**/*.ts", exclude(".*\\.\\.d.\\.ts"))]
fn pass(file_name: PathBuf) {
    let res = run_test(file_name.clone(), false).unwrap();
    println!("TYPES: {}", res);

    run_test(file_name.clone(), true);

    res.compare_to_file(&file_name.with_extension("swc-stderr")).unwrap();

    println!("[SUCCESS]{}", file_name.display())
}
