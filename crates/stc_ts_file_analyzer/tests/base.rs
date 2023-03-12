#![feature(box_syntax)]
#![allow(clippy::manual_strip)]

use std::{
    env,
    fs::{self, read_to_string},
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
use stc_ts_errors::{debug::debugger::Debugger, ErrorKind};
use stc_ts_file_analyzer::{
    analyzer::{Analyzer, NoopLoader},
    env::EnvFactory,
    validator::ValidateWith,
};
use stc_ts_storage::{ErrorStore, Single};
use stc_ts_testing::{conformance::parse_conformance_test, tsc::TscError};
use stc_ts_types::module_id;
use stc_ts_utils::StcComments;
use swc_common::{errors::DiagnosticId, input::SourceFileInput, FileName, SyntaxContext};
use swc_ecma_ast::EsVersion;
use swc_ecma_parser::{lexer::Lexer, Parser, Syntax, TsConfig};
use swc_ecma_transforms::resolver;
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

            let (module_id, top_level_mark) = generator.generate(&path);

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
            module = module.fold_with(&mut resolver(env.shared().marks().unresolved_mark(), top_level_mark, true));
            let module = RModule::from_orig(&mut node_id_gen, module);

            let mut storage = Single {
                parent: None,
                id: module_id,
                top_level_ctxt: SyntaxContext::empty().apply_mark(top_level_mark),
                path,
                is_dts: false,
                info: Default::default(),
            };

            {
                // Don't print logs from builtin modules.
                let _tracing = tracing::subscriber::set_default(logger(Level::DEBUG));

                let mut analyzer = Analyzer::root(env, cm, Default::default(), box &mut storage, &NoopLoader, None);
                module.visit_with(&mut analyzer);
            }

            let errors = ::stc_ts_errors::ErrorKind::flatten(storage.info.errors.into_iter().collect());

            for e in errors {
                e.emit(&handler);
            }

            if false {
                return Ok(());
            }

            Err(())
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
    let stderr = run_test(input, true).unwrap();

    if stderr.is_empty() {
        panic!("Expected error, but got none");
    }
}

// This invokes `tsc` to get expected result.
#[fixture("tests/tsc/**/*.ts")]
fn compare(input: PathBuf) {
    let cache_path = input.with_extension("tsc-errors.json");

    let mut actual = validate(&input);
    actual.sort();

    let tsc_result = if !cache_path.is_file() {
        if env::var("STC_SKIP_EXEC").unwrap_or_default() == "1" {
            return;
        }
        let result = invoke_tsc(&input);
        fs::write(&cache_path, serde_json::to_string_pretty(&result).unwrap()).unwrap();
        result
    } else {
        serde_json::from_str(&read_to_string(&cache_path).unwrap()).unwrap()
    };

    let mut expected = tsc_result
        .into_iter()
        .map(|err| StcError {
            line: err.line,
            code: ErrorKind::normalize_error_code(err.code),
        })
        .collect_vec();
    expected.sort();

    assert_eq!(actual, expected);

    testing::unignore_fixture(&input);
}

fn invoke_tsc(input: &Path) -> Vec<TscError> {
    let output = Command::new("npx")
        .arg("tsc")
        .arg("--pretty")
        .arg("--noEmit")
        .arg("--lib")
        .arg("es2020")
        .arg(input)
        .output()
        .expect("failed to invoke tsc");
    let stdout = String::from_utf8_lossy(&output.stdout);
    let stderr = String::from_utf8_lossy(&output.stderr);

    eprintln!("tsc output: \nStdout:\n{}\nStderr:\n{}", stdout, stderr);

    TscError::parse_all(&stdout)
}

/// If `for_error` is false, this function will run as type dump mode.
fn run_test(file_name: PathBuf, want_error: bool) -> Option<NormalizedOutput> {
    let filename = file_name.display().to_string();
    println!("{}", filename);

    for case in parse_conformance_test(&file_name).unwrap() {
        let result = testing::Tester::new()
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

                let env = Env::simple(case.rule, case.target, case.module_config, &libs);
                let stable_env = env.shared().clone();
                let generator = module_id::ModuleIdGenerator::default();
                let path = Arc::new(FileName::Real(file_name.clone()));

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

        if want_error {
            if result.trim().is_empty() {
                return None;
            }

            panic!("Failed to validate")
        } else {
            return Some(result);
        }
    }

    None
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

    {
        let _guard = tracing::subscriber::set_default(tracing::subscriber::NoSubscriber::default());
        run_test(file_name.clone(), true);
    }

    res.compare_to_file(&file_name.with_extension("swc-stderr")).unwrap();

    println!("[SUCCESS]{}", file_name.display())
}

#[fixture("tests/pass-only/**/*.ts")]
fn pass_only(input: PathBuf) {
    for case in parse_conformance_test(&input).unwrap() {
        testing::run_test2(false, |cm, handler| {
            let fm = cm.load_file(&input).unwrap();

            let env = Env::simple(case.rule, case.target, case.module_config, &case.libs);

            let generator = module_id::ModuleIdGenerator::default();
            let path = Arc::new(FileName::Real(input.to_path_buf()));

            let (module_id, top_level_mark) = generator.generate(&path);

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
            module = module.fold_with(&mut resolver(env.shared().marks().unresolved_mark(), top_level_mark, true));
            let module = RModule::from_orig(&mut node_id_gen, module);

            let mut storage = Single {
                parent: None,
                id: module_id,
                top_level_ctxt: SyntaxContext::empty().apply_mark(top_level_mark),
                path,
                is_dts: false,
                info: Default::default(),
            };

            {
                // Don't print logs from builtin modules.
                let _tracing = tracing::subscriber::set_default(logger(Level::DEBUG));

                let mut analyzer = Analyzer::root(env, cm, Default::default(), box &mut storage, &NoopLoader, None);
                module.visit_with(&mut analyzer);
            }

            let errors = ::stc_ts_errors::ErrorKind::flatten(storage.info.errors.into_iter().collect());
            let ok = errors.is_empty();

            for e in &errors {
                e.emit(&handler);
            }

            if !ok {
                panic!()
            }

            Ok(())
        })
        .unwrap();
    }

    testing::unignore_fixture(&input);
}
