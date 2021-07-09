#![feature(box_syntax)]

use itertools::Itertools;
use rnode::{NodeIdGenerator, RNode, VisitWith};
use stc_testing::logger;
use stc_ts_ast_rnode::RModule;
use stc_ts_builtin_types::Lib;
use stc_ts_file_analyzer::{
    analyzer::{Analyzer, NoopLoader},
    env::{Env, ModuleConfig},
    Rule,
};
use stc_ts_storage::Single;
use stc_ts_testing::tsc::TscError;
use stc_ts_types::module_id;
use std::{
    path::{Path, PathBuf},
    process::Command,
    sync::Arc,
};
use swc_common::{errors::DiagnosticId, input::SourceFileInput, GLOBALS};
use swc_ecma_ast::EsVersion;
use swc_ecma_parser::{lexer::Lexer, Parser, Syntax, TsConfig};
use swc_ecma_transforms::resolver::ts_resolver;
use swc_ecma_visit::FoldWith;
use testing::{fixture, Tester};

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
struct StcError {
    line: usize,
    code: usize,
}

fn validate(input: &Path) -> Vec<StcError> {
    let tester = Tester::new();
    let diagnostics = tester
        .errors(|cm, handler| {
            let log = logger();

            let fm = cm.load_file(input).unwrap();

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

            let env = Env::simple(
                Rule { ..Default::default() },
                EsVersion::Es2021,
                ModuleConfig::None,
                &libs,
            );

            let generator = module_id::Generator::default();
            let path = Arc::new(input.to_path_buf());

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
            module = module.fold_with(&mut ts_resolver(env.shared().marks().top_level_mark()));
            let module = RModule::from_orig(&mut node_id_gen, module);

            let mut storage = Single {
                parent: None,
                id: generator.generate(&path).1,
                path,
                info: Default::default(),
            };

            {
                // Don't print logs from builtin modules.
                let _tracing = tracing::subscriber::set_default(
                    tracing_subscriber::FmtSubscriber::builder()
                        .without_time()
                        .with_max_level(tracing::Level::TRACE)
                        .with_ansi(true)
                        .with_test_writer()
                        .finish(),
                );

                let mut analyzer =
                    Analyzer::root(log.logger, env.clone(), cm.clone(), box &mut storage, &NoopLoader, None);
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
                .expect("conformance teting: All errors should have proper error code");
            let code = match code {
                DiagnosticId::Error(err) => err,
                DiagnosticId::Lint(lint) => {
                    unreachable!("Unexpected lint '{}' found", lint)
                }
            };

            StcError {
                line: cp.line,
                code: code.strip_prefix("TS").unwrap().parse().unwrap(),
            }
        })
        .collect()
}

/// This invokes `tsc` to get expected result.
#[fixture("tsc/**/*.ts")]
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
    let output = Command::new("tsc")
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

    assert!(stderr.is_empty());

    TscError::parse_all(&stdout)
}
