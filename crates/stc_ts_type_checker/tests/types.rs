#![recursion_limit = "256"]
#![feature(box_syntax)]
#![feature(box_patterns)]
#![feature(test)]

extern crate test;

#[path = "common/mod.rs"]
mod common;

use std::{
    collections::HashSet,
    env,
    fs::read_to_string,
    io,
    io::Write,
    path::{Path, PathBuf},
    sync::{Arc, RwLock},
};

use once_cell::sync::Lazy;
use stc_ts_builtin_types::Lib;
use stc_ts_env::{Env, ModuleConfig, Rule};
use stc_ts_errors::debug::debugger::Debugger;
use stc_ts_file_analyzer::env::EnvFactory;
use stc_ts_module_loader::resolvers::node::NodeResolver;
use stc_ts_testing::tsc::TsTestCase;
use stc_ts_type_checker::Checker;
use stc_ts_utils::StcComments;
use swc_common::{
    errors::{ColorConfig, EmitterWriter, Handler, HandlerFlags},
    input::SourceFileInput,
    FileName, SourceMap, Span,
};
use swc_ecma_ast::EsVersion;
use swc_ecma_parser::{Parser, Syntax, TsConfig};
use swc_ecma_visit::Fold;
use test::test_main;
use testing::{run_test2, NormalizedOutput, StdErr, Tester};

use self::common::load_fixtures;

fn is_ignored(path: &Path) -> bool {
    static IGNORED: Lazy<Vec<String>> = Lazy::new(|| {
        let content = read_to_string("tests/types.ignored.txt").unwrap();

        content
            .lines()
            .filter(|v| !v.trim().is_empty())
            .map(|v| v.to_string())
            .collect()
    });

    static PASS: Lazy<Vec<String>> = Lazy::new(|| {
        let content = read_to_string("tests/types.pass.txt").unwrap();

        content
            .lines()
            .filter(|v| !v.trim().is_empty())
            .map(|v| v.to_string())
            .collect()
    });

    !PASS
        .iter()
        .any(|line| path.to_string_lossy().contains(line))
        || IGNORED
            .iter()
            .any(|line| path.to_string_lossy().contains(line))
}

#[test]
fn types() {
    let args: Vec<_> = env::args().collect();
    let tests = load_fixtures("conformance", |file_name| {
        if is_ignored(&file_name) {
            return None;
        }

        Some(box move || {
            do_test(&file_name).unwrap();
        })
    });
    test_main(&args, tests, Default::default());
}

fn do_test(path: &Path) -> Result<(), StdErr> {
    let str_name = path.display().to_string();

    let (libs, rule, ts_config, target) = ::testing::run_test(false, |cm, handler| {
        let fm = cm.load_file(path).expect("failed to read file");

        Ok({
            // We parse files twice. At first, we read comments and detect
            // configurations for following parse.

            let comments = StcComments::default();

            let mut parser = Parser::new(
                Syntax::Typescript(TsConfig {
                    tsx: str_name.contains("tsx"),
                    ..Default::default()
                }),
                SourceFileInput::from(&*fm),
                Some(&comments),
            );
            let mut target = EsVersion::default();

            let module = parser.parse_module().map_err(|e| {
                e.into_diagnostic(&handler).emit();
                ()
            })?;

            let mut libs = vec![Lib::Es5];
            let mut rule = Rule::default();
            let ts_config = TsConfig::default();

            let span = module.span;
            let cmts = comments.leading.get(&span.lo());
            match cmts {
                Some(ref cmts) => {
                    for cmt in cmts.iter() {
                        let s = cmt.text.trim();
                        if !s.starts_with("@") {
                            continue;
                        }
                        let s = &s[1..]; // '@'

                        if s.starts_with("target:") || s.starts_with("Target:") {
                            let s = s["target:".len()..].trim().to_lowercase();
                            target = match &*s {
                                "es3" => EsVersion::Es3,
                                "es5" => EsVersion::Es5,
                                "es2015" => EsVersion::Es2015,
                                "es6" => EsVersion::Es2015,
                                "es2016" => EsVersion::Es2016,
                                "es2017" => EsVersion::Es2017,
                                "es2018" => EsVersion::Es2018,
                                "es2019" => EsVersion::Es2019,
                                "esnext" => EsVersion::Es2019,
                                _ => unimplemented!("target: {:?}", s),
                            };
                            libs = match target {
                                EsVersion::Es3 | EsVersion::Es5 => vec![Lib::Es5],
                                EsVersion::Es2015 => Lib::load("es2015"),
                                EsVersion::Es2016 => Lib::load("es2016"),
                                EsVersion::Es2017 => Lib::load("es2017"),
                                EsVersion::Es2018 => Lib::load("es2018"),
                                EsVersion::Es2019 => Lib::load("es2019"),
                                EsVersion::Es2020 => Lib::load("es2020"),
                                EsVersion::Es2021 => Lib::load("es2021"),
                                EsVersion::Es2022 => Lib::load("es2022"),
                            };
                        } else if s.starts_with("strict:") {
                            let strict = s["strict:".len()..].trim().parse().unwrap();
                            rule.no_implicit_any = strict;
                            rule.no_implicit_this = strict;
                            rule.always_strict = strict;
                            rule.strict_null_checks = strict;
                            rule.strict_function_types = strict;
                        } else if s.starts_with("noLib:") {
                            let v = s["noLib:".len()..].trim().parse().unwrap();
                            if v {
                                libs = vec![];
                            }
                        } else if s.starts_with("noImplicitAny:") {
                            let v = s["noImplicitAny:".len()..].trim().parse().unwrap();
                            rule.no_implicit_any = v;
                        } else if s.starts_with("noImplicitReturns:") {
                            let v = s["noImplicitReturns:".len()..].trim().parse().unwrap();
                            rule.no_implicit_returns = v;
                        } else if s.starts_with("declaration") {
                        } else if s.starts_with("stripInternal:") {
                            // TODO(kdy1): Handle
                        } else if s.starts_with("traceResolution") {
                            // no-op
                        } else if s.starts_with("allowUnusedLabels:") {
                            let v = s["allowUnusedLabels:".len()..].trim().parse().unwrap();
                            rule.allow_unused_labels = v;
                        } else if s.starts_with("noEmitHelpers") {
                            // TODO
                        } else if s.starts_with("downlevelIteration: ") {
                            // TODO
                        } else if s.starts_with("sourceMap:") || s.starts_with("sourcemap:") {
                            // TODO
                        } else if s.starts_with("isolatedModules:") {
                            // TODO
                        } else if s.starts_with("lib:") {
                            let mut ls = HashSet::<_>::default();
                            for v in s["lib:".len()..].trim().split(",") {
                                ls.extend(Lib::load(v))
                            }
                            libs = ls.into_iter().collect()
                        } else if s.starts_with("allowUnreachableCode:") {
                            let v = s["allowUnreachableCode:".len()..].trim().parse().unwrap();
                            rule.allow_unreachable_code = v;
                        } else if s.starts_with("strictNullChecks:") {
                            let v = s["strictNullChecks:".len()..].trim().parse().unwrap();
                            rule.strict_null_checks = v;
                        } else if s.starts_with("noImplicitThis:") {
                            let v = s["noImplicitThis:".len()..].trim().parse().unwrap();
                            rule.no_implicit_this = v;
                        } else if s.starts_with("skipDefaultLibCheck") {
                            // TODO
                        } else {
                            panic!("Comment is not handled: {}", s);
                        }
                    }
                }
                None => {}
            }

            (libs, rule, ts_config, target)
        })
    })
    .ok()
    .unwrap_or_default();

    let tester = Tester::new();

    let visualized = tester
        .print_errors(|cm, type_info_handler| -> Result<(), _> {
            let (handler_for_errors, error_text) = new_handler(cm.clone());
            let handler_for_errors = Arc::new(handler_for_errors);

            let type_info_handler = Arc::new(type_info_handler);
            let mut checker = Checker::new(
                cm.clone(),
                handler_for_errors.clone(),
                Env::simple(rule, target, ModuleConfig::None, &libs),
                TsConfig {
                    tsx: str_name.contains("tsx"),
                    ..ts_config
                },
                Some(Debugger {
                    cm: cm.clone(),
                    handler: type_info_handler.clone(),
                }),
                Arc::new(NodeResolver),
            );

            checker.check(Arc::new(FileName::Real(path.into())));

            let errors = ::stc_ts_errors::Error::flatten(checker.take_errors());

            checker.run(|| {
                for e in errors {
                    e.emit(&handler_for_errors);
                }
            });

            eprintln!("{}", NormalizedOutput::from(error_text));

            Err(())
        })
        .expect_err("should fail");

    let spec = run_test2(false, |cm, _| {
        let handler = Arc::new(Handler::with_tty_emitter(
            ColorConfig::Always,
            true,
            false,
            Some(cm.clone()),
        ));

        Ok(TsTestCase::parse(
            &cm,
            &handler,
            &PathBuf::from("tests")
                .join("reference")
                .join(path.with_extension("types").file_name().unwrap())
                .canonicalize()
                .unwrap(),
            None,
        )
        .unwrap())
    })
    .unwrap();

    // TODO(kdy1): Match on type data.

    if spec.type_data.is_empty() {
        return Ok(());
    }

    visualized
        .compare_to_file(path.with_extension("stdout"))
        .unwrap();

    Ok(())
}

struct Spanner {
    span: Span,
}

impl Fold for Spanner {
    fn fold_span(&mut self, _: Span) -> Span {
        self.span
    }
}

/// Creates a new handler for testing.
fn new_handler(cm: Arc<SourceMap>) -> (Handler, BufferedError) {
    let buf: BufferedError = Default::default();

    let e = EmitterWriter::new(Box::new(buf.clone()), Some(cm.clone()), false, true);

    let handler = Handler::with_emitter_and_flags(
        Box::new(e),
        HandlerFlags {
            treat_err_as_bug: false,
            can_emit_warnings: true,
            ..Default::default()
        },
    );

    (handler, buf)
}

#[derive(Clone, Default)]
struct BufferedError(Arc<RwLock<Vec<u8>>>);

impl Write for BufferedError {
    fn write(&mut self, d: &[u8]) -> io::Result<usize> {
        self.0.write().unwrap().write(d)
    }

    fn flush(&mut self) -> io::Result<()> {
        Ok(())
    }
}

impl From<BufferedError> for NormalizedOutput {
    fn from(buf: BufferedError) -> Self {
        let s = buf.0.read().unwrap();
        let s: String = String::from_utf8_lossy(&s).into();

        s.into()
    }
}
