#![recursion_limit = "256"]
#![feature(vec_remove_item)]
#![feature(box_syntax)]
#![feature(box_patterns)]
#![feature(test)]

extern crate test;

#[path = "common/mod.rs"]
mod common;

use self::common::load_fixtures;
use self::common::SwcComments;
use anyhow::Context;
use anyhow::Error;
use once_cell::sync::Lazy;
use serde::Deserialize;
use stc_testing::logger;
use stc_ts_builtin_types::Lib;
use stc_ts_file_analyzer::env::Env;
use stc_ts_file_analyzer::Rule;
use stc_ts_module_loader::resolver::node::NodeResolver;
use stc_ts_type_checker::Checker;
use std::collections::HashSet;
use std::env;
use std::fs::read_to_string;
use std::fs::File;
use std::panic::catch_unwind;
use std::path::Path;
use std::sync::Arc;
use swc_common::errors::DiagnosticBuilder;
use swc_common::errors::DiagnosticId;
use swc_common::input::SourceFileInput;
use swc_common::BytePos;
use swc_common::SourceMap;
use swc_common::Span;
use swc_common::Spanned;
use swc_ecma_ast::EsVersion;
use swc_ecma_parser::JscTarget;
use swc_ecma_parser::Parser;
use swc_ecma_parser::Syntax;
use swc_ecma_parser::TsConfig;
use swc_ecma_visit::Fold;
use test::test_main;
use testing::StdErr;
use testing::Tester;

#[derive(Debug, Clone, PartialEq, Eq, Deserialize)]
struct RefError {
    pub line: usize,
    pub column: usize,
    pub code: String,
}

fn is_ignored(path: &Path) -> bool {
    static IGNORED: Lazy<Vec<String>> = Lazy::new(|| {
        let content = read_to_string("tests/conformance.ignored.txt").unwrap();

        content
            .lines()
            .filter(|v| !v.trim().is_empty())
            .map(|v| v.to_string())
            .collect()
    });

    static PASS: Lazy<Vec<String>> = Lazy::new(|| {
        let content = read_to_string("tests/conformance.pass.txt").unwrap();

        content
            .lines()
            .filter(|v| !v.trim().is_empty())
            .map(|v| v.to_string())
            .collect()
    });

    if IGNORED.iter().any(|line| path.to_string_lossy().contains(line)) {
        return true;
    }

    if let Ok(test) = env::var("TEST") {
        return !path.to_string_lossy().contains(&test);
    }

    !PASS.iter().any(|line| path.to_string_lossy().contains(line))
}

#[test]
fn conformance() {
    let args: Vec<_> = env::args().collect();
    let tests = load_fixtures("conformance", |path| {
        if is_ignored(&path) {
            return None;
        }

        let str_name = path.display().to_string();

        // If parser returns error, ignore it for now.

        let cm = SourceMap::default();
        let fm = cm.load_file(&path).unwrap();

        // Postpone multi-file tests.
        if fm.src.to_lowercase().contains("@filename") || fm.src.contains("<reference path") {
            return None;
        }

        catch_unwind(|| {
            let mut parser = Parser::new(
                Syntax::Typescript(TsConfig {
                    tsx: str_name.contains("tsx"),
                    ..Default::default()
                }),
                SourceFileInput::from(&*fm),
                None,
            );
            parser.parse_module().ok()
        })
        .ok()??;

        if let Ok(errors) = load_expected_errors(&path) {
            for err in errors {
                if err.code.starts_with("TS1") && err.code.len() == 6 {
                    return None;
                }

                // These are actually parser test.
                match &*err.code {
                    "TS2369" => return None,
                    _ => {}
                }
            }
        }

        Some(box move || {
            do_test(&path).unwrap();
        })
    });
    test_main(&args, tests, Default::default());
}

fn load_expected_errors(ts_file: &Path) -> Result<Vec<RefError>, Error> {
    let errors_file = ts_file.with_extension("errors.json");
    if !errors_file.exists() {
        println!("errors file does not exists: {}", errors_file.display());
        Ok(vec![])
    } else {
        let errors: Vec<RefError> =
            serde_json::from_reader(File::open(errors_file).expect("failed to open error sfile"))
                .context("failed to parse errors.txt.json")?;

        // TODO: Match column and message

        Ok(errors)
    }
}

struct TestSpec {
    err_shift_n: usize,
    libs: Vec<Lib>,
    rule: Rule,
    ts_config: TsConfig,
    target: EsVersion,
}

fn parse_targets(s: &str) -> Vec<EsVersion> {
    match s {
        "es3" => return vec![JscTarget::Es3],
        "es5" => return vec![JscTarget::Es5],
        "es2015" => return vec![JscTarget::Es2015],
        "es6" => return vec![JscTarget::Es2015],
        "es2016" => return vec![JscTarget::Es2016],
        "es2017" => return vec![JscTarget::Es2017],
        "es2018" => return vec![JscTarget::Es2018],
        "es2019" => return vec![JscTarget::Es2019],
        "es2020" => return vec![JscTarget::Es2020],
        "esnext" => return vec![JscTarget::Es2020],
        _ => {}
    }
    if !s.contains(",") {
        panic!("failed to parse `{}` as targets", s)
    }
    s.split(",").map(|s| s.trim()).flat_map(parse_targets).collect()
}

fn parse_test(file_name: &Path) -> Vec<TestSpec> {
    let mut err_shift_n = 0;
    let mut first_stmt_line = 0;

    let fname = file_name.to_string_lossy();
    ::testing::run_test(false, |cm, handler| {
        let fm = cm.load_file(file_name).expect("failed to read file");

        // We parse files twice. At first, we read comments and detect
        // configurations for following parse.

        let comments = SwcComments::default();

        let mut parser = Parser::new(
            Syntax::Typescript(TsConfig {
                tsx: fname.contains("tsx"),
                ..Default::default()
            }),
            SourceFileInput::from(&*fm),
            Some(&comments),
        );
        let mut targets = vec![(JscTarget::default(), false)];

        let module = parser.parse_module().map_err(|e| {
            e.into_diagnostic(&handler).emit();
            ()
        })?;

        for line in fm.src.lines() {
            if line.is_empty() {
                err_shift_n += 1;
            } else {
                break;
            }
        }

        if !module.body.is_empty() {
            first_stmt_line = cm.lookup_line(module.body[0].span().lo).unwrap().line;
        }

        let mut libs = vec![Lib::Es5, Lib::Dom];
        let mut rule = Rule::default();
        let ts_config = TsConfig::default();

        let mut had_comment = false;

        let span = module.span;
        let cmts = comments.leading.get(&span.lo());
        match cmts {
            Some(ref cmts) => {
                for cmt in cmts.iter() {
                    let s = cmt.text.trim();
                    if !s.starts_with("@") {
                        if had_comment {
                            err_shift_n = cm.lookup_char_pos(cmt.span.hi).line - 1;
                            break;
                        }
                        continue;
                    }
                    had_comment = true;
                    err_shift_n = cm.lookup_char_pos(cmt.span.hi + BytePos(1)).line;
                    let s = &s[1..]; // '@'

                    if s.starts_with("target:") || s.starts_with("Target:") {
                        let s = s["target:".len()..].trim().to_lowercase();
                        targets = parse_targets(&s).into_iter().map(|v| (v, true)).collect();
                    } else if s.starts_with("strict:") {
                        let strict = s["strict:".len()..].trim().parse().unwrap();
                        rule.no_implicit_any = strict;
                        rule.no_implicit_this = strict;
                        rule.always_strict = strict;
                        rule.strict_null_checks = strict;
                        rule.strict_function_types = strict;
                    } else if s.starts_with("strict") {
                        let strict = true;
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
                    } else if s.to_lowercase().starts_with("noimplicitany:") {
                        let v = s["noImplicitAny:".len()..].trim().parse().unwrap();
                        rule.no_implicit_any = v;
                    } else if s.starts_with("noImplicitReturns:") {
                        let v = s["noImplicitReturns:".len()..].trim().parse().unwrap();
                        rule.no_implicit_returns = v;
                    } else if s.starts_with("declaration") {
                    } else if s.starts_with("stripInternal:") {
                        // TODO: Handle
                    } else if s.starts_with("traceResolution") {
                        // no-op
                    } else if s.starts_with("allowUnusedLabels:") {
                        let v = s["allowUnusedLabels:".len()..].trim().parse().unwrap();
                        rule.allow_unused_labels = v;
                    } else if s.starts_with("noEmitHelpers") {
                        // TODO
                    } else if s.starts_with("downlevelIteration:") {
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
                    } else if s.starts_with("module") {
                    } else if s.to_lowercase().starts_with("notypesandsymbols") {
                        // Ignored as we don't generate them.
                    } else if s.to_lowercase().starts_with("noemit")
                        || s.to_lowercase().starts_with("jsx")
                        || s.to_lowercase().starts_with("usedefineforclassfields")
                        || s.to_lowercase().starts_with("preserveconstenums")
                    {
                        // Ignored as we only checks type.
                    } else {
                        panic!("Comment is not handled: {}", s);
                    }
                }
            }
            None => {}
        }

        libs.sort();

        err_shift_n = err_shift_n.min(first_stmt_line);
        dbg!(err_shift_n);

        Ok(targets
            .into_iter()
            .map(|(target, specified)| {
                let libs = if specified && libs == vec![Lib::Es5, Lib::Dom] {
                    match target {
                        JscTarget::Es3 | JscTarget::Es5 => vec![Lib::Es5],
                        JscTarget::Es2015 => Lib::load("es2015"),
                        JscTarget::Es2016 => Lib::load("es2016"),
                        JscTarget::Es2017 => Lib::load("es2017"),
                        JscTarget::Es2018 => Lib::load("es2018"),
                        JscTarget::Es2019 => Lib::load("es2019"),
                        JscTarget::Es2020 => Lib::load("es2020"),
                    }
                } else {
                    libs.clone()
                };

                TestSpec {
                    err_shift_n,
                    ts_config,
                    libs,
                    rule,
                    target,
                }
            })
            .collect())
    })
    .ok()
    .unwrap()
}

fn do_test(file_name: &Path) -> Result<(), StdErr> {
    let fname = file_name.display().to_string();
    let mut expected_errors = load_expected_errors(&file_name).unwrap();

    let specs = parse_test(file_name);

    for TestSpec {
        err_shift_n,
        libs,
        rule,
        ts_config,
        target,
    } in specs
    {
        for err in &mut expected_errors {
            // Typescript conformance test remove lines starting with @-directives.
            err.line += err_shift_n;
        }

        let full_ref_errors = expected_errors.clone();
        let full_ref_err_cnt = full_ref_errors.len();

        let tester = Tester::new();
        let diagnostics = tester
            .errors(|cm, handler| {
                let log = logger();
                let handler = Arc::new(handler);
                let mut checker = Checker::new(
                    log.logger,
                    cm.clone(),
                    handler.clone(),
                    Env::simple(rule, target, &libs),
                    TsConfig {
                        tsx: fname.contains("tsx"),
                        ..ts_config
                    },
                    None,
                    Arc::new(NodeResolver),
                );

                checker.check(Arc::new(file_name.into()));

                let errors = ::stc_ts_errors::Error::flatten(checker.take_errors());

                checker.run(|| {
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

        let mut actual_errors = diagnostics
            .iter()
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

                (cp.line, code)
            })
            .collect::<Vec<_>>();

        let full_actual_errors = actual_errors.clone();

        for (line, error_code) in full_actual_errors.clone() {
            if let Some(idx) = expected_errors
                .iter()
                .position(|err| err.line == line && err.code == error_code)
            {
                expected_errors.remove(idx);
                if let Some(idx) = actual_errors
                    .iter()
                    .position(|(r_line, r_code)| line == *r_line && error_code == *r_code)
                {
                    actual_errors.remove(idx);
                }
            }
        }

        //
        //      - All reference errors are matched
        //      - Actual errors does not remain
        let success = expected_errors.is_empty() && actual_errors.is_empty();

        let res: Result<(), _> = tester.print_errors(|_, handler| {
            // If we failed, we only emit errors which has wrong line.

            for (d, line_col) in diagnostics.into_iter().zip(full_actual_errors.clone()) {
                if success
                    || env::var("PRINT_ALL").unwrap_or(String::from("")) == "1"
                    || actual_errors.contains(&line_col)
                {
                    DiagnosticBuilder::new_diagnostic(&handler, d).emit();
                }
            }

            Err(())
        });

        let err = match res {
            Ok(_) => StdErr::from(String::from("")),
            Err(err) => err,
        };

        let err_count = actual_errors.len();

        if full_ref_errors.is_empty() {
            println!("[INFER_ONLY]{}", file_name.display());
        }

        if !success {
            panic!(
                "\n============================================================\n{:?}
============================================================\n{} unmatched errors out of {} errors. Got {} extra \
                 errors.\nWanted: {:?}\nUnwanted: {:?}\n\nAll required errors: {:?}\nAll actual errors: {:?}",
                err,
                expected_errors.len(),
                full_ref_err_cnt,
                err_count,
                expected_errors,
                actual_errors,
                full_ref_errors,
                full_actual_errors,
            );
        }
    }

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
