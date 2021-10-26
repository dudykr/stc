#![recursion_limit = "256"]
#![feature(box_syntax)]
#![feature(box_patterns)]
#![feature(test)]

extern crate test;

#[path = "common/mod.rs"]
mod common;

use self::common::load_fixtures;
use anyhow::{Context, Error};
use once_cell::sync::Lazy;
use parking_lot::Mutex;
use serde::Deserialize;
use stc_testing::init_tracing;
use stc_ts_builtin_types::Lib;
use stc_ts_env::{Env, ModuleConfig, Rule};
use stc_ts_file_analyzer::env::EnvFactory;
use stc_ts_module_loader::resolver::node::NodeResolver;
use stc_ts_type_checker::Checker;
use stc_ts_utils::StcComments;
use std::{
    collections::HashSet,
    env, fs,
    fs::{read_to_string, File},
    mem,
    panic::catch_unwind,
    path::{Path, PathBuf},
    sync::Arc,
    time::{Duration, Instant},
};
use swc_common::{
    errors::{DiagnosticBuilder, DiagnosticId},
    input::SourceFileInput,
    BytePos, SourceMap, Span, Spanned,
};
use swc_ecma_ast::{EsVersion, Program};
use swc_ecma_parser::{JscTarget, Parser, Syntax, TsConfig};
use swc_ecma_visit::Fold;
use test::test_main;
use testing::{StdErr, Tester};

struct RecordOnPanic {
    stats: Stats,
}

impl Drop for RecordOnPanic {
    fn drop(&mut self) {
        record_stat(self.stats.clone());
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Deserialize)]
struct RefError {
    pub line: usize,
    pub column: usize,
    pub code: String,
}

#[derive(Debug, Default, Clone)]
struct Stats {
    required_error: usize,
    matched_error: usize,
    extra_error: usize,
}

fn is_all_test_enabled() -> bool {
    env::var("TEST").map(|s| s == "").unwrap_or(false)
}

fn print_matched_errors() -> bool {
    !env::var("DONT_PRINT_MATCHED").map(|s| s == "1").unwrap_or(false)
}

fn record_time(line_count: usize, time_of_check: Duration, full_time: Duration) {
    static TOTAL_CHECK: Lazy<Mutex<Duration>> = Lazy::new(|| Default::default());
    static TOTAL_FULL: Lazy<Mutex<Duration>> = Lazy::new(|| Default::default());
    static LINES: Lazy<Mutex<usize>> = Lazy::new(|| Default::default());

    if cfg!(debug_assertions) {
        return;
    }

    let time_of_check = {
        let mut guard = TOTAL_CHECK.lock();
        *guard += time_of_check;
        *guard
    };
    let full_time = {
        let mut guard = TOTAL_FULL.lock();
        *guard += full_time;
        *guard
    };

    let line_count = {
        let mut guard = LINES.lock();
        *guard += line_count;
        *guard
    };

    let content = format!(
        "{:#?}",
        Timings {
            lines: line_count,
            check_time: time_of_check,
            full_time
        }
    );

    #[derive(Debug)]
    struct Timings {
        lines: usize,
        check_time: Duration,
        full_time: Duration,
    }

    // If we are testing everything, update stats file.
    if is_all_test_enabled() {
        fs::write("tests/tsc.timings.rust-debug", &content).unwrap();
    }
}

/// Add stats and return total stats.
fn record_stat(stats: Stats) -> Stats {
    static STATS: Lazy<Mutex<Stats>> = Lazy::new(|| Default::default());

    if !cfg!(debug_assertions) {
        return stats;
    }

    let mut guard = STATS.lock();
    guard.required_error += stats.required_error;
    guard.matched_error += stats.matched_error;
    guard.extra_error += stats.extra_error;

    let stats = (*guard).clone();

    let content = format!("{:#?}", stats);

    if env::var("WIP_STATS").unwrap_or_default() == "1" && env::var("STC_IGNORE_WIP").unwrap_or_default() != "1" {
        fs::write("tests/wip-stats.rust-debug", &content).unwrap();
    }

    // If we are testing everything, update stats file.
    if is_all_test_enabled() {
        fs::write("tests/tsc-stats.rust-debug", &content).unwrap();
    }

    stats
}

/// Retunrs **path**s (separated by `/`) of tests.
fn load_list(name: &str) -> Vec<String> {
    let content = read_to_string(name).unwrap();

    content
        .lines()
        .map(|v| v.replace("::", "/"))
        .filter(|v| !v.trim().is_empty())
        .collect()
}

fn is_ignored(path: &Path) -> bool {
    static IGNORED: Lazy<Vec<String>> = Lazy::new(|| {
        let mut v = load_list("tests/tsc.ignored.txt");

        v.extend(load_list("tests/tsc.multiresult.txt"));

        v
    });

    static PASS: Lazy<Vec<String>> = Lazy::new(|| {
        let mut v = load_list("tests/conformance.pass.txt");
        v.extend(load_list("tests/compiler.pass.txt"));
        if env::var("STC_IGNORE_WIP").unwrap_or_default() != "1" {
            v.extend(load_list("tests/tsc.wip.txt"));
        }
        v
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
    let tests = load_fixtures("conformance", create_test);
    // tests.extend(load_fixtures("compiler", create_test));
    test_main(&args, tests, Default::default());
}

fn create_test(path: PathBuf) -> Option<Box<dyn FnOnce() + Send + Sync>> {
    if is_ignored(&path) {
        return None;
    }

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

    let str_name = path.display().to_string();

    // If parser returns error, ignore it for now.

    let cm = SourceMap::default();
    let fm = cm.load_file(&path).unwrap();

    // Postpone multi-file tests.
    if fm.src.to_lowercase().contains("@filename") || fm.src.contains("<reference path") {
        if is_all_test_enabled() {
            record_stat(Stats {
                required_error: load_expected_errors(&path).map(|v| v.len()).unwrap_or_default(),
                ..Default::default()
            });
        }

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
        parser.parse_program().ok()
    })
    .ok()??;

    Some(box move || {
        do_test(&path).unwrap();
    })
}

fn load_expected_errors(ts_file: &Path) -> Result<Vec<RefError>, Error> {
    let errors_file = ts_file.with_extension("errors.json");
    if !errors_file.exists() {
        println!("errors file does not exists: {}", errors_file.display());
        Ok(vec![])
    } else {
        let mut errors: Vec<RefError> =
            serde_json::from_reader(File::open(errors_file).expect("failed to open error sfile"))
                .context("failed to parse errors.txt.json")?;

        for err in &mut errors {
            let orig_code = err.code.replace("TS", "").parse().expect("failed to parse error code");
            let code = stc_ts_errors::Error::normalize_error_code(orig_code);

            if orig_code != code {
                err.code = format!("TS{}", code);
            }
        }

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
    module_config: ModuleConfig,
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

        let comments = StcComments::default();

        let mut parser = Parser::new(
            Syntax::Typescript(TsConfig {
                tsx: fname.contains("tsx"),
                ..Default::default()
            }),
            SourceFileInput::from(&*fm),
            Some(&comments),
        );
        let mut targets = vec![(JscTarget::default(), false)];

        let program = parser.parse_program().map_err(|e| {
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

        match &program {
            Program::Module(v) => {
                if !v.body.is_empty() {
                    first_stmt_line = cm.lookup_line(v.body[0].span().lo).unwrap().line;
                }
            }
            Program::Script(v) => {
                if !v.body.is_empty() {
                    first_stmt_line = cm.lookup_line(v.body[0].span().lo).unwrap().line;
                }
            }
        }

        let mut libs = vec![Lib::Es5, Lib::Dom];
        let mut rule = Rule {
            allow_unreachable_code: false,
            ..Default::default()
        };
        let mut module_config = ModuleConfig::None;
        let ts_config = TsConfig::default();

        let mut had_comment = false;

        let span = program.span();
        let cmts = comments.leading.get(&span.lo());
        match cmts {
            Some(ref cmts) => {
                let directive_start = cmts
                    .iter()
                    .position(|cmt| cmt.text.trim().starts_with("@"))
                    .unwrap_or(0);
                let cmt_start_line = if directive_start == 0 {
                    0
                } else {
                    cmts.iter()
                        .find(|cmt| cmt.text.trim().starts_with("@"))
                        .map(|cmt| cm.lookup_char_pos(cmt.span.hi).line)
                        .unwrap_or(0)
                };

                for cmt in cmts.iter().skip(directive_start) {
                    let s = cmt.text.trim();
                    if !s.starts_with("@") {
                        if had_comment {
                            err_shift_n = cm.lookup_char_pos(cmt.span.hi).line - 1 - cmt_start_line;
                            break;
                        }
                        continue;
                    }
                    had_comment = true;
                    err_shift_n = cm.lookup_char_pos(cmt.span.hi + BytePos(1)).line - cmt_start_line;
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
                        let s = s["lib:".len()..].trim();
                        let mut ls = HashSet::<_>::default();
                        for v in s.split(",") {
                            ls.extend(Lib::load(&v.to_lowercase().replace("es6", "es2015")))
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
                    } else if s.starts_with("suppressImplicitAnyIndexErrors:") {
                        // TODO
                        let v = s["suppressImplicitAnyIndexErrors:".len()..].trim().parse().unwrap();
                        rule.suppress_implicit_any_index_errors = v;
                    } else if s.starts_with("module:") {
                        let v = s["module:".len()..].trim().parse().unwrap();
                        module_config = v;
                    } else if s.to_lowercase().starts_with("notypesandsymbols") {
                        // Ignored as we don't generate them.
                    } else if s.to_lowercase().starts_with("usedefineforclassfields") {
                        rule.use_define_property_for_class_fields = true;
                    } else if s.to_lowercase().starts_with("noemit")
                        || s.to_lowercase().starts_with("jsx")
                        || s.to_lowercase().starts_with("preserveconstenums")
                    {
                        // Ignored as we only checks type.
                    } else if s.starts_with("strict") {
                        let strict = true;
                        rule.no_implicit_any = strict;
                        rule.no_implicit_this = strict;
                        rule.always_strict = strict;
                        rule.strict_null_checks = strict;
                        rule.strict_function_types = strict;
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
                        JscTarget::Es2015 => Lib::load("es2015.full"),
                        JscTarget::Es2016 => Lib::load("es2016.full"),
                        JscTarget::Es2017 => Lib::load("es2017.full"),
                        JscTarget::Es2018 => Lib::load("es2018.full"),
                        JscTarget::Es2019 => Lib::load("es2019.full"),
                        JscTarget::Es2020 => Lib::load("es2020.full"),
                        JscTarget::Es2021 => Lib::load("es2021.full"),
                    }
                } else {
                    if specified {
                        libs_with_deps(&libs)
                    } else {
                        libs.clone()
                    }
                };

                TestSpec {
                    err_shift_n,
                    libs,
                    rule,
                    ts_config,
                    target,
                    module_config,
                }
            })
            .collect())
    })
    .ok()
    .unwrap()
}

fn do_test(file_name: &Path) -> Result<(), StdErr> {
    let file_stem = file_name.file_stem().unwrap();
    let fname = file_name.display().to_string();
    let mut expected_errors = load_expected_errors(&file_name).unwrap();

    let specs = parse_test(file_name);

    for TestSpec {
        err_shift_n,
        libs,
        rule,
        ts_config,
        target,
        module_config,
    } in specs
    {
        let mut time_of_check = Duration::new(0, 0);
        let mut full_time = Duration::new(0, 0);

        let stat_guard = RecordOnPanic {
            stats: Stats {
                required_error: expected_errors.len(),
                ..Default::default()
            },
        };

        let mut stats = Stats::default();
        dbg!(&libs);
        for err in &mut expected_errors {
            // This error use special span.
            if err.line == 0 {
                continue;
            }
            // Typescript conformance test remove lines starting with @-directives.
            err.line += err_shift_n;
        }

        let full_ref_errors = expected_errors.clone();
        let full_ref_err_cnt = full_ref_errors.len();

        let tester = Tester::new();
        let diagnostics = tester
            .errors(|cm, handler| {
                let handler = Arc::new(handler);
                let mut checker = Checker::new(
                    cm.clone(),
                    handler.clone(),
                    Env::simple(rule, target, module_config, &libs),
                    TsConfig {
                        tsx: fname.contains("tsx"),
                        ..ts_config
                    },
                    None,
                    Arc::new(NodeResolver),
                );

                // Install a new OpenTelemetry trace pipeline
                let _guard = init_tracing(file_stem.to_string_lossy().to_string());

                let start = Instant::now();

                checker.check(Arc::new(file_name.into()));

                let end = Instant::now();

                time_of_check = end - start;

                let errors = ::stc_ts_errors::Error::flatten(checker.take_errors());

                checker.run(|| {
                    for e in errors {
                        e.emit(&handler);
                    }
                });

                let end = Instant::now();

                full_time = end - start;

                if false {
                    return Ok(());
                }

                return Err(());
            })
            .expect_err("");

        mem::forget(stat_guard);

        if !cfg!(debug_assertions) {
            let line_cnt = {
                let content = fs::read_to_string(&file_name).unwrap();

                content.lines().count()
            };
            record_time(line_cnt, time_of_check, full_time);

            // if time > Duration::new(0, 500_000_000) {
            //     let _ = fs::write(file_name.with_extension("timings.txt"),
            // format!("{:?}", time)); }
        }

        let mut extra_errors = diagnostics
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

        let full_actual_errors = extra_errors.clone();

        for (line, error_code) in full_actual_errors.clone() {
            if let Some(idx) = expected_errors
                .iter()
                .position(|err| (err.line == line || err.line == 0) && err.code == error_code)
            {
                stats.matched_error += 1;

                let is_zero_line = expected_errors[idx].line == 0;
                expected_errors.remove(idx);
                if let Some(idx) = extra_errors
                    .iter()
                    .position(|(r_line, r_code)| (line == *r_line || is_zero_line) && error_code == *r_code)
                {
                    extra_errors.remove(idx);
                }
            }
        }

        //
        //      - All reference errors are matched
        //      - Actual errors does not remain
        let success = expected_errors.is_empty() && extra_errors.is_empty();

        let res: Result<(), _> = tester.print_errors(|_, handler| {
            // If we failed, we only emit errors which has wrong line.

            for (d, line_col) in diagnostics.into_iter().zip(full_actual_errors.clone()) {
                if success
                    || env::var("PRINT_ALL").unwrap_or(String::from("")) == "1"
                    || extra_errors.contains(&line_col)
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

        let extra_err_count = extra_errors.len();
        stats.required_error += expected_errors.len();
        stats.extra_error += extra_err_count;

        let stats = record_stat(stats);

        if cfg!(debug_assertions) {
            println!("[STATS] {:#?}", stats);

            if expected_errors.is_empty() {
                println!("[REMOVE_ONLY]{}", file_name.display());
            }
        }

        if extra_errors.len() == expected_errors.len() {
            let expected_lines = expected_errors.iter().map(|v| v.line).collect::<Vec<_>>();
            let extra_lines = extra_errors.iter().map(|(v, _)| *v).collect::<Vec<_>>();

            if expected_lines == extra_lines {
                println!("[ERROR_CODE_ONLY]{}", file_name.display());
            }
        }

        if !success {
            if print_matched_errors() {
                panic!(
                    "\n============================================================\n{:?}
    ============================================================\n{} unmatched errors out of {} errors. Got {} extra \
                     errors.\nWanted: {:?}\nUnwanted: {:?}\n\nAll required errors: {:?}\nAll actual errors: {:?}",
                    err,
                    expected_errors.len(),
                    full_ref_err_cnt,
                    extra_err_count,
                    expected_errors,
                    extra_errors,
                    full_ref_errors,
                    full_actual_errors,
                );
            } else {
                panic!(
                    "\n============================================================\n{:?}
    ============================================================\n{} unmatched errors out of {} errors. Got {} extra \
                     errors.\nWanted: {:?}\nUnwanted: {:?}",
                    err,
                    expected_errors.len(),
                    full_ref_err_cnt,
                    extra_err_count,
                    expected_errors,
                    extra_errors,
                );
            }
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

fn libs_with_deps(libs: &[Lib]) -> Vec<Lib> {
    fn add(libs: &mut Vec<Lib>, l: Lib) {
        if libs.contains(&l) {
            return;
        }
        libs.push(l);

        match l {
            Lib::Es5 | Lib::Es5Full => {}
            Lib::Es2015
            | Lib::Es2015Core
            | Lib::Es2015Full
            | Lib::Es2015Collection
            | Lib::Es2015SymbolWellknown
            | Lib::Es2015Generator
            | Lib::Es2015Iterable
            | Lib::Es2015Promise
            | Lib::Es2015Proxy
            | Lib::Es2015Reflect
            | Lib::Es2015Symbol => add(libs, Lib::Es5Full),
            Lib::Es2016 | Lib::Es2016ArrayInclude | Lib::Es2016Full => add(libs, Lib::Es2015Full),
            Lib::Es2017
            | Lib::Es2017Sharedmemory
            | Lib::Es2017Full
            | Lib::Es2017Intl
            | Lib::Es2017Object
            | Lib::Es2017String
            | Lib::Es2017Typedarrays => add(libs, Lib::Es2016Full),
            Lib::Es2018
            | Lib::Es2018Asyncgenerator
            | Lib::Es2018Asynciterable
            | Lib::Es2018Intl
            | Lib::Es2018Promise
            | Lib::Es2018Regexp
            | Lib::Es2018Full => add(libs, Lib::Es2017Full),
            Lib::Es2019
            | Lib::Es2019Array
            | Lib::Es2019Full
            | Lib::Es2019Object
            | Lib::Es2019String
            | Lib::Es2019Symbol => add(libs, Lib::Es2018Full),
            Lib::Es2020
            | Lib::Es2020Bigint
            | Lib::Es2020Full
            | Lib::Es2020Intl
            | Lib::Es2020Promise
            | Lib::Es2020Sharedmemory
            | Lib::Es2020String
            | Lib::Es2020SymbolWellknown => add(libs, Lib::Es2019Full),

            Lib::Esnext
            | Lib::EsnextIntl
            | Lib::EsnextPromise
            | Lib::EsnextString
            | Lib::EsnextWeakref
            | Lib::EsnextFull => add(libs, Lib::Es2020Full),

            Lib::Dom
            | Lib::DomIterable
            | Lib::DomIterableGenerated
            | Lib::Header
            | Lib::Scripthost
            | Lib::WebworkerGenerated
            | Lib::WebworkerImportscripts
            | Lib::WebworkerIterableGenerated => {}
        }

        for l in l.deps() {
            add(libs, l);
        }
    }

    let mut all = vec![];

    for lib in libs {
        add(&mut all, *lib);
    }

    all.sort();
    all.dedup();

    all
}
