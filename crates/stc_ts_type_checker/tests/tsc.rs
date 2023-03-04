#![recursion_limit = "256"]
#![feature(box_syntax)]
#![feature(box_patterns)]
#![feature(test)]
#![allow(clippy::if_same_then_else)]
#![allow(clippy::manual_strip)]

extern crate test;

#[path = "common/mod.rs"]
mod common;

use std::{
    env, fs,
    fs::read_to_string,
    mem,
    panic::{catch_unwind, resume_unwind},
    path::{Path, PathBuf},
    sync::Arc,
};

use anyhow::{Context, Error};
use indexmap::IndexMap;
use once_cell::sync::Lazy;
use parking_lot::Mutex;
use serde::{Deserialize, Serialize};
use stc_ts_env::Env;
use stc_ts_file_analyzer::env::EnvFactory;
use stc_ts_module_loader::resolvers::node::NodeResolver;
use stc_ts_testing::{
    conformance::{parse_conformance_test, TestSpec},
    tsc::TscError,
};
use stc_ts_type_checker::{
    loader::{DefaultFileLoader, LoadFile, ModuleLoader},
    Checker,
};
use swc_common::{
    errors::{DiagnosticBuilder, DiagnosticId},
    input::SourceFileInput,
    FileName, SourceFile, SourceMap, Span,
};
use swc_ecma_loader::resolve::Resolve;
use swc_ecma_parser::{Parser, Syntax, TsConfig};
use swc_ecma_visit::Fold;
use test::test_main;
use testing::{StdErr, Tester};

use self::common::load_fixtures;

struct RecordOnPanic {
    stats_file_name: PathBuf,
    stats: Stats,
}

impl Drop for RecordOnPanic {
    fn drop(&mut self) {
        let stats = Stats {
            panic: 1,
            ..self.stats.clone()
        };
        stats.print_to(&self.stats_file_name);
        add_to_total_stats(stats);
    }
}

/// The reference error data.
#[derive(Debug, Clone, PartialEq, Eq, Deserialize, PartialOrd, Ord)]
struct RefError {
    #[serde(default)]
    pub filename: Option<String>,
    pub line: usize,
    pub column: usize,
    pub code: String,
}

#[derive(Debug, Default, Clone)]
struct Stats {
    required_error: usize,
    /// Correct error count.
    matched_error: usize,
    /// False-positive error count.
    extra_error: usize,

    /// Tests failed with panic
    panic: usize,
}

fn is_all_test_enabled() -> bool {
    env::var("TEST").map(|s| s.is_empty()).unwrap_or(false)
}

fn print_matched_errors() -> bool {
    !env::var("DO_NOT_PRINT_MATCHED").map(|s| s == "1").unwrap_or(false)
}

/// Add stats and return total stats.
fn add_to_total_stats(stats: Stats) -> Stats {
    static STATS: Lazy<Mutex<Stats>> = Lazy::new(Default::default);

    if !cfg!(debug_assertions) {
        return stats;
    }

    let mut guard = STATS.lock();
    guard.required_error += stats.required_error;
    guard.matched_error += stats.matched_error;
    guard.extra_error += stats.extra_error;
    guard.panic += stats.panic;

    let stats = (*guard).clone();

    drop(guard);

    // If we are testing everything, update stats file.
    if is_all_test_enabled() {
        stats.print_to(Path::new("tests/tsc-stats.rust-debug"));
    }

    stats
}

/// Returns **path**s (separated by `/`) of tests.
fn load_list(name: &str) -> Vec<String> {
    let content = read_to_string(name).unwrap();

    content
        .lines()
        .map(|v| v.replace("::", "/"))
        .filter(|v| !v.trim().is_empty())
        .collect()
}

fn is_ignored(path: &Path) -> bool {
    static IGNORED: Lazy<Vec<String>> = Lazy::new(|| load_list("tests/tsc.ignored.txt"));

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

    !PASS.iter().any(|line| path.to_string_lossy().ends_with(line))
}

#[test]
fn conformance() {
    let args: Vec<_> = env::args().collect();
    let tests = load_fixtures("conformance", create_test);
    // tests.extend(load_fixtures("compiler", create_test));
    test_main(&args, tests, Default::default());
}

fn is_parser_test(errors: &[TscError]) -> bool {
    for err in errors {
        if 1000 <= err.code && err.code < 2000 {
            return true;
        }
        // These are actually parser test.
        if err.code == 2369 {
            return true;
        }
    }

    false
}

fn create_test(path: PathBuf) -> Option<Box<dyn FnOnce() + Send + Sync>> {
    if is_ignored(&path) {
        return None;
    }

    let specs = catch_unwind(|| parse_conformance_test(&path)).ok()?;
    let use_target = specs.len() > 1;

    if use_target {
        for spec in specs.iter() {
            if is_parser_test(&load_expected_errors(&path, Some(spec)).1) {
                return None;
            }
        }
    } else if is_parser_test(&load_expected_errors(&path, None).1) {
        return None;
    }

    let str_name = path.display().to_string();

    // If parser returns error, ignore it for now.

    let cm = SourceMap::default();
    cm.new_source_file(FileName::Anon, "".into());

    let fm = cm.load_file(&path).unwrap();

    // Ignore parser error tests
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

    Some(box move || {
        let mut last = None;
        for spec in specs {
            let res = catch_unwind(|| {
                do_test(&path, spec, use_target).unwrap();
            });
            if let Err(err) = res {
                last = Some(err);
            }
        }

        if let Some(last) = last {
            resume_unwind(last);
        }
    })
}

/// If `spec` is [Some], it's use to construct filename.
///
/// Returns `(file_suffix, errors)`
fn load_expected_errors(ts_file: &Path, spec: Option<&TestSpec>) -> (String, Vec<TscError>) {
    let errors_file = match spec {
        Some(v) => ts_file.with_file_name(format!(
            "{}{}.errors.json",
            ts_file.file_stem().unwrap().to_string_lossy(),
            v.suffix
        )),
        None => ts_file.with_extension("errors.json"),
    };

    let errors = if !errors_file.exists() {
        println!("errors file does not exists: {}", errors_file.display());
        vec![]
    } else {
        let mut errors: Vec<RefError> = serde_json::from_str(&read_to_string(&errors_file).expect("failed to open errors file"))
            .context("failed to parse errors.txt.json")
            .unwrap();

        errors
            .into_iter()
            .map(|err| {
                let orig_code = err.code.replace("TS", "").parse().expect("failed to parse error code");
                let code = stc_ts_errors::ErrorKind::normalize_error_code(orig_code);

                TscError {
                    file: err.filename,
                    line: err.line,
                    col: err.column,
                    code,
                }
            })
            .collect()
    };

    (
        errors_file.file_name().unwrap().to_string_lossy().replace(".errors.json", ""),
        errors,
    )
}

fn do_test(file_name: &Path, spec: TestSpec, use_target: bool) -> Result<(), StdErr> {
    let (file_stem, mut expected_errors) = load_expected_errors(file_name, if use_target { Some(&spec) } else { None });
    expected_errors.sort();

    let stats_file_name = file_name.with_file_name(format!("{}.stats.rust-debug", file_stem));
    let error_diff_file_name = file_name.with_file_name(format!("{}.error-diff.json", file_stem));

    let TestSpec {
        err_shift_n,
        libs,
        rule,
        target,
        module_config,
        lib_files,
        ..
    } = spec;

    let stat_guard = RecordOnPanic {
        stats_file_name: stats_file_name.clone(),
        stats: Stats {
            required_error: expected_errors.len(),
            ..Default::default()
        },
    };

    let main_src = Arc::new(fs::read_to_string(file_name).unwrap());
    // Postpone multi-file tests.
    if main_src.contains("<reference path") {
        panic!("`<reference path` is not supported yet");
    }

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
            let env = Env::simple(rule, target, module_config, &libs);

            let fs = TestFileSystem {
                files: spec.sub_files.clone(),
            };

            let mut checker = Checker::new(
                cm.clone(),
                handler.clone(),
                env.clone(),
                None,
                ModuleLoader::new(cm, env, fs.clone(), fs),
            );

            // Install a logger
            let _guard = testing::init();

            for lib in lib_files {
                checker.check(Arc::new(FileName::Real(lib.to_path_buf())));
            }

            if spec.sub_files.is_empty() {
                checker.check(Arc::new(FileName::Real(file_name.into())));
            } else if let Some((file_name, ..)) = spec.sub_files.last() {
                checker.check(Arc::new(FileName::Real(file_name.into())));
            }

            let errors = ::stc_ts_errors::ErrorKind::flatten(checker.take_errors());

            for e in errors {
                e.emit(&handler);
            }

            if false {
                return Ok(());
            }

            Err(())
        })
        .expect_err("");

    mem::forget(stat_guard);

    let mut full_actual_errors = diagnostics
        .iter()
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
            }
            .parse()
            .unwrap();

            TscError {
                file: Some(cp.file.name.to_string()),
                line: cp.line,
                col: cp.col.0,
                code,
            }
        })
        .collect::<Vec<_>>();
    full_actual_errors.sort();

    let mut extra_errors = full_actual_errors.clone();

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
            if success || env::var("PRINT_ALL").unwrap_or_default() == "1" || extra_errors.contains(&line_col) {
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

    // Print per-test stats so we can prevent regressions.
    stats.print_to(&stats_file_name);
    add_to_total_stats(stats);

    if env::var("CI").unwrap_or_default() != "1" {
        let mut diff = ErrorDiff::default();

        for err in extra_errors.iter() {
            *diff.extra_errors.entry(err.1.clone()).or_default() += 1;
            diff.extra_error_lines.entry(err.1.clone()).or_default().push(err.0);
        }

        for err in expected_errors.iter() {
            *diff.required_errors.entry(format!("TS{}", err.code)).or_default() += 1;
            diff.required_error_lines
                .entry(format!("TS{}", err.code))
                .or_default()
                .push(err.line);
        }

        if diff.extra_errors.is_empty() && diff.required_errors.is_empty() {
            let _ = fs::remove_file(&error_diff_file_name);
        } else {
            fs::write(&error_diff_file_name, serde_json::to_string_pretty(&diff).unwrap()).expect("failed to write error diff file");
        }
    }

    if print_matched_errors() {
        eprintln!(
            "\n============================================================\n{:?}
============================================================\n{} unmatched errors out of {} errors. Got {} extra errors.\nWanted: \
             {:?}\nUnwanted: {:?}\n\nAll required errors: {:?}\nAll actual errors: {:?}",
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
        eprintln!(
            "\n============================================================\n{:?}
============================================================\n{} unmatched errors out of {} errors. Got {} extra errors.\nWanted: \
             {:?}\nUnwanted: {:?}",
            err,
            expected_errors.len(),
            full_ref_err_cnt,
            extra_err_count,
            expected_errors,
            extra_errors,
        );
    }
    if !success {
        panic!()
    }

    Ok(())
}

impl Stats {
    fn print_to(&self, file_name: &Path) {
        if env::var("CI").unwrap_or_default() == "1" {
            let stat_string = fs::read_to_string(file_name).expect("failed to read test stats file");

            assert_eq!(format!("{:#?}", self), stat_string, "CI=1 so test stats must match");
        } else {
            fs::write(file_name, format!("{:#?}", self)).expect("failed to write test stats");
        }
    }
}

#[derive(Debug, Default, Serialize)]
struct ErrorDiff {
    /// Count by error code
    required_errors: IndexMap<String, usize>,
    required_error_lines: IndexMap<String, Vec<usize>>,
    /// Count by error code
    extra_errors: IndexMap<String, usize>,
    extra_error_lines: IndexMap<String, Vec<usize>>,
}

struct Spanner {
    span: Span,
}

impl Fold for Spanner {
    fn fold_span(&mut self, _: Span) -> Span {
        self.span
    }
}

#[derive(Clone)]
struct TestFileSystem {
    files: Arc<Vec<(String, String)>>,
}

impl Resolve for TestFileSystem {
    fn resolve(&self, base: &FileName, module_specifier: &str) -> Result<FileName, Error> {
        println!("resolve: {:?} {:?}", base, module_specifier);

        if !module_specifier.starts_with('.') {
            return NodeResolver.resolve(base, module_specifier);
        }

        if let Some(name) = module_specifier.strip_prefix("./") {
            return Ok(FileName::Real(name.into()));
        }

        todo!("resolve: current = {:?}; target ={:?}", base, module_specifier);
    }
}

impl LoadFile for TestFileSystem {
    fn load_file(&self, cm: &Arc<SourceMap>, filename: &Arc<FileName>) -> Result<(Arc<SourceFile>, Syntax), Error> {
        println!("load_file: {:?} ", filename);

        if self.files.is_empty() {
            return DefaultFileLoader.load_file(cm, filename);
        }

        for (name, content) in self.files.iter() {
            if filename.to_string() == *name || format!("{}.ts", filename) == *name || format!("{}.tsx", filename) == *name {
                let fm = cm.new_source_file((**filename).clone(), content.clone());

                return Ok((fm, Syntax::Typescript(Default::default())));
            }
        }

        if filename.to_string().ends_with(".d.ts") {
            return DefaultFileLoader.load_file(cm, filename);
        }

        todo!("load_file: {:?} ", filename);
    }
}
