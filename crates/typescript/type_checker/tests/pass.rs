#![recursion_limit = "256"]
#![feature(vec_remove_item)]
#![feature(box_syntax)]
#![feature(box_patterns)]
#![feature(test)]

extern crate test;

#[path = "common/mod.rs"]
mod common;

use common::load_fixtures;
use serde::Deserialize;
use stc_testing::logger;
use stc_ts_file_analyzer::env::Env;
use stc_ts_file_analyzer::Lib;
use stc_ts_type_checker::Checker;
use std::{env, path::Path, sync::Arc};
use swc_ecma_parser::{JscTarget, TsConfig};
use test::test_main;
use testing::StdErr;

#[derive(Debug, Clone, PartialEq, Eq, Deserialize)]
struct Error {
    pub line: usize,
    pub column: usize,
    pub msg: String,
}

#[test]
#[ignore = "Not implemented yet"]
fn passes() {
    let args: Vec<_> = env::args().collect();
    let tests = load_fixtures("pass", |file_name| {
        if !file_name.ends_with("index.ts") && !file_name.ends_with("index.tsx") {
            return None;
        }

        Some(box move || {
            do_test(&file_name).unwrap();
        })
    });
    test_main(&args, tests, Default::default());
}

fn do_test(file_name: &Path) -> Result<(), StdErr> {
    let fname = file_name.display().to_string();

    let (libs, rule, ts_config, target) = (
        vec![Lib::Es5],
        Default::default(),
        Default::default(),
        JscTarget::Es5,
    );

    let res = ::testing::run_test2(false, |cm, handler| {
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
        );
        checker.check(Arc::new(file_name.into()));

        let errors = ::stc_ts_file_analyzer::errors::Error::flatten(checker.take_errors());

        checker.run(|| {
            for e in errors {
                e.emit(&handler);
            }
        });

        if handler.has_errors() {
            return Err(());
        }

        Ok(())
    });

    res.expect("should be parsed and validated");

    Ok(())
}
