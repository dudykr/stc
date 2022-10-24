#![recursion_limit = "256"]
#![feature(box_syntax)]
#![feature(box_patterns)]
#![feature(test)]

extern crate test;

#[path = "common/mod.rs"]
mod common;

use std::{env, path::Path, sync::Arc};

use serde::Deserialize;
use stc_ts_builtin_types::Lib;
use stc_ts_env::{Env, ModuleConfig};
use stc_ts_file_analyzer::env::EnvFactory;
use stc_ts_module_loader::resolvers::node::NodeResolver;
use stc_ts_type_checker::Checker;
use swc_common::FileName;
use swc_ecma_ast::EsVersion;
use swc_ecma_parser::TsConfig;
use test::test_main;
use testing::StdErr;

use self::common::load_fixtures;

#[derive(Debug, Clone, PartialEq, Eq, Deserialize)]
struct Error {
    pub line: usize,
    pub column: usize,
    pub msg: String,
}

#[test]
#[ignore]
fn errors() {
    let args: Vec<_> = env::args().collect();
    let tests = load_fixtures("errors", |file_name| {
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
        EsVersion::Es5,
    );

    let res = ::testing::run_test2(false, |cm, handler| {
        let handler = Arc::new(handler);
        let mut checker = Checker::new(
            cm.clone(),
            handler.clone(),
            Env::simple(rule, target, ModuleConfig::None, &libs),
            TsConfig {
                tsx: fname.contains("tsx"),
                ..ts_config
            },
            None,
            Arc::new(NodeResolver),
        );
        checker.check(Arc::new(FileName::Real(file_name.into())));
        let errors = ::stc_ts_errors::Error::flatten(checker.take_errors());

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

    let err = res.expect_err("should fail, but parsed as");
    if err
        .compare_to_file(format!("{}.stderr", file_name.display()))
        .is_err()
    {
        panic!()
    }

    Ok(())
}
