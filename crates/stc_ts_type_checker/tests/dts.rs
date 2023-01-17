use std::{path::Path, sync::Arc};

use stc_ts_builtin_types::Lib;
use stc_ts_env::{Env, ModuleConfig};
use stc_ts_errors::ErrorKind;
use stc_ts_file_analyzer::env::EnvFactory;
use stc_ts_module_loader::resolvers::node::NodeResolver;
use stc_ts_type_checker::Checker;
use swc_common::FileName;
use swc_ecma_ast::EsVersion;
use swc_ecma_parser::TsConfig;

#[test]
fn test_node() {
    run_tests_for_types_pkg("node");
}

#[test]
fn test_react() {
    run_tests_for_types_pkg("react");
}

fn run_tests_for_types_pkg(name: &str) {
    testing::run_test2(false, |cm, handler| {
        let handler = Arc::new(handler);

        let mut checker = Checker::new(
            cm,
            handler.clone(),
            Env::simple(Default::default(), EsVersion::latest(), ModuleConfig::None, &Lib::load("es5")),
            TsConfig { ..Default::default() },
            None,
            Arc::new(NodeResolver),
        );

        checker.check(Arc::new(FileName::Real(
            Path::new("node_modules").join("@types").join(name).join("index.d.ts"),
        )));

        for err in ErrorKind::flatten(checker.take_errors()) {
            err.emit(&handler);
        }

        if handler.has_errors() {
            return Err(());
        }

        Ok(())
    })
    .unwrap();
}
