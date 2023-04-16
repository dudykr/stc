use std::{env::current_dir, sync::Arc};

use stc_ts_builtin_types::Lib;
use stc_ts_env::{Env, ModuleConfig};
use stc_ts_errors::ErrorKind;
use stc_ts_file_analyzer::env::EnvFactory;
use stc_ts_module_loader::resolvers::node::NodeResolver;
use stc_ts_type_checker::{
    loader::{DefaultFileLoader, ModuleLoader},
    Checker,
};
use swc_common::FileName;
use swc_ecma_ast::EsVersion;
use swc_ecma_loader::resolve::Resolve;

#[test]
#[ignore = "Cross-file namespace is not supported"]
fn test_node() {
    run_tests_for_types_pkg("@types/node/index.d.ts");
}

#[test]
#[ignore = "Module resolution is buggy"]
fn test_react() {
    run_tests_for_types_pkg("@types/react/index.d.ts");
}

#[test]
fn test_csstype() {
    run_tests_for_types_pkg("csstype/index.d.ts");
}

fn run_tests_for_types_pkg(module_specifier: &str) {
    testing::run_test2(false, |cm, handler| {
        let handler = Arc::new(handler);

        let path = NodeResolver::new()
            .resolve(&FileName::Real(current_dir().unwrap()), module_specifier)
            .expect("failed to resolve entry");

        let env = Env::simple(Default::default(), EsVersion::latest(), ModuleConfig::None, &Lib::load("es2020"));
        let mut checker = Checker::new(
            cm.clone(),
            handler.clone(),
            env.clone(),
            None,
            Box::new(ModuleLoader::new(cm, env, NodeResolver, DefaultFileLoader)),
        );

        checker.check(Arc::new(path));

        for err in ErrorKind::flatten(checker.take_errors()) {
            err.emit(&handler);
        }

        if handler.has_errors() {
            panic!()
        }

        Ok(())
    })
    .unwrap();
}
