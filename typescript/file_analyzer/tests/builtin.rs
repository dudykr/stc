use std::sync::Arc;

use stc_testing::logger;
use stc_ts_builtin_types::Lib;
use stc_ts_file_analyzer::analyzer::Analyzer;
use stc_ts_file_analyzer::env::BuiltIn;
use stc_ts_file_analyzer::env::Env;
use stc_ts_file_analyzer::env::StableEnv;
use stc_ts_storage::Builtin;
use swc_common::Globals;
use swc_common::DUMMY_SP;
use swc_common::GLOBALS;

#[test]
pub fn builtin() {
    testing::run_test2(false, |cm, handler| {
        let globals = Arc::new(Globals::default());

        GLOBALS.set(&globals, || {
            let log = logger();
            let shared = StableEnv::new(log.logger, globals.clone());
            let data = BuiltIn::from_ts_libs(&shared, &[Lib::Es5]);

            let env = Env::new(
                shared,
                Default::default(),
                swc_ecma_ast::EsVersion::Es2020,
                Arc::new(data),
            );

            let f = env
                .get_global_type(DUMMY_SP, &"Function".into())
                .expect("failed to get global type Function");

            println!("{:?}", f);

            Ok(())
        })
    })
    .unwrap();
}
