use stc_testing::logger;
use stc_ts_builtin_types::Lib;
use stc_ts_file_analyzer::env::BuiltIn;
use stc_ts_file_analyzer::env::Env;
use stc_ts_file_analyzer::env::StableEnv;
use std::sync::Arc;
use swc_common::Globals;
use swc_common::DUMMY_SP;
use swc_common::GLOBALS;

#[test]
pub fn builtin() {
    testing::run_test2(false, |_, _| {
        let globals = Arc::new(Globals::default());

        GLOBALS.set(&globals, || {
            let log = logger();
            let shared = StableEnv::new(log.logger, globals.clone());
            let mut libs = vec![];
            for s in &[
                "es2020.full",
                "es2019.full",
                "es2018.full",
                "es2017.full",
                "es2016.full",
                "es2015.full",
                "es5.full",
            ] {
                libs.extend(Lib::load(&s));
            }
            libs.sort();
            libs.dedup();
            let data = BuiltIn::from_ts_libs(&shared, &libs);

            let env = Env::new(
                shared,
                Default::default(),
                swc_ecma_ast::EsVersion::Es2020,
                Arc::new(data),
            );

            {
                let f = env
                    .get_global_type(DUMMY_SP, &"Function".into())
                    .expect("failed to get global type Function");

                let function = f.foldable().interface().unwrap();
                assert_eq!(function.extends, vec![]);

                for member in &function.body {
                    if let Some(key) = member.key() {
                        println!("Key: {:?}", key);
                    }
                }

                let keyed_item_count = function.body.iter().filter_map(|el| el.key()).count();

                assert_eq!(keyed_item_count, 10);
            }

            Ok(())
        })
    })
    .unwrap();
}
