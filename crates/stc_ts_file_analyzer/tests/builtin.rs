use std::sync::Arc;

use stc_ts_builtin_types::Lib;
use stc_ts_env::{BuiltIn, Env, ModuleConfig, StableEnv};
use stc_ts_file_analyzer::env::BuiltInGen;
use swc_atoms::JsWord;
use swc_common::DUMMY_SP;

#[test]
pub fn builtin() {
    testing::run_test2(false, |_, _| {
        let shared = StableEnv::new();
        let mut libs = vec![];
        for s in &[
            "es2022.full",
            "es2021.full",
            "es2020.full",
            "es2019.full",
            "es2018.full",
            "es2017.full",
            "es2016.full",
            "es2015.full",
            "es5.full",
        ] {
            libs.extend(Lib::load(s));
        }
        libs.sort();
        libs.dedup();
        let data = BuiltIn::from_ts_libs(&shared, &libs, false);

        let env = Env::new(
            shared,
            Default::default(),
            swc_ecma_ast::EsVersion::Es2020,
            ModuleConfig::None,
            Arc::new(data),
        );

        {
            let f = env
                .get_global_type(DUMMY_SP, &"Function".into())
                .expect("failed to get global type Function");

            let function = f.expect_interface();
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
    .unwrap();
}

#[test]
pub fn intl() {
    testing::run_test2(false, |_, _| {
        let shared = StableEnv::new();
        let mut libs = vec![];
        {
            let s = &"es5";
            libs.extend(Lib::load(s));
        }
        libs.sort();
        libs.dedup();
        let data = BuiltIn::from_ts_libs(&shared, &libs, false);

        let env = Env::new(
            shared,
            Default::default(),
            swc_ecma_ast::EsVersion::Es2020,
            ModuleConfig::None,
            Arc::new(data),
        );

        {
            let intl = env
                .get_global_type(DUMMY_SP, &"Intl".into())
                .expect("failed to get global type Intl");

            let i = intl.expect_module();
            let type_names = i.exports.types.iter().map(|v| v.0).collect::<Vec<_>>();
            eprintln!("Type names: {:?}", type_names);
            assert!(i.exports.types.contains_key(&JsWord::from("NumberFormatOptions")));
        }

        Ok(())
    })
    .unwrap();
}
