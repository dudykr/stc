#![feature(box_syntax)]
#![feature(test)]

use stc_ts_builtin_types::Lib;
use stc_ts_file_analyzer::env::{Env, ModuleConfig};
use std::hint::black_box;
use swc_common::DUMMY_SP;
use swc_ecma_ast::EsVersion;

fn main() {
    testing::run_test2(false, |_cm, _handler| {
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
        let env = Env::simple(Default::default(), EsVersion::latest(), ModuleConfig::None, &libs);
        let env = black_box(env);

        println!("{:?}", env.get_global_type(DUMMY_SP, &"Function".into()));
        Ok(())
    })
    .unwrap()
}
