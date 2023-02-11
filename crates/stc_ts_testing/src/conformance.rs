//! Module for parsing conformance test suite

use stc_ts_builtin_types::Lib;
use stc_ts_env::{ModuleConfig, Rule};
use swc_ecma_ast::EsVersion;
use swc_ecma_parser::TsConfig;

pub struct TestSpec {
    pub err_shift_n: usize,
    pub libs: Vec<Lib>,
    pub rule: Rule,
    #[allow(unused)]
    pub ts_config: TsConfig,
    pub target: EsVersion,
    pub raw_target: String,
    pub module_config: ModuleConfig,
}
