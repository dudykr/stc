//! Module for parsing conformance test suite

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
