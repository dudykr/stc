use stc_ts_env::Rule;

#[salsa::tracked]
pub struct ParsedTsConfig {
    #[no_eq]
    pub rule: Rule,
}
