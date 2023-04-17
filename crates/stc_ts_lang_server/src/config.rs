use stc_ts_env::{ModuleConfig, Rule};
use swc_ecma_ast::EsVersion;

use crate::{ir::SourceText, Db};

#[salsa::tracked]
pub struct ParsedTsConfig {
    #[no_eq]
    pub rule: Rule,

    #[no_eq]
    pub target: EsVersion,

    #[no_eq]
    pub module: ModuleConfig,
}

#[salsa::tracked]
pub(crate) fn parse_ts_config(db: &dyn Db, content: SourceText) -> ParsedTsConfig {}
