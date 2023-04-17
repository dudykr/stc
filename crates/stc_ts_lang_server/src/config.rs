use jsonc_parser::{parse_to_serde_value, ParseOptions};
use stc_ts_env::{ModuleConfig, Rule};
use swc_ecma_ast::EsVersion;
use tracing::error;

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
pub(crate) fn parse_ts_config(db: &dyn Db, src: SourceText) -> ParsedTsConfig {
    let s = src.content(db);

    let result = parse_to_serde_value(
        s.trim_start_matches('\u{feff}'),
        &ParseOptions {
            allow_comments: true,
            allow_trailing_commas: true,
            allow_loose_object_property_names: false,
        },
    );

    let result = match result {
        Ok(Some(v)) => v,
        _ => {
            error!("Failed to parse ts config: {:?}", result);
            return ParsedTsConfig::new(db, Default::default(), Default::default(), Default::default());
        }
    };
}
