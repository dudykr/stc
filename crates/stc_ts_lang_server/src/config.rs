use std::sync::Arc;

use stc_ts_env::{ModuleConfig, Rule};
use swc_common::FileName;
use swc_ecma_ast::EsVersion;
use tracing::error;
use tsconfig::{Target, TsConfig};

use crate::{ir::SourceFile, Db};

#[salsa::tracked]
pub struct ParsedTsConfig {
    #[no_eq]
    pub rule: Rule,

    #[no_eq]
    pub target: EsVersion,

    #[no_eq]
    pub module: ModuleConfig,

    #[no_eq]
    #[return_ref]
    pub raw: Option<tsconfig::CompilerOptions>,
}

#[salsa::tracked]
pub(crate) fn read_tsconfig_file_for(db: &dyn Db, _filename: SourceFile) -> SourceFile {
    // TODO: Use the file systme stored in `db`

    SourceFile::new(db, Arc::new(FileName::Custom("todo.tsconfig.json".into())), "{}".into())
}

#[salsa::tracked]
pub(crate) fn tsconfig_for(db: &dyn Db, filename: SourceFile) -> ParsedTsConfig {
    let content = read_tsconfig_file_for(db, filename);

    parse_ts_config(db, content)
}

#[salsa::tracked]
pub(crate) fn parse_ts_config(db: &dyn Db, src: SourceFile) -> ParsedTsConfig {
    let s = src.content(db);

    // TODO: Use file path to support `extends`

    let result = TsConfig::parse_str(s);
    let v = match result {
        Ok(v) => v,
        _ => {
            error!("Failed to parse ts config: {:?}", result);
            return ParsedTsConfig::new(db, Default::default(), Default::default(), Default::default(), Default::default());
        }
    };

    ParsedTsConfig::new(
        db,
        v.compiler_options.as_ref().map(Rule::from).unwrap_or_default(),
        v.compiler_options
            .as_ref()
            .and_then(|v| v.target.clone())
            .map(|v| match v {
                Target::Es3 => EsVersion::Es3,
                Target::Es5 => EsVersion::Es5,
                Target::Es2015 | Target::Es6 => EsVersion::Es2015,
                Target::Es2016 | Target::Es7 => EsVersion::Es2016,
                Target::Es2017 => EsVersion::Es2017,
                Target::Es2018 => EsVersion::Es2018,
                Target::Es2019 => EsVersion::Es2019,
                Target::Es2020 => EsVersion::Es2020,
                Target::EsNext => EsVersion::EsNext,
                Target::Other(s) => match &*s {
                    "es2021" => EsVersion::Es2021,
                    "es2022" => EsVersion::Es2021,
                    _ => todo!("Unknown target: {}", s),
                },
            })
            .unwrap_or_else(EsVersion::latest),
        v.compiler_options
            .as_ref()
            .and_then(|v| v.module.clone())
            .map_or_else(ModuleConfig::default, ModuleConfig::from),
        v.compiler_options,
    )
}
