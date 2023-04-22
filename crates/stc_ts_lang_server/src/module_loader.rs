use std::sync::Arc;

use stc_ts_builtin_types::Lib;
use stc_ts_env::{BuiltIn, Env};
use stc_ts_file_analyzer::env::BuiltInGen;
use stc_ts_module_loader::resolvers::node::NodeResolver;
use stc_ts_type_checker::loader::{DefaultFileLoader, LoadModule, ModuleLoader};
use stc_utils::DebugIgnore;

use crate::{config::ParsedTsConfig, Db};

#[salsa::tracked]
pub(crate) struct ProjectEnv {
    #[id]
    tsconfig: ParsedTsConfig,

    #[no_eq]
    pub env: DebugIgnore<Env>,
    #[no_eq]
    pub loader: DebugIgnore<Arc<dyn LoadModule>>,
}

#[salsa::tracked]
pub(crate) fn get_module_loader(db: &dyn Db, config: ParsedTsConfig) -> ProjectEnv {
    let shared = db.shared();
    let cm = shared.cm.clone();

    let libs = config
        .raw(db)
        .as_ref()
        .and_then(|v| v.lib.as_ref())
        .map(|libs| libs.iter().map(From::from).collect::<Vec<_>>())
        .unwrap_or_else(|| vec![Lib::Es5]);

    let builtin = BuiltIn::from_ts_libs(&shared.stable_env, &libs, false);
    let env = Env::new(
        shared.stable_env.clone(),
        config.rule(db),
        config.target(db),
        config.module(db),
        builtin,
    );

    let loader = ModuleLoader::new(cm, env.clone(), NodeResolver, DefaultFileLoader);

    ProjectEnv::new(db, config, DebugIgnore(env), DebugIgnore(Arc::new(loader)))
}
