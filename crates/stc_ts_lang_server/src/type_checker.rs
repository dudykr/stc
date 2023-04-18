use std::{
    fmt::Debug,
    mem::take,
    sync::{Arc, Mutex},
};

use stc_ts_builtin_types::Lib;
use stc_ts_env::{BuiltIn, Env};
use stc_ts_file_analyzer::env::BuiltInGen;
use stc_ts_module_loader::resolvers::node::NodeResolver;
use stc_ts_type_checker::{
    loader::{DefaultFileLoader, LoadModule, ModuleLoader},
    Checker,
};
use stc_ts_types::Type;
use stc_utils::DebugIgnore;
use swc_common::{
    errors::{Diagnostic, Emitter, Handler},
    FileName,
};

use crate::{config::ParsedTsConfig, parser::ParsedFile, Db};

#[salsa::tracked]
pub(crate) struct TypeCheckInput {
    pub file: ParsedFile,
    pub config: ParsedTsConfig,
}

#[salsa::accumulator]
pub struct Diagnostics(Diagnostic);

#[salsa::tracked]
pub(crate) struct ModuleTypeData {
    #[no_eq]
    pub data: Type,
}

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
pub(crate) fn get_module_loader(db: &dyn Db, input: TypeCheckInput) -> ProjectEnv {}

#[salsa::tracked]
pub(crate) fn check_type(db: &dyn Db, input: TypeCheckInput) -> ModuleTypeData {
    let emitter = EmitterImpl::default();
    let errors = emitter.0.clone();

    let shared = db.shared();
    let cm = shared.cm.clone();

    let handler = Handler::with_emitter(true, false, Box::new(emitter));
    let handler = Arc::new(handler);

    let config = input.config(db);
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

    let checker = Checker::new(
        cm.clone(),
        handler,
        env.clone(),
        None,
        Box::new(ModuleLoader::new(cm, env, NodeResolver, DefaultFileLoader)),
    );

    let module_id = checker.check(input.file(db).filename(db));

    let errors = take(&mut *errors.lock().unwrap());
    for err in errors {
        Diagnostics::push(db, err);
    }

    let data = checker.get_types(module_id);

    let ty = data.expect("failed to get type data for the module");

    ModuleTypeData::new(db, ty)
}

#[derive(Default)]
struct EmitterImpl(Arc<Mutex<Vec<Diagnostic>>>);

impl Emitter for EmitterImpl {
    fn emit(&mut self, db: &swc_common::errors::DiagnosticBuilder<'_>) {
        self.0.lock().unwrap().push((**db).clone());
    }
}
