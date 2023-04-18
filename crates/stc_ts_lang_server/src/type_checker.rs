use std::{
    fmt::Debug,
    mem::take,
    sync::{Arc, Mutex},
};

use stc_ts_type_checker::Checker;
use stc_ts_types::Type;
use swc_common::{
    errors::{Diagnostic, Emitter, Handler},
    FileName,
};

use crate::{
    config::{tsconfig_for, ParsedTsConfig},
    ir::SourceFile,
    module_loader::get_module_loader,
    Db,
};

#[salsa::tracked]
pub(crate) struct TypeCheckInput {
    pub file: SourceFile,
    pub config: ParsedTsConfig,
}

#[salsa::accumulator]
pub struct Diagnostics(Diagnostic);

#[salsa::tracked]
pub(crate) struct ModuleTypeData {
    #[no_eq]
    pub data: Type,
}

pub(crate) fn prepare_input(db: &dyn Db, filename: &Arc<FileName>) -> TypeCheckInput {
    let file = db.read_file(filename);
    let config = tsconfig_for(db, file);

    TypeCheckInput::new(db, file, config)
}

#[salsa::tracked]
pub(crate) fn check_type(db: &dyn Db, input: TypeCheckInput) -> ModuleTypeData {
    let emitter = EmitterImpl::default();
    let errors = emitter.0.clone();

    let shared = db.shared();
    let cm = shared.cm.clone();

    let handler = Handler::with_emitter(true, false, Box::new(emitter));
    let handler = Arc::new(handler);

    let config = input.config(db);
    let project = get_module_loader(db, config);
    let env = project.env(db).0;

    let checker = Checker::new(cm, handler, env, None, Box::new(project.loader(db).0.clone()));

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
