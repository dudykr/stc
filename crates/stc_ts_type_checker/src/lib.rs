//! Full type checker with dependency support.
#![feature(box_syntax)]

use std::{mem::take, sync::Arc, time::Instant};

use dashmap::{DashMap, DashSet, SharedValue};
use fxhash::{FxBuildHasher, FxHashMap};
use loader::LoadModule;
use once_cell::sync::OnceCell;
use parking_lot::{Mutex, RwLock};
use rnode::{NodeIdGenerator, RNode, VisitWith};
use stc_ts_ast_rnode::{RModule, RStr, RTsModuleName};
use stc_ts_dts::{apply_mutations, cleanup_module_for_dts};
use stc_ts_env::Env;
use stc_ts_errors::{debug::debugger::Debugger, Error};
use stc_ts_file_analyzer::{analyzer::Analyzer, loader::Load, validator::ValidateWith, ModuleTypeData, VResult};
use stc_ts_storage::{ErrorStore, File, Group, Single};
use stc_ts_types::{ModuleId, Type};
use stc_utils::{cache::Freeze, early_error};
use swc_atoms::JsWord;
use swc_common::{errors::Handler, FileName, SourceMap, Spanned, DUMMY_SP};
use swc_ecma_ast::Module;
use tracing::{info, warn};

pub mod loader;
mod typings;

/// Onc instance per swc::Compiler
pub struct Checker<L>
where
    L: LoadModule,
{
    cm: Arc<SourceMap>,
    handler: Arc<Handler>,
    /// Cache
    module_types: RwLock<FxHashMap<ModuleId, Arc<OnceCell<Type>>>>,

    declared_modules: DashMap<String, ModuleId, FxBuildHasher>,

    /// Information required to generate `.d.ts` files.
    dts_modules: Arc<DashMap<ModuleId, RModule, FxBuildHasher>>,

    module_loader: L,

    /// Modules which are being processed or analyzed.
    started: Arc<DashSet<ModuleId, FxBuildHasher>>,

    errors: Mutex<Vec<Error>>,

    env: Env,

    debugger: Option<Debugger>,
}

impl<L> Checker<L>
where
    L: LoadModule,
{
    pub fn new(cm: Arc<SourceMap>, handler: Arc<Handler>, env: Env, debugger: Option<Debugger>, module_loader: L) -> Self {
        Checker {
            env,
            cm,
            handler,
            module_types: Default::default(),
            dts_modules: Default::default(),
            started: Default::default(),
            errors: Default::default(),
            debugger,
            declared_modules: Default::default(),
            module_loader,
        }
    }
}

impl<L> Checker<L>
where
    L: LoadModule,
{
    /// Get type information of a module.
    pub fn get_types(&self, id: ModuleId) -> Option<Type> {
        let lock = self.module_types.read();
        lock.get(&id).and_then(|v| v.get().cloned())
    }

    /// Removes dts module from `self` and return it.
    pub fn take_dts(&self, id: ModuleId) -> Option<Module> {
        self.dts_modules.remove(&id).map(|v| v.1.into_orig())
    }

    pub fn module_loader(&self) -> &L {
        &self.module_loader
    }

    /// After calling this method, you can get errors using `.take_errors()`
    pub fn check(&self, entry: Arc<FileName>) -> ModuleId {
        let start = Instant::now();

        let modules = self.module_loader.load_module(&entry, true).expect("failed to load entry");

        let end = Instant::now();
        log::debug!("Loading of `{}` and dependencies took {:?}", entry, end - start);

        let start = Instant::now();

        self.analyze_module(None, entry.clone());

        let end = Instant::now();
        log::debug!("Analysis of `{}` and dependencies took {:?}", entry, end - start);

        modules.entry.id
    }

    pub fn take_errors(&mut self) -> Vec<Error> {
        take(self.errors.get_mut())
    }

    /// Analyzes one module.
    fn analyze_module(&self, starter: Option<Arc<FileName>>, path: Arc<FileName>) -> Type {
        let modules_in_group = self
            .module_loader
            .load_module(&path, false)
            .expect("failed to load module? (cycle)");

        let id = modules_in_group.entry.id;
        {
            let lock = self.module_types.read();
            // If a circular chunks are fully analyzed, used them.
            if let Some(full) = lock.get(&id).and_then(|cell| cell.get().cloned()) {
                return full;
            }
        }

        let is_first_run = self.started.insert(id);

        if is_first_run && modules_in_group.modules.len() > 1 {
            {
                // Mark all modules in the circular group as in-progress.
                let shards = self.started.shards();

                for record in modules_in_group.modules.iter() {
                    let idx = self.started.determine_map(&record.id);

                    let mut lock = shards[idx].write();
                    lock.insert(record.id, SharedValue::new(()));
                }
            }

            {
                let mut node_id_gen = NodeIdGenerator::default();
                let mut storage = Group {
                    parent: None,
                    files: Arc::new(
                        modules_in_group
                            .modules
                            .iter()
                            .map(|record| File {
                                id,
                                path: record.filename.clone(),
                                stmt_count: record.ast.body.len(),
                                top_level_ctxt: record.top_level_ctxt,
                            })
                            .collect(),
                    ),
                    errors: Default::default(),
                    info: Default::default(),
                };
                let modules = modules_in_group
                    .modules
                    .iter()
                    .map(|record| RModule::from_orig(&mut node_id_gen, record.ast.clone()))
                    .collect::<Vec<_>>();
                let mut mutations;
                {
                    let mut a = Analyzer::root(
                        self.env.clone(),
                        self.cm.clone(),
                        modules_in_group.comments.clone(),
                        box &mut storage,
                        self,
                        self.debugger.clone(),
                    );
                    let _ = modules.validate_with(&mut a);
                    mutations = a.mutations.unwrap();
                }

                for (record, mut dts_module) in modules_in_group.modules.iter().zip(modules) {
                    let type_data = storage.info.entry(record.id).or_default();

                    {
                        apply_mutations(&mut mutations, &mut dts_module);
                        cleanup_module_for_dts(&mut dts_module.body, type_data);
                    }

                    // TODO(kdy1): Prevent duplicate work.
                    if let Some(..) = self.dts_modules.insert(record.id, dts_module) {
                        warn!("Duplicated work: `{}`: (.d.ts already computed)", path);
                    }
                }

                {
                    let mut lock = self.errors.lock();
                    lock.extend(storage.take_errors());
                }
                {
                    let mut lock = self.module_types.write();
                    for (module_id, data) in storage.info {
                        let type_info = Type::Module(stc_ts_types::Module {
                            span: DUMMY_SP,
                            name: RTsModuleName::Str(RStr {
                                span: DUMMY_SP,
                                value: format!("{:?}", module_id).into(),
                                raw: None,
                            }),
                            exports: box data,
                            metadata: Default::default(),
                            tracker: Default::default(),
                        })
                        .freezed();

                        let res = lock.entry(module_id).or_default().set(type_info);
                        match res {
                            Ok(()) => {}
                            Err(..) => {
                                warn!("Duplicated work: `{}`: (type info is already cached)", path);
                            }
                        }
                    }
                }
            }

            let lock = self.module_types.read();
            return lock.get(&id).and_then(|cell| cell.get().cloned()).unwrap();
        }
        info!("Request: {}\nRequested by {:?}", path, starter);

        {
            // With write lock, we ensure that OnceCell is inserted.
            let mut lock = self.module_types.write();
            lock.entry(id).or_default();
        }

        {
            let start = Instant::now();
            let mut did_work = false;
            let v = self.module_types.read().get(&id).cloned().unwrap();
            // We now wait for dependency without holding lock
            let res = v
                .get_or_init(|| {
                    did_work = true;

                    self.analyze_non_circular_module(id, path.clone())
                })
                .clone();

            let dur = Instant::now() - start;
            if !did_work {
                log::warn!("Waited for {}: {:?}", path, dur);
            }

            res
        }
    }

    fn analyze_non_circular_module(&self, module_id: ModuleId, path: Arc<FileName>) -> Type {
        let start = Instant::now();

        let is_dts = match &*path {
            FileName::Real(path) => path.to_string_lossy().ends_with(".d.ts"),
            _ => false,
        };

        let mut node_id_gen = NodeIdGenerator::default();
        let records = self.module_loader.load_module(&path, false).expect("failed to load module?");
        assert_eq!(
            records.modules.len(),
            1,
            "analyze_non_circular_module should be called with a single module"
        );

        let record = records.modules.into_iter().next().unwrap();

        let mut module = RModule::from_orig(&mut node_id_gen, record.ast.clone());

        let mut storage = Single {
            parent: None,
            id: module_id,
            top_level_ctxt: record.top_level_ctxt,
            path: path.clone(),
            is_dts,
            info: Default::default(),
        };
        let mut mutations;
        {
            let start = Instant::now();
            let mut a = Analyzer::root(
                self.env.clone(),
                self.cm.clone(),
                records.comments,
                box &mut storage,
                self,
                self.debugger.clone(),
            );

            module.visit_with(&mut a);

            let end = Instant::now();
            let dur = end - start;
            log::debug!("[Timing] Analysis of {} took {:?}", path, dur);

            mutations = a.mutations.unwrap();
        }

        {
            // Get .d.ts file
            apply_mutations(&mut mutations, &mut module);
            cleanup_module_for_dts(&mut module.body, &storage.info.exports);
        }

        if early_error() {
            for err in storage.info.errors {
                self.handler.struct_span_err(err.span(), &format!("{:?}", err)).emit();
            }
        } else {
            let mut errors = self.errors.lock();
            errors.extend(storage.info.errors);
        }

        let type_info = Type::Module(stc_ts_types::Module {
            span: module.span,
            name: RTsModuleName::Str(RStr {
                span: DUMMY_SP,
                value: format!("{:?}", module_id).into(),
                raw: None,
            }),
            exports: box storage.info.exports,
            metadata: Default::default(),
            tracker: Default::default(),
        })
        .freezed();

        self.dts_modules.insert(module_id, module);

        let dur = Instant::now() - start;
        log::trace!("[Timing] Full analysis of {} took {:?}", path, dur);

        type_info
    }
}

impl<L> Load for Checker<L>
where
    L: LoadModule,
{
    fn module_id(&self, base: &Arc<FileName>, module_specifier: &str) -> Option<ModuleId> {
        if let Some(id) = self.declared_modules.get(module_specifier) {
            return Some(*id);
        }

        let records = self.module_loader.load_dep(base, module_specifier).ok()?;
        Some(records.entry.id)
    }

    fn is_in_same_circular_group(&self, base: &Arc<FileName>, module_specifier: &str) -> bool {
        let records = self.module_loader.load_dep(base, module_specifier).ok();

        match records {
            Some(set) => set.modules.iter().any(|record| record.filename == *base),
            None => false,
        }
    }

    fn load_circular_dep(&self, base: &Arc<FileName>, dep: &str, _partial: &ModuleTypeData) -> VResult<Type> {
        if let Some(id) = self.declared_modules.get(dep).as_deref().copied() {
            if let Some(cache) = self.module_types.read().get(&id) {
                if let Some(ty) = cache.get() {
                    return Ok(ty.clone());
                }
            }
        }

        let records = self.module_loader.load_dep(base, dep).unwrap();

        let data = self.analyze_module(Some(base.clone()), records.entry.filename.clone());

        Ok(data)
    }

    fn load_non_circular_dep(&self, base: &Arc<FileName>, dep: &str) -> VResult<Type> {
        if let Some(id) = self.declared_modules.get(dep).as_deref().copied() {
            if let Some(cache) = self.module_types.read().get(&id) {
                if let Some(ty) = cache.get() {
                    return Ok(ty.clone());
                }
            }
        }

        let records = self.module_loader.load_dep(base, dep).unwrap();

        let data = self.analyze_module(Some(base.clone()), records.entry.filename.clone());

        Ok(data)
    }

    fn declare_module(&self, name: &JsWord, module: Type) -> ModuleId {
        module.assert_clone_cheap();

        let module_id = self
            .module_loader
            .load_module(&Arc::new(FileName::Custom(name.to_string())), false)
            .unwrap()
            .entry
            .id;

        self.module_types.write().insert(module_id, Arc::new(OnceCell::from(module)));

        info!("Declaring module with type `{}`", name);
        self.declared_modules.insert(name.to_string(), module_id);

        module_id
    }
}
