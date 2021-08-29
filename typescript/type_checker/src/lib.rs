//! Full type checker with dependency support.
#![feature(box_syntax)]

use dashmap::{DashMap, DashSet, SharedValue};
use fxhash::{FxBuildHasher, FxHashMap};
use once_cell::sync::OnceCell;
use parking_lot::{Mutex, RwLock};
use rnode::{NodeIdGenerator, RNode, VisitWith};
use stc_ts_ast_rnode::RModule;
use stc_ts_dts::{apply_mutations, cleanup_module_for_dts};
use stc_ts_errors::{debug::debugger::Debugger, Error};
use stc_ts_file_analyzer::{
    analyzer::Analyzer,
    env::Env,
    loader::{Load, ModuleInfo},
    validator::ValidateWith,
    ModuleTypeData, ValidationResult,
};
use stc_ts_module_loader::{resolver::Resolve, ModuleGraph};
use stc_ts_storage::{ErrorStore, File, Group, Single};
use stc_ts_types::ModuleId;
use stc_ts_utils::StcComments;
use stc_utils::early_error;
use std::{mem::take, path::PathBuf, sync::Arc, time::Instant};
use swc_atoms::JsWord;
use swc_common::{errors::Handler, SourceMap, Spanned};
use swc_ecma_ast::Module;
use swc_ecma_parser::TsConfig;
use swc_ecma_transforms::resolver::ts_resolver;
use swc_ecma_visit::FoldWith;
use tracing::{info, warn};

/// Onc instance per swc::Compiler
pub struct Checker {
    cm: Arc<SourceMap>,
    handler: Arc<Handler>,
    /// Cache
    module_types: RwLock<FxHashMap<ModuleId, Arc<OnceCell<Arc<ModuleTypeData>>>>>,

    /// Informatnion required to generate `.d.ts` files.
    dts_modules: Arc<DashMap<ModuleId, RModule, FxBuildHasher>>,

    module_graph: Arc<ModuleGraph<StcComments, Arc<dyn Resolve>>>,

    /// Modules which are being processed or analyzed.
    started: Arc<DashSet<ModuleId, FxBuildHasher>>,

    errors: Mutex<Vec<Error>>,

    env: Env,

    debugger: Option<Debugger>,
}

impl Checker {
    pub fn new(
        cm: Arc<SourceMap>,
        handler: Arc<Handler>,
        env: Env,
        parser_config: TsConfig,
        debugger: Option<Debugger>,
        resolver: Arc<dyn Resolve>,
    ) -> Self {
        Checker {
            env: env.clone(),
            cm: cm.clone(),
            handler,
            module_types: Default::default(),
            dts_modules: Default::default(),
            module_graph: Arc::new(ModuleGraph::new(
                cm,
                Some(Default::default()),
                resolver,
                parser_config,
                env.target(),
            )),
            started: Default::default(),
            errors: Default::default(),
            debugger,
        }
    }

    pub fn run<F, R>(&self, op: F) -> R
    where
        F: FnOnce() -> R,
    {
        ::swc_common::GLOBALS.set(&self.globals(), || op())
    }

    pub fn globals(&self) -> &swc_common::Globals {
        &self.env.shared().swc_globals()
    }
}

impl Checker {
    /// Get type informations of a module.
    pub fn get_types(&self, id: ModuleId) -> Option<Arc<ModuleTypeData>> {
        let lock = self.module_types.read();
        lock.get(&id).and_then(|v| v.get().cloned())
    }

    /// Removes dts module from `self` and return it.
    pub fn take_dts(&self, id: ModuleId) -> Option<Module> {
        self.dts_modules.remove(&id).map(|v| v.1.into_orig())
    }

    pub fn id(&self, path: &Arc<PathBuf>) -> ModuleId {
        self.module_graph.id(path)
    }

    /// After calling this method, you can get errors using `.take_errors()`
    pub fn check(&self, entry: Arc<PathBuf>) -> ModuleId {
        self.run(|| {
            let start = Instant::now();

            let id = self.module_graph.load_all(&entry).unwrap();

            let end = Instant::now();
            log::info!("Loading of `{}` (and deps) took {:?}", entry.display(), end - start);

            let start = Instant::now();

            self.analyze_module(None, entry.clone());

            let end = Instant::now();
            log::info!("Analysis `{}` (and deps) took {:?}", entry.display(), end - start);

            id
        })
    }

    pub fn take_errors(&mut self) -> Vec<Error> {
        take(self.errors.get_mut())
    }

    /// Analyzes one module.
    fn analyze_module(&self, starter: Option<Arc<PathBuf>>, path: Arc<PathBuf>) -> Arc<ModuleTypeData> {
        self.run(|| {
            let id = self.module_graph.id(&path);

            {
                let lock = self.module_types.read();
                // If a circular chunks are fully analyzed, used them.
                if let Some(full) = lock.get(&id).map(|cell| cell.get().cloned()).flatten() {
                    return full;
                }
            }

            let is_first_run = self.started.insert(id);

            let circular_set = self.module_graph.get_circular(id);

            if is_first_run {
                if let Some(set) = &circular_set {
                    {
                        // Mark all modules in the circular group as in-progress.
                        let shards = self.started.shards();

                        for &dep_id in set {
                            let idx = self.started.determine_map(&dep_id);

                            let mut lock = shards[idx].write();
                            lock.insert(dep_id, SharedValue::new(()));
                        }
                    }

                    {
                        let mut node_id_gen = NodeIdGenerator::default();
                        let mut storage = Group {
                            parent: None,
                            files: Arc::new(
                                set.iter()
                                    .copied()
                                    .map(|id| {
                                        let path = self.module_graph.path(id);
                                        let stmt_count = self.module_graph.stmt_count_of(id);
                                        File { id, path, stmt_count }
                                    })
                                    .collect(),
                            ),
                            errors: Default::default(),
                            info: Default::default(),
                        };
                        let ids = set.iter().copied().collect::<Vec<_>>();
                        let modules = ids
                            .iter()
                            .map(|&id| self.module_graph.clone_module(id))
                            .filter_map(|m| m)
                            .map(|module| {
                                RModule::from_orig(
                                    &mut node_id_gen,
                                    module.fold_with(&mut ts_resolver(self.env.shared().marks().top_level_mark())),
                                )
                            })
                            .collect::<Vec<_>>();
                        let mut mutations;
                        {
                            let mut a = Analyzer::root(
                                self.env.clone(),
                                self.cm.clone(),
                                box &mut storage,
                                self,
                                self.debugger.clone(),
                            );
                            let _ = modules.validate_with(&mut a);
                            mutations = a.mutations.unwrap();
                        }

                        for (id, mut dts_module) in ids.iter().zip(modules) {
                            let type_data = storage.info.entry(*id).or_default();

                            {
                                apply_mutations(&mut mutations, &mut dts_module);
                                cleanup_module_for_dts(&mut dts_module.body, &type_data);
                            }

                            // TODO: Prevent duplicate work.
                            match self.dts_modules.insert(*id, dts_module) {
                                Some(..) => {
                                    warn!("Duplicated work: `{}`: (.d.ts already computed)", path.display());
                                }
                                None => {}
                            }
                        }

                        {
                            let mut lock = self.errors.lock();
                            lock.extend(storage.take_errors());
                        }
                        {
                            let mut lock = self.module_types.write();
                            for (module_id, data) in storage.info {
                                let res = lock.entry(module_id).or_default().set(Arc::new(data));
                                match res {
                                    Ok(()) => {}
                                    Err(..) => {
                                        warn!("Duplicated work: `{}`: (type info is already cached)", path.display());
                                    }
                                }
                            }
                        }
                    }

                    let lock = self.module_types.read();
                    return lock.get(&id).map(|cell| cell.get().cloned()).flatten().unwrap();
                }
            }
            info!(
                "Request: {}\nRequested by {:?}\nCircular set: {:?}",
                path.display(),
                starter,
                circular_set
            );

            {
                // With write lock, we ensure that OnceCell is inserted.
                let mut lock = self.module_types.write();
                lock.entry(id).or_default();
            }

            {
                let start = Instant::now();
                let mut did_work = false;
                let v = self.module_types.read().get(&id).cloned().clone().unwrap();
                // We now wait for dependency without holding lock
                let res = v
                    .get_or_init(|| {
                        did_work = true;
                        let result = self.analyze_non_circular_module(id, path.clone());
                        result
                    })
                    .clone();

                let dur = Instant::now() - start;
                if did_work {
                    eprintln!("[Timing] Waited for {}: {:?}", path.display(), dur);
                }

                res
            }
        })
    }

    fn analyze_non_circular_module(&self, id: ModuleId, path: Arc<PathBuf>) -> Arc<ModuleTypeData> {
        self.run(|| {
            let start = Instant::now();

            let mut node_id_gen = NodeIdGenerator::default();
            let mut module = self
                .module_graph
                .clone_module(id)
                .unwrap_or_else(|| unreachable!("Module graph does not contains {:?}: {}", id, path.display()));
            module = module.fold_with(&mut ts_resolver(self.env.shared().marks().top_level_mark()));
            let mut module = RModule::from_orig(&mut node_id_gen, module);

            let mut storage = Single {
                parent: None,
                id,
                path: path.clone(),
                info: Default::default(),
            };
            let mut mutations;
            {
                let mut a = Analyzer::root(
                    self.env.clone(),
                    self.cm.clone(),
                    box &mut storage,
                    self,
                    self.debugger.clone(),
                );

                module.visit_with(&mut a);
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

            let type_info = Arc::new(storage.info.exports);

            self.dts_modules.insert(id, module);

            let dur = Instant::now() - start;
            eprintln!("[Timing] Full analysis of {}: {:?}", path.display(), dur);

            type_info
        })
    }
}

impl Load for Checker {
    fn module_id(&self, base: &Arc<PathBuf>, src: &JsWord) -> Option<ModuleId> {
        let path = self.module_graph.resolve(&base, src).ok()?;
        let id = self.module_graph.id(&path);
        Some(id)
    }

    fn is_in_same_circular_group(&self, base: ModuleId, dep: ModuleId) -> bool {
        let circular_set = self.module_graph.get_circular(base);

        match circular_set {
            Some(set) => set.contains(&dep),
            None => false,
        }
    }

    fn load_circular_dep(
        &self,
        base: ModuleId,
        dep: ModuleId,
        _partial: &ModuleTypeData,
    ) -> ValidationResult<ModuleInfo> {
        let base_path = self.module_graph.path(base);
        let dep_path = self.module_graph.path(dep);

        let data = self.analyze_module(Some(base_path.clone()), dep_path.clone());

        return Ok(ModuleInfo { module_id: dep, data });
    }

    fn load_non_circular_dep(&self, base: ModuleId, dep: ModuleId) -> ValidationResult<ModuleInfo> {
        let base_path = self.module_graph.path(base);
        let dep_path = self.module_graph.path(dep);

        info!("({}): Loading {}", base_path.display(), dep_path.display());

        let data = self.analyze_module(Some(base_path.clone()), dep_path.clone());

        return Ok(ModuleInfo { module_id: dep, data });
    }
}
