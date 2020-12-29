//! Full type checker with dependency support.
#![feature(box_syntax)]

use dashmap::DashMap;
use dashmap::DashSet;
use dashmap::SharedValue;
use fxhash::FxHashMap;
use once_cell::sync::OnceCell;
use parking_lot::Mutex;
use parking_lot::RwLock;
use rnode::NodeIdGenerator;
use rnode::RNode;
use rnode::VisitMutWith;
use slog::Logger;
use stc_ts_ast_rnode::RModule;
use stc_ts_dts::cleanup_module_for_dts;
use stc_ts_file_analyzer::analyzer::Analyzer;
use stc_ts_file_analyzer::env::Env;
use stc_ts_file_analyzer::errors::Error;
use stc_ts_file_analyzer::loader::Load;
use stc_ts_file_analyzer::loader::ModuleInfo;
use stc_ts_file_analyzer::mode::ErrorStore;
use stc_ts_file_analyzer::mode::File;
use stc_ts_file_analyzer::mode::Group;
use stc_ts_file_analyzer::mode::Single;
use stc_ts_file_analyzer::validator::ValidateWith;
use stc_ts_file_analyzer::DepInfo;
use stc_ts_file_analyzer::ModuleTypeData;
use stc_ts_module_loader::resolver::node::NodeResolver;
use stc_ts_module_loader::ModuleGraph;
use stc_ts_types::ModuleId;
use stc_ts_utils::StcComments;
use stc_utils::early_error;
use std::mem::take;
use std::path::PathBuf;
use std::sync::Arc;
use std::time::Instant;
use swc_atoms::JsWord;
use swc_common::errors::Handler;
use swc_common::SourceMap;
use swc_common::Spanned;
use swc_ecma_ast::Module;
use swc_ecma_parser::TsConfig;
use swc_ecma_transforms::resolver::ts_resolver;
use swc_ecma_visit::FoldWith;

/// Onc instance per swc::Compiler
pub struct Checker {
    logger: Logger,
    cm: Arc<SourceMap>,
    handler: Arc<Handler>,
    /// Cache
    module_types: RwLock<FxHashMap<ModuleId, Arc<OnceCell<Arc<ModuleTypeData>>>>>,

    /// Informatnion required to generate `.d.ts` files.
    dts_modules: Arc<DashMap<ModuleId, RModule>>,

    module_graph: Arc<ModuleGraph<StcComments, NodeResolver>>,

    /// Modules which are being processed or analyzed.
    started: Arc<DashSet<ModuleId>>,

    errors: Mutex<Vec<Error>>,

    env: Env,
}

impl Checker {
    pub fn new(
        logger: Logger,
        cm: Arc<SourceMap>,
        handler: Arc<Handler>,
        env: Env,
        parser_config: TsConfig,
    ) -> Self {
        Checker {
            logger,
            env: env.clone(),
            cm: cm.clone(),
            handler,
            module_types: Default::default(),
            dts_modules: Default::default(),
            module_graph: Arc::new(ModuleGraph::new(
                cm,
                Some(Default::default()),
                NodeResolver,
                parser_config,
                env.target(),
            )),
            started: Default::default(),
            errors: Default::default(),
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
        lock.get(&id).map(|v| v.get().cloned()).flatten()
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
            let id = self.module_graph.load_all(&entry).unwrap();

            self.analyze_module(None, entry.clone());

            id
        })
    }

    pub fn take_errors(&mut self) -> Vec<Error> {
        take(self.errors.get_mut())
    }

    /// Analyzes one module.
    fn analyze_module(
        &self,
        starter: Option<Arc<PathBuf>>,
        path: Arc<PathBuf>,
    ) -> Arc<ModuleTypeData> {
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
                                        File {
                                            id,
                                            path,
                                            stmt_count,
                                        }
                                    })
                                    .collect(),
                            ),
                            errors: Default::default(),
                            info: Default::default(),
                        };
                        let ids = set.iter().copied().collect::<Vec<_>>();
                        let mut modules = ids
                            .iter()
                            .map(|&id| self.module_graph.clone_module(id))
                            .map(|module| {
                                RModule::from_orig(
                                    &mut node_id_gen,
                                    module.fold_with(&mut ts_resolver(
                                        self.env.shared().marks().top_level_mark(),
                                    )),
                                )
                            })
                            .collect::<Vec<_>>();
                        {
                            let mut a = Analyzer::root(
                                self.logger
                                    .new(slog::o!("file" => path.to_string_lossy().to_string())),
                                self.env.clone(),
                                self.cm.clone(),
                                box &mut storage,
                                self,
                            );
                            let _ = modules.validate_with(&mut a);
                        }

                        for (id, mut dts_module) in ids.iter().zip(modules) {
                            let type_data = storage.info.entry(*id).or_default();

                            {
                                cleanup_module_for_dts(&mut dts_module.body, &type_data);
                            }

                            // TODO: Prevent duplicate work.
                            match self.dts_modules.insert(*id, dts_module) {
                                Some(..) => {
                                    slog::warn!(
                                        self.logger,
                                        "Duplicated work: `{}`: (.d.ts already computed)",
                                        path.display()
                                    );
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
                                        slog::warn!(
                                            self.logger,
                                            "Duplicated work: `{}`: (type info is already cached)",
                                            path.display()
                                        );
                                    }
                                }
                            }
                        }
                    }

                    let lock = self.module_types.read();
                    return lock
                        .get(&id)
                        .map(|cell| cell.get().cloned())
                        .flatten()
                        .unwrap();
                }
            }
            slog::info!(
                &self.logger,
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
                    eprintln!("[Timing] Full analysis of {}: {:?}", path.display(), dur);
                } else {
                    eprintln!("[Timing] Waited for {}: {:?}", path.display(), dur);
                }

                res
            }
        })
    }

    fn analyze_non_circular_module(&self, id: ModuleId, path: Arc<PathBuf>) -> Arc<ModuleTypeData> {
        self.run(|| {
            let mut node_id_gen = NodeIdGenerator::default();
            let mut module = self.module_graph.clone_module(id);
            module = module.fold_with(&mut ts_resolver(self.env.shared().marks().top_level_mark()));
            let mut module = RModule::from_orig(&mut node_id_gen, module);

            let mut storage = Single {
                parent: None,
                id,
                path: path.clone(),
                info: Default::default(),
            };
            {
                let mut a = Analyzer::root(
                    self.logger
                        .new(slog::o!("file" => path.to_string_lossy().to_string())),
                    self.env.clone(),
                    self.cm.clone(),
                    box &mut storage,
                    self,
                );

                module.visit_mut_with(&mut a);
            }

            {
                // Get .d.ts file
                cleanup_module_for_dts(&mut module.body, &storage.info.exports);
            }

            if early_error() {
                for err in storage.info.errors {
                    self.handler
                        .struct_span_err(err.span(), &format!("{:?}", err))
                        .emit();
                }
            } else {
                let mut errors = self.errors.lock();
                errors.extend(storage.info.errors);
            }

            let type_info = Arc::new(storage.info.exports);

            self.dts_modules.insert(id, module);

            type_info
        })
    }
}

impl Load for Checker {
    fn is_in_same_circular_group(&self, base: &Arc<PathBuf>, src: &JsWord) -> bool {
        let id = self.module_graph.id(&base);

        let path = self.module_graph.resolve(&base, src).unwrap();
        let target = self.module_graph.id(&path);

        let circular_set = self.module_graph.get_circular(id);

        match circular_set {
            Some(set) => set.contains(&target),
            None => false,
        }
    }

    fn load_circular_dep(
        &self,
        base: Arc<PathBuf>,
        _partial: &ModuleTypeData,
        import: &DepInfo,
    ) -> Result<ModuleInfo, Error> {
        let _base_id = self.module_graph.id(&base);
        let path = self.module_graph.resolve(&base, &import.src).unwrap();
        let id = self.module_graph.id(&path);

        let data = self.analyze_module(Some(base.clone()), path.clone());

        return Ok(ModuleInfo {
            module_id: id,
            data,
        });
    }

    fn load_non_circular_dep(
        &self,
        base: Arc<PathBuf>,
        import: &DepInfo,
    ) -> Result<ModuleInfo, Error> {
        let mut _result = ModuleTypeData::default();

        // TODO: Use ModuleId for analyze_module
        let path = self.module_graph.resolve(&base, &import.src).unwrap();
        slog::info!(
            self.logger,
            "({}): Loading {}",
            base.display(),
            path.display()
        );
        let id = self.module_graph.id(&path);

        let data = self.analyze_module(Some(base.clone()), path.clone());

        return Ok(ModuleInfo {
            module_id: id,
            data,
        });
    }

    fn module_id(&self, base: &Arc<PathBuf>, src: &JsWord) -> ModuleId {
        let path = self.module_graph.resolve(&base, src).unwrap();
        let id = self.module_graph.id(&path);
        id
    }
}
