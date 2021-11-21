//! Full type checker with dependency support.
#![feature(box_syntax)]

use dashmap::{DashMap, DashSet, SharedValue};
use fxhash::{FxBuildHasher, FxHashMap};
use once_cell::sync::OnceCell;
use parking_lot::{Mutex, RwLock};
use rnode::{NodeIdGenerator, RNode, VisitWith};
use stc_ts_ast_rnode::{RModule, RStr, RTsModuleName};
use stc_ts_dts::{apply_mutations, cleanup_module_for_dts};
use stc_ts_env::Env;
use stc_ts_errors::{debug::debugger::Debugger, Error};
use stc_ts_file_analyzer::{
    analyzer::Analyzer, loader::Load, validator::ValidateWith, ModuleTypeData, ValidationResult,
};
use stc_ts_module_loader::ModuleGraph;
use stc_ts_storage::{ErrorStore, File, Group, Single};
use stc_ts_types::{ModuleId, Type};
use stc_ts_utils::StcComments;
use stc_utils::{cache::Freeze, early_error, panic_ctx};
use std::{mem::take, sync::Arc, time::Instant};
use swc_atoms::JsWord;
use swc_common::{errors::Handler, FileName, SourceMap, Spanned, DUMMY_SP};
use swc_ecma_ast::Module;
use swc_ecma_loader::resolve::Resolve;
use swc_ecma_parser::TsConfig;
use swc_ecma_transforms::resolver::ts_resolver;
use swc_ecma_visit::FoldWith;
use tracing::{info, warn};

mod typings;

/// Onc instance per swc::Compiler
pub struct Checker {
    cm: Arc<SourceMap>,
    handler: Arc<Handler>,
    /// Cache
    module_types: RwLock<FxHashMap<ModuleId, Arc<OnceCell<Type>>>>,

    declared_modules: RwLock<Vec<(ModuleId, Type)>>,

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
        cm.new_source_file(FileName::Anon, "".into());

        Checker {
            env: env.clone(),
            cm: cm.clone(),
            handler,
            module_types: Default::default(),
            dts_modules: Default::default(),
            module_graph: Arc::new(ModuleGraph::new(
                cm,
                Default::default(),
                resolver,
                parser_config,
                env.target(),
            )),
            started: Default::default(),
            errors: Default::default(),
            debugger,
            declared_modules: Default::default(),
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
    pub fn get_types(&self, id: ModuleId) -> Option<Type> {
        let lock = self.module_types.read();
        lock.get(&id).and_then(|v| v.get().cloned())
    }

    /// Removes dts module from `self` and return it.
    pub fn take_dts(&self, id: ModuleId) -> Option<Module> {
        self.dts_modules.remove(&id).map(|v| v.1.into_orig())
    }

    pub fn id(&self, path: &Arc<FileName>) -> ModuleId {
        self.module_graph.id(path)
    }

    /// After calling this method, you can get errors using `.take_errors()`
    pub fn check(&self, entry: Arc<FileName>) -> ModuleId {
        self.run(|| {
            let start = Instant::now();

            let id = self.module_graph.load_all(&entry).unwrap();

            let end = Instant::now();
            log::info!("Loading of `{}` and dependencies took {:?}", entry, end - start);

            let start = Instant::now();

            self.analyze_module(None, entry.clone());

            let end = Instant::now();
            log::info!("Analysis of `{}` and dependencies took {:?}", entry, end - start);

            id
        })
    }

    pub fn take_errors(&mut self) -> Vec<Error> {
        take(self.errors.get_mut())
    }

    /// Analyzes one module.
    fn analyze_module(&self, starter: Option<Arc<FileName>>, path: Arc<FileName>) -> Type {
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
                                self.module_graph.comments().clone(),
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

                            // TODO(kdy1): Prevent duplicate work.
                            match self.dts_modules.insert(*id, dts_module) {
                                Some(..) => {
                                    warn!("Duplicated work: `{}`: (.d.ts already computed)", path);
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
                                let type_info = Type::Module(stc_ts_types::Module {
                                    span: DUMMY_SP,
                                    name: RTsModuleName::Str(RStr {
                                        span: DUMMY_SP,
                                        value: format!("{:?}", module_id).into(),
                                        has_escape: false,
                                        kind: Default::default(),
                                    }),
                                    exports: box data,
                                    metadata: Default::default(),
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
                    return lock.get(&id).map(|cell| cell.get().cloned()).flatten().unwrap();
                }
            }
            info!(
                "Request: {}\nRequested by {:?}\nCircular set: {:?}",
                path, starter, circular_set
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
                if !did_work {
                    log::warn!("Waited for {}: {:?}", path, dur);
                }

                res
            }
        })
    }

    fn analyze_non_circular_module(&self, id: ModuleId, path: Arc<FileName>) -> Type {
        self.run(|| {
            let _panic = panic_ctx!(format!("analyze_non_circular_module({})", path));

            let start = Instant::now();

            let is_dts = match &*path {
                FileName::Real(path) => path.to_string_lossy().ends_with(".d.ts"),
                _ => false,
            };

            let mut node_id_gen = NodeIdGenerator::default();
            let mut module = self
                .module_graph
                .clone_module(id)
                .unwrap_or_else(|| unreachable!("Module graph does not contains {:?}: {}", id, path));
            module = module.fold_with(&mut ts_resolver(self.env.shared().marks().top_level_mark()));

            let _panic = panic_ctx!(format!("Span of module = ({:?})", module.span));

            let mut module = RModule::from_orig(&mut node_id_gen, module);

            let mut storage = Single {
                parent: None,
                id,
                path: path.clone(),
                info: Default::default(),
                is_dts,
            };
            let mut mutations;
            {
                let mut a = Analyzer::root(
                    self.env.clone(),
                    self.cm.clone(),
                    self.module_graph.comments().clone(),
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

            let type_info = Type::Module(stc_ts_types::Module {
                span: module.span,
                name: RTsModuleName::Str(RStr {
                    span: DUMMY_SP,
                    value: format!("{:?}", id).into(),
                    has_escape: false,
                    kind: Default::default(),
                }),
                exports: box storage.info.exports,
                metadata: Default::default(),
            })
            .freezed();

            self.dts_modules.insert(id, module);

            let dur = Instant::now() - start;
            eprintln!("[Timing] Full analysis of {}: {:?}", path, dur);

            type_info
        })
    }
}

impl Load for Checker {
    fn module_id(&self, base: &Arc<FileName>, src: &JsWord) -> Option<ModuleId> {
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

    fn load_circular_dep(&self, base: ModuleId, dep: ModuleId, _partial: &ModuleTypeData) -> ValidationResult {
        let base_path = self.module_graph.path(base);
        let dep_path = self.module_graph.path(dep);

        let data = self.analyze_module(Some(base_path.clone()), dep_path.clone());

        return Ok(data);
    }

    fn load_non_circular_dep(&self, base: ModuleId, dep: ModuleId) -> ValidationResult {
        let base_path = self.module_graph.path(base);
        let dep_path = self.module_graph.path(dep);

        if matches!(&*dep_path, FileName::Custom(..)) {
            let ty = self
                .declared_modules
                .read()
                .iter()
                .find_map(|(v, ty)| if *v == dep { Some(ty.clone()) } else { None });

            if let Some(ty) = ty {
                return Ok(ty);
            }
        }

        info!("({}): Loading {}", base_path, dep_path);

        let data = self.analyze_module(Some(base_path.clone()), dep_path.clone());

        return Ok(data);
    }

    fn declare_module(&self, name: &JsWord, module: Type) {
        module.assert_clone_cheap();

        let module_id = self.module_graph.id_for_declare_module(name);

        info!("Declaring module with type `{}`", name);
        self.declared_modules.write().push((module_id, module));
    }
}
