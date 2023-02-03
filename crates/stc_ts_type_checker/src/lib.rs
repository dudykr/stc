//! Full type checker with dependency support.
#![feature(box_syntax)]

use std::{mem::take, sync::Arc, time::Instant};

use dashmap::{DashMap, DashSet, SharedValue};
use fxhash::{FxBuildHasher, FxHashMap};
use once_cell::sync::OnceCell;
use parking_lot::{Mutex, RwLock};
use rnode::{NodeIdGenerator, RNode, VisitWith};
use stc_ts_ast_rnode::{RModule, RStr, RTsModuleName};
use stc_ts_dts::{apply_mutations, cleanup_module_for_dts};
use stc_ts_env::Env;
use stc_ts_errors::{debug::debugger::Debugger, Error};
use stc_ts_file_analyzer::{analyzer::Analyzer, loader::Load, validator::ValidateWith, ModuleTypeData, VResult};
use stc_ts_module_loader::ModuleGraph;
use stc_ts_storage::{ErrorStore, File, Group, Single};
use stc_ts_types::{ModuleId, Type};
use stc_ts_utils::StcComments;
use stc_utils::{cache::Freeze, early_error};
use swc_atoms::JsWord;
use swc_common::{errors::Handler, FileName, SourceMap, Spanned, SyntaxContext, DUMMY_SP};
use swc_ecma_ast::Module;
use swc_ecma_loader::resolve::Resolve;
use swc_ecma_parser::TsConfig;
use swc_ecma_transforms::resolver;
use swc_ecma_visit::FoldWith;
use tracing::{info, warn};

use crate::store::ModuleStore;

mod store;
mod typings;

/// Onc instance per swc::Compiler
pub struct Checker {
    cm: Arc<SourceMap>,
    handler: Arc<Handler>,
    /// Cache
    module_types: RwLock<FxHashMap<ModuleId, Arc<OnceCell<Type>>>>,

    declared_modules: RwLock<Vec<(ModuleId, Type)>>,

    /// Information required to generate `.d.ts` files.
    dts_modules: Arc<DashMap<ModuleId, RModule, FxBuildHasher>>,

    store: ModuleStore,

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
            started: Default::default(),
            errors: Default::default(),
            debugger,
            declared_modules: Default::default(),
        }
    }
}

impl Checker {
    /// Get type information of a module.
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
        let start = Instant::now();

        let id = self.module_graph.load_all(&entry);

        let end = Instant::now();
        log::debug!("Loading of `{}` and dependencies took {:?}", entry, end - start);

        let start = Instant::now();

        self.analyze_module(None, entry.clone());

        let end = Instant::now();
        log::debug!("Analysis of `{}` and dependencies took {:?}", entry, end - start);

        id.unwrap_or_else(|(id, _)| id)
    }

    pub fn take_errors(&mut self) -> Vec<Error> {
        take(self.errors.get_mut())
    }

    /// Analyzes one module.
    fn analyze_module(&self, starter: Option<Arc<FileName>>, path: Arc<FileName>) -> Type {
        let id = self.module_graph.id(&path);

        {
            let lock = self.module_types.read();
            // If a circular chunks are fully analyzed, used them.
            if let Some(full) = lock.get(&id).and_then(|cell| cell.get().cloned()) {
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
                                    let top_level_mark = self.module_graph.top_level_mark(id);
                                    File {
                                        id,
                                        path,
                                        stmt_count,
                                        top_level_ctxt: SyntaxContext::empty().apply_mark(top_level_mark),
                                    }
                                })
                                .collect(),
                        ),
                        errors: Default::default(),
                        info: Default::default(),
                    };
                    let ids = set.to_vec();
                    let modules = ids
                        .iter()
                        .map(|&id| (id, self.module_graph.clone_module(id)))
                        .filter_map(|m| m.1.map(|v| (m.0, v)))
                        .map(|(module_id, module)| {
                            RModule::from_orig(
                                &mut node_id_gen,
                                module.fold_with(&mut resolver(
                                    self.env.shared().marks().unresolved_mark(),
                                    self.module_graph.top_level_mark(module_id),
                                    true,
                                )),
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
                            cleanup_module_for_dts(&mut dts_module.body, type_data);
                        }

                        // TODO(kdy1): Prevent duplicate work.
                        if let Some(..) = self.dts_modules.insert(*id, dts_module) {
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
        }
        info!("Request: {}\nRequested by {:?}\nCircular set: {:?}", path, starter, circular_set);

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
        let mut module = self
            .module_graph
            .clone_module(module_id)
            .unwrap_or_else(|| unreachable!("Module graph does not contains {:?}: {}", module_id, path));
        let top_level_mark = self.module_graph.top_level_mark(module_id);
        module = module.fold_with(&mut resolver(self.env.shared().marks().unresolved_mark(), top_level_mark, true));

        let mut module = RModule::from_orig(&mut node_id_gen, module);

        let mut storage = Single {
            parent: None,
            id: module_id,
            top_level_ctxt: SyntaxContext::empty().apply_mark(top_level_mark),
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
                self.module_graph.comments().clone(),
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

impl Load for Checker {
    fn module_id(&self, base: &Arc<FileName>, src: &JsWord) -> Option<ModuleId> {
        let path = self.module_graph.resolve(base, src).ok()?;
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

    fn load_circular_dep(&self, base: ModuleId, dep: ModuleId, _partial: &ModuleTypeData) -> VResult<Type> {
        let base_path = self.module_graph.path(base);
        let dep_path = self.module_graph.path(dep);

        let data = self.analyze_module(Some(base_path), dep_path);

        Ok(data)
    }

    fn load_non_circular_dep(&self, base: ModuleId, dep: ModuleId) -> VResult<Type> {
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

        let data = self.analyze_module(Some(base_path), dep_path);

        Ok(data)
    }

    fn declare_module(&self, name: &JsWord, module: Type) {
        module.assert_clone_cheap();

        let module_id = self.module_graph.id_for_declare_module(name);

        info!("Declaring module with type `{}`", name);
        self.declared_modules.write().push((module_id, module));
    }
}
