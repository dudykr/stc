#![deny(warnings)]

use std::{mem::take, sync::Arc};

use anyhow::{anyhow, bail, Error};
use dashmap::DashMap;
use fxhash::FxBuildHasher;
use parking_lot::{Mutex, RwLock};
use rayon::prelude::*;
use stc_ts_types::{module_id::ModuleIdGenerator, ModuleId};
use stc_utils::panic_ctx;
use swc_atoms::JsWord;
use swc_common::{collections::AHashMap, comments::Comments, FileName, SourceMap, DUMMY_SP};
use swc_ecma_ast::{EsVersion, Module};
use swc_ecma_loader::resolve::Resolve;
use swc_ecma_parser::{lexer::Lexer, Parser, StringInput, Syntax, TsConfig};
use swc_fast_graph::digraph::FastDiGraphMap;
use swc_graph_analyzer::{DepGraph, GraphAnalyzer};
use tracing::{debug, error};

use self::analyzer::find_modules_and_deps;
use crate::resolvers::typescript::TsResolver;

mod analyzer;
pub mod resolvers;

#[derive(Debug, Clone)]
struct ModuleRecord {
    pub module: Arc<Module>,
    pub deps: Vec<ModuleId>,
}

/// # Implementation note
///
/// This module loader works by
///
/// 1. Collect deps recursively (in parallel)
/// 2. Load all resolved dependencies (in parallel)
/// 3. Handle all `declare module` statements (in parallel)
/// 4. Load all modules again, but this time with all deps resolved. (in
/// parallel)
/// 5. Build a dependency graph.
///
///
/// Double-loading is required because we have to handle all `declare module`
/// statements to get all dependencies resolved.
pub struct ModuleGraph<C, R>
where
    C: Comments + Send + Sync,
    R: Resolve,
{
    cm: Arc<SourceMap>,
    parser_config: TsConfig,
    target: EsVersion,
    comments: C,

    id_generator: ModuleIdGenerator,
    loaded: DashMap<ModuleId, Result<ModuleRecord, ()>, FxBuildHasher>,
    resolver: TsResolver<R>,

    errors: Mutex<Vec<Error>>,
    parsing_errors: Mutex<Vec<swc_ecma_parser::error::Error>>,
    deps: RwLock<DepGraphData>,

    parse_cache: Mutex<AHashMap<Arc<FileName>, Arc<Module>>>,
}
#[derive(Default)]
struct DepGraphData {
    pub all: Vec<ModuleId>,
    pub graph: FastDiGraphMap<ModuleId, ()>,
    pub cycles: Vec<Vec<ModuleId>>,
}

struct LoadResult {
    module: Arc<Module>,
    deps: Vec<Arc<FileName>>,
}

impl<C, R> ModuleGraph<C, R>
where
    C: Comments + Send + Sync,
    R: Resolve,
{
    pub fn new(cm: Arc<SourceMap>, comments: C, resolver: R, parser_config: TsConfig, target: EsVersion) -> Self {
        ModuleGraph {
            cm,
            parser_config,
            target,
            comments,
            id_generator: Default::default(),
            loaded: Default::default(),
            resolver: TsResolver::new(resolver),
            errors: Default::default(),
            parsing_errors: Default::default(),
            deps: Default::default(),
            parse_cache: Default::default(),
        }
    }

    pub fn comments(&self) -> &C {
        &self.comments
    }

    /// TODO: Fix race condition of `errors`.
    pub fn load_all(&self, entry: &Arc<FileName>) -> Result<ModuleId, (ModuleId, Error)> {
        self.load_including_deps(entry, false);
        self.load_including_deps(entry, true);

        let module_id = self.id_generator.generate(entry);

        let res = {
            let mut analyzer = GraphAnalyzer::new(&*self);
            analyzer.load(module_id);
            analyzer.into_result()
        };

        {
            let mut deps = self.deps.write();

            deps.all.extend(res.all);
            deps.cycles.extend(res.cycles);

            for n in res.graph.nodes() {
                deps.graph.add_node(n);
            }
            for (a, b, _) in res.graph.all_edges() {
                deps.graph.add_edge(a, b, ());
            }
        }

        let errors = take(&mut *self.errors.lock());
        if !errors.is_empty() {
            let err = anyhow!(
                "failed load modules:\n{}",
                errors.iter().map(|s| format!("{:?}", s)).collect::<Vec<_>>().join("\n")
            );
            return Err((module_id, err));
        }

        Ok(module_id)
    }

    pub fn id_for_declare_module(&self, module_name: &JsWord) -> ModuleId {
        self.id_generator.generate(&Arc::new(FileName::Custom(module_name.to_string())))
    }

    pub fn path(&self, id: ModuleId) -> Arc<FileName> {
        self.id_generator.path(id)
    }

    pub fn get_circular(&self, id: ModuleId) -> Option<Vec<ModuleId>> {
        let deps = self.deps.read();

        deps.cycles
            .iter()
            .find_map(|set| if set.contains(&id) { Some(set) } else { None })
            .cloned()
    }

    pub fn id(&self, path: &Arc<FileName>) -> ModuleId {
        self.id_generator.generate(path)
    }

    pub fn resolve(&self, base: &FileName, specifier: &JsWord) -> Result<Arc<FileName>, Error> {
        self.resolver.resolve(base, specifier)
    }

    fn with_module<F, Ret>(&self, id: ModuleId, f: F) -> Ret
    where
        F: FnOnce(Option<&Module>) -> Ret,
    {
        let m = self.loaded.get(&id);

        match m.as_deref() {
            Some(m) => match m {
                Ok(v) => f(Some(&v.module)),
                Err(..) => {
                    error!("`self.loaded` did not contain `id`: {:?}", id);

                    f(Some(&Module {
                        span: DUMMY_SP,
                        body: Default::default(),
                        shebang: Default::default(),
                    }))
                }
            },
            None => f(None),
        }
    }

    pub fn clone_module(&self, id: ModuleId) -> Option<Module> {
        self.with_module(id, |m| m.cloned())
    }

    pub fn stmt_count_of(&self, id: ModuleId) -> usize {
        self.with_module(id, |m| m.map(|v| v.body.len()).unwrap_or(0))
    }

    fn load_including_deps(&self, path: &Arc<FileName>, resolve_all: bool) {
        let id = self.id_generator.generate(path);

        let loaded = self.load(path, resolve_all);
        let loaded = match loaded {
            Ok(v) => v,
            Err(err) => {
                if resolve_all {
                    error!("failed to load module: {:?}", err);

                    self.errors.lock().push(err);

                    self.loaded.insert(id, Err(()));
                }

                return;
            }
        };

        let loaded = match loaded {
            Some(v) => v,
            None => return,
        };

        #[cfg(feature = "no-threading")]
        let iter = loaded.deps.into_iter();
        #[cfg(not(feature = "no-threading"))]
        let iter = loaded.deps.into_par_iter();

        let dep_module_ids = iter
            .map(|dep_path| {
                let id = self.id_generator.generate(&dep_path);

                self.load_including_deps(&dep_path, resolve_all);
                id
            })
            .collect::<Vec<_>>();

        if resolve_all {
            let _res = self.loaded.insert(
                id,
                Ok(ModuleRecord {
                    module: loaded.module,
                    deps: dep_module_ids,
                }),
            );
            // assert_eq!(res, None, "duplicate?");
        }
    }

    /// Returns `Ok(None)` if it's already loaded.
    ///
    /// Note that this methods does not modify `self.loaded`.
    fn load(&self, filename: &Arc<FileName>, resolve_all: bool) -> Result<Option<LoadResult>, Error> {
        let module_id = self.id_generator.generate(filename);

        if self.loaded.contains_key(&module_id) {
            return Ok(None);
        }

        debug!("Loading {:?}: {}", module_id, filename);

        // TODO(kdy1): Check if it's better to use content of `declare module "http"`?
        if resolve_all {
            match &**filename {
                FileName::Real(..) => {}
                _ => {
                    return Ok(Some(LoadResult {
                        module: Arc::new(Module {
                            span: DUMMY_SP,
                            body: Default::default(),
                            shebang: Default::default(),
                        }),
                        deps: Default::default(),
                    }))
                }
            }
        }

        let module = self.load_one_module(filename)?;

        let _panic = panic_ctx!(format!("ModuleGraph.load({}, span = {:?})", filename, module.span));

        let (declared_modules, deps) = find_modules_and_deps(&self.comments, &module);

        for decl in declared_modules {
            self.resolver.declare_module(decl);
        }

        let resolver = &self.resolver;

        let deps = if resolve_all {
            deps.into_par_iter()
                .map(|specifier| resolver.resolve(filename, &specifier))
                .filter_map(|res| res.ok())
                .collect()
        } else {
            deps.into_par_iter()
                .map(|specifier| resolver.resolve(filename, &specifier))
                .filter_map(|res| res.ok())
                .collect()
        };

        log::debug!("Loaded {:?}: {}", module_id, filename);

        Ok(Some(LoadResult { module, deps }))
    }

    fn load_one_module(&self, filename: &Arc<FileName>) -> Result<Arc<Module>, Error> {
        if let Some(cache) = self.parse_cache.lock().get(filename).cloned() {
            return Ok(cache);
        }

        let path = match &**filename {
            FileName::Real(path) => path,
            _ => {
                bail!("cannot load `{:?}`", filename)
            }
        };

        let fm = self.cm.load_file(&path)?;
        let lexer = Lexer::new(
            Syntax::Typescript(TsConfig {
                dts: path.as_os_str().to_string_lossy().ends_with(".d.ts"),
                tsx: path.extension().map(|v| v == "tsx").unwrap_or(false),
                ..self.parser_config.clone()
            }),
            self.target,
            StringInput::from(&*fm),
            Some(&self.comments),
        );

        let mut parser = Parser::new_from(lexer);
        let result = parser.parse_module();

        let module = match result {
            Ok(v) => v,
            Err(err) => {
                let mut errors = self.parsing_errors.lock();
                errors.push(err);

                bail!("Failed to parse {}", path.display())
            }
        };
        let extra_errors = parser.take_errors();
        if !extra_errors.is_empty() {
            let mut errors = self.parsing_errors.lock();
            errors.extend(extra_errors);
        }

        let module = Arc::new(module);
        self.parse_cache.lock().insert(filename.clone(), module.clone());

        Ok(module)
    }
}

impl<C, R> DepGraph for ModuleGraph<C, R>
where
    C: Comments + Send + Sync,
    R: Resolve,
{
    type ModuleId = ModuleId;

    fn deps_of(&self, module_id: Self::ModuleId) -> Vec<Self::ModuleId> {
        let m = self
            .loaded
            .get(&module_id)
            .unwrap_or_else(|| unreachable!("{:?} is not loaded", module_id));

        match &*m {
            Ok(m) => m.deps.clone(),
            Err(..) => Default::default(),
        }
    }
}
