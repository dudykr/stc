#![deny(warnings)]

use crate::resolvers::typescript::TsResolver;

use self::deps::find_deps;
use anyhow::{bail, Error};
use dashmap::DashMap;
use fxhash::FxBuildHasher;
use parking_lot::{Mutex, RwLock};
use rayon::prelude::*;
use stc_ts_types::{module_id, ModuleId};
use std::{
    mem::take,
    path::{Path, PathBuf},
    sync::Arc,
};
use swc_atoms::JsWord;
use swc_common::{comments::Comments, FileName, SourceMap, DUMMY_SP};
use swc_ecma_ast::{EsVersion, Module};
use swc_ecma_loader::resolve::Resolve;
use swc_ecma_parser::{lexer::Lexer, Parser, StringInput, Syntax, TsConfig};
use swc_fast_graph::digraph::FastDiGraphMap;
use swc_graph_analyzer::{DepGraph, GraphAnalyzer};

mod deps;
pub mod resolvers;

#[derive(Debug, Clone)]
struct ModuleRecord {
    pub module: Arc<Module>,
    pub deps: Vec<ModuleId>,
}

pub struct ModuleGraph<C, R>
where
    C: Comments + Send + Sync,
    R: Resolve,
{
    cm: Arc<SourceMap>,
    parser_config: TsConfig,
    target: EsVersion,
    comments: Option<C>,

    id_generator: module_id::Generator,
    paths: DashMap<ModuleId, Arc<FileName>, FxBuildHasher>,
    loaded: DashMap<ModuleId, Result<ModuleRecord, ()>, FxBuildHasher>,
    resolver: TsResolver<R>,

    errors: Mutex<Vec<Error>>,
    parsing_errors: Mutex<Vec<swc_ecma_parser::error::Error>>,
    deps: RwLock<Deps>,
}
#[derive(Default)]
struct Deps {
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
    pub fn new(
        cm: Arc<SourceMap>,
        comments: Option<C>,
        resolver: R,
        parser_config: TsConfig,
        target: EsVersion,
    ) -> Self {
        Self {
            cm,
            comments,
            parser_config,
            target,
            id_generator: Default::default(),
            loaded: Default::default(),
            resolver: TsResolver::new(resolver),
            parsing_errors: Default::default(),
            deps: Default::default(),
            paths: Default::default(),
            errors: Default::default(),
        }
    }

    /// TODO: Fix race condition of `errors`.
    pub fn load_all(&self, entry: &Arc<PathBuf>) -> Result<ModuleId, Error> {
        self.load_including_deps(entry);

        let (_, module_id) = self.id_generator.generate(entry);

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
            bail!(
                "failed load modules:\n{}",
                errors.iter().map(|s| format!("{:?}", s)).collect::<Vec<_>>().join("\n")
            );
        }

        Ok(module_id)
    }

    pub fn path(&self, id: ModuleId) -> Arc<PathBuf> {
        self.paths.get(&id).unwrap().clone()
    }

    pub fn get_circular(&self, id: ModuleId) -> Option<Vec<ModuleId>> {
        let deps = self.deps.read();

        deps.cycles
            .iter()
            .find_map(|set| if set.contains(&id) { Some(set) } else { None })
            .cloned()
    }

    pub fn id(&self, path: &Arc<PathBuf>) -> ModuleId {
        let res = self.id_generator.generate(path);
        self.paths.insert(res.1, path.clone());
        res.1
    }

    pub fn resolve(&self, base: &Path, specifier: &JsWord) -> Result<Arc<FileName>, Error> {
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
                Err(_) => f(Some(&Module {
                    span: DUMMY_SP,
                    body: Default::default(),
                    shebang: Default::default(),
                })),
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

    fn load_including_deps(&self, path: &Arc<FileName>) {
        let (_, id) = self.id_generator.generate(path);

        let loaded = self.load(path);
        let loaded = match loaded {
            Ok(v) => v,
            Err(err) => {
                self.errors.lock().push(err);

                self.loaded.insert(id, Err(()));
                return;
            }
        };

        let loaded = match loaded {
            Some(v) => v,
            None => return,
        };

        self.paths.insert(id, path.clone());

        let dep_module_ids = loaded
            .deps
            .into_par_iter()
            .map(|dep_path| {
                let id = self.id_generator.generate(&dep_path).1;
                self.paths.insert(id, dep_path.clone());

                self.load_including_deps(&dep_path);
                id
            })
            .collect::<Vec<_>>();

        let _res = self.loaded.insert(
            id,
            Ok(ModuleRecord {
                module: loaded.module,
                deps: dep_module_ids,
            }),
        );
        // assert_eq!(res, None, "duplicate?");
    }

    /// Returns `Ok(None)` if it's already loaded.
    ///
    /// Note that this methods does not modify `self.loaded`.
    fn load(&self, path: &Arc<PathBuf>) -> Result<Option<LoadResult>, Error> {
        let (_new, module_id) = self.id_generator.generate(path);

        if self.loaded.contains_key(&module_id) {
            return Ok(None);
        }

        log::debug!("Loading {:?}: {}", module_id, path.display());

        let fm = self.cm.load_file(&path)?;
        let lexer = Lexer::new(
            Syntax::Typescript(TsConfig {
                dts: path.as_os_str().to_string_lossy().ends_with(".d.ts"),
                tsx: path.extension().map(|v| v == "tsx").unwrap_or(false),
                ..self.parser_config.clone()
            }),
            self.target,
            StringInput::from(&*fm),
            self.comments.as_ref().map(|v| v as _),
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

        let deps = find_deps(&module);
        let module = Arc::new(module);

        let resolver = &self.resolver;
        let deps = deps
            .into_par_iter()
            .map(|specifier| resolver.resolve(path, &specifier))
            .collect::<Result<Vec<_>, _>>()?;

        log::debug!("Loaded {:?}: {}", module_id, path.display());

        Ok(Some(LoadResult { module, deps }))
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
