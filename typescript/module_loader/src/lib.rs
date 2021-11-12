use self::{deps::find_deps, resolver::Resolve};
use anyhow::{bail, Context, Error};
use dashmap::DashMap;
use fxhash::FxBuildHasher;
use parking_lot::{Mutex, RwLock};
use rayon::prelude::*;
use stc_ts_types::{module_id, ModuleId};
use std::{
    path::{Path, PathBuf},
    sync::Arc,
};
use swc_atoms::JsWord;
use swc_common::{comments::Comments, SourceMap};
use swc_ecma_ast::{EsVersion, Module};
use swc_ecma_parser::{lexer::Lexer, Parser, StringInput, Syntax, TsConfig};
use swc_fast_graph::digraph::FastDiGraphMap;
use swc_graph_analyzer::{DepGraph, GraphAnalyzer};
use tracing::error;

mod deps;
pub mod resolver;

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
    paths: DashMap<ModuleId, Arc<PathBuf>, FxBuildHasher>,
    loaded: DashMap<ModuleId, ModuleRecord, FxBuildHasher>,
    resolver: R,

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
    deps: Vec<Arc<PathBuf>>,
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
            resolver,
            parsing_errors: Default::default(),
            deps: Default::default(),
            paths: Default::default(),
        }
    }

    pub fn load_all(&self, entry: &Arc<PathBuf>) -> Result<ModuleId, Error> {
        let res = self.load_including_deps(entry);
        match res {
            Err(err) => {
                error!("Failed to load {}:\n{:?}", entry.display(), err);
            }
            _ => {}
        }

        let (_, module_id) = self.id_generator.generate(entry);

        let res = {
            let mut analyzer = GraphAnalyzer::new(&self);
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

    pub fn resolve(&self, base: &Path, specifier: &JsWord) -> Result<Arc<PathBuf>, Error> {
        self.resolver.resolve(base, specifier)
    }

    pub fn clone_module(&self, id: ModuleId) -> Option<Module> {
        let m = self.loaded.get(&id)?;

        Some((&*m.module).clone())
    }

    pub fn stmt_count_of(&self, id: ModuleId) -> usize {
        let m = self.loaded.get(&id).unwrap();
        m.module.body.len()
    }

    fn load_including_deps(&self, path: &Arc<PathBuf>) -> Result<(), Error> {
        let loaded = self
            .load(path)
            .with_context(|| format!("failed to load file at {}", path.display()))?;

        let loaded = match loaded {
            Some(v) => v,
            None => return Ok(()),
        };

        let (_, id) = self.id_generator.generate(path);

        self.paths.insert(id, path.clone());

        let res = loaded
            .deps
            .into_iter()
            .map(|dep_path| -> Result<_, Error> {
                let _ = self.load_including_deps(&dep_path)?;

                let id = self.id_generator.generate(&dep_path).1;
                self.paths.insert(id, dep_path.clone());
                Ok(id)
            })
            .collect::<Result<Vec<_>, _>>();
        let dep_module_ids = res?;

        let _res = self.loaded.insert(
            id,
            ModuleRecord {
                module: loaded.module,
                deps: dep_module_ids,
            },
        );
        // assert_eq!(res, None, "duplicate?");

        Ok(())
    }

    /// Returns `Ok(None)` if it's already loaded.
    ///
    /// Note that this methods does not modify `self.loaded`.
    fn load(&self, path: &Arc<PathBuf>) -> Result<Option<LoadResult>, Error> {
        let (_new, module_id) = self.id_generator.generate(path);

        if self.loaded.contains_key(&module_id) {
            return Ok(None);
        }

        log::debug!("Loading {}", path.display());

        let fm = self.cm.load_file(&path)?;
        let lexer = Lexer::new(
            Syntax::Typescript(TsConfig {
                dts: path.as_os_str().to_string_lossy().ends_with(".d.ts"),
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
        m.deps.clone()
    }
}
