#![feature(option_expect_none)]

use self::deps::find_deps;
use self::resolver::Resolve;
use anyhow::bail;
use anyhow::Context;
use anyhow::Error;
use dashmap::DashMap;
use fxhash::FxBuildHasher;
use fxhash::FxHashSet;
use parking_lot::Mutex;
use parking_lot::RwLock;
use petgraph::{algo::all_simple_paths, graphmap::DiGraphMap};
use rayon::prelude::*;
use stc_ts_types::module_id;
use stc_ts_types::ModuleId;
use std::collections::HashSet;
use std::path::Path;
use std::path::PathBuf;
use std::sync::Arc;
use swc_atoms::JsWord;
use swc_common::comments::Comments;
use swc_common::SourceMap;
use swc_ecma_ast::Module;
use swc_ecma_parser::lexer::Lexer;
use swc_ecma_parser::JscTarget;
use swc_ecma_parser::Parser;
use swc_ecma_parser::StringInput;
use swc_ecma_parser::Syntax;
use swc_ecma_parser::TsConfig;

mod deps;
pub mod resolver;

pub struct ModuleGraph<C, R>
where
    C: Comments + Send + Sync,
    R: Resolve,
{
    cm: Arc<SourceMap>,
    parser_config: TsConfig,
    target: JscTarget,
    comments: Option<C>,

    id_generator: module_id::Generator,
    paths: DashMap<ModuleId, Arc<PathBuf>, FxBuildHasher>,
    loaded: DashMap<ModuleId, Arc<Module>, FxBuildHasher>,
    resolver: R,

    parsing_errors: Mutex<Vec<swc_ecma_parser::error::Error>>,
    deps: RwLock<Deps>,
}
#[derive(Default)]
struct Deps {
    graph: DiGraphMap<ModuleId, ()>,
    circular: Vec<FxHashSet<ModuleId>>,
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
        target: JscTarget,
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
        self.load_including_deps(entry)
            .with_context(|| format!("load_including_deps failed: {}", entry.display()))?;

        let (_, module_id) = self.id_generator.generate(entry);

        Ok(module_id)
    }

    pub fn path(&self, id: ModuleId) -> Arc<PathBuf> {
        self.paths.get(&id).unwrap().clone()
    }

    pub fn get_circular(&self, id: ModuleId) -> Option<FxHashSet<ModuleId>> {
        let deps = self.deps.read();

        deps.circular
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

    pub fn clone_module(&self, id: ModuleId) -> Module {
        let m = self.loaded.get(&id).unwrap();

        (&**m).clone()
    }

    pub fn stmt_count_of(&self, id: ModuleId) -> usize {
        let m = self.loaded.get(&id).unwrap();
        m.body.len()
    }

    fn load_including_deps(&self, path: &Arc<PathBuf>) -> Result<(), Error> {
        let loaded = self.load(path)?;
        let loaded = match loaded {
            Some(v) => v,
            None => return Ok(()),
        };

        let (_, id) = self.id_generator.generate(path);
        self.paths.insert(id, path.clone());

        self.loaded.insert(id, loaded.module).expect_none("duplicate?");

        let dep_module_ids = loaded
            .deps
            .into_par_iter()
            .map(|dep_path| -> Result<_, Error> {
                self.load_including_deps(&dep_path)?;

                let id = self.id_generator.generate(&dep_path).1;
                self.paths.insert(id, dep_path.clone());
                Ok(id)
            })
            .collect::<Result<Vec<_>, _>>()?;

        let mut deps = self.deps.write();
        deps.graph.add_node(id);
        for dep_id in dep_module_ids {
            deps.graph.add_edge(id, dep_id, ());

            let cycles = all_simple_paths(&deps.graph, dep_id, id, 0, None).collect::<Vec<Vec<ModuleId>>>();

            for path in cycles {
                let set = deps.circular.iter_mut().find(|set| {
                    for &path_id in &path {
                        if set.contains(&path_id) {
                            return true;
                        }
                    }

                    false
                });

                match set {
                    Some(set) => {
                        set.extend(path);
                    }
                    None => {
                        let mut set = HashSet::default();
                        set.extend(path);
                        deps.circular.push(set);
                    }
                }
            }
        }

        Ok(())
    }

    /// Returns `Ok(None)` if it's already loaded.
    ///
    /// Note that this methods does not modify `self.loaded`.
    fn load(&self, path: &Arc<PathBuf>) -> Result<Option<LoadResult>, Error> {
        let (new, module_id) = self.id_generator.generate(path);

        if !new || self.loaded.contains_key(&module_id) {
            return Ok(None);
        }
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
            .collect::<Result<_, _>>()?;

        Ok(Some(LoadResult { module, deps }))
    }
}
