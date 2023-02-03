use std::sync::{Arc, Mutex, RwLock};

use anyhow::{bail, Context, Result};
use auto_impl::auto_impl;
use dashmap::{DashMap, DashSet};
use fxhash::FxBuildHasher;
use petgraph::algo::kosaraju_scc;
use rayon::prelude::*;
use stc_ts_env::Env;
use stc_ts_types::{module_id::ModuleIdGenerator, ModuleId};
use stc_ts_utils::StcComments;
use swc_common::{FileName, SourceMap, SyntaxContext, GLOBALS};
use swc_ecma_ast::{EsVersion, Module};
use swc_ecma_loader::resolve::Resolve;
use swc_ecma_parser::{lexer::Lexer, Parser, StringInput, Syntax, TsConfig};
use swc_ecma_visit::VisitMutWith;
use swc_fast_graph::digraph::FastDiGraphMap;

use self::analyzer::find_modules_and_deps;

mod analyzer;
pub mod store;

pub struct ModuleRecord {
    pub id: ModuleId,
    pub filename: Arc<FileName>,
    pub top_level_ctxt: SyntaxContext,
    pub ast: Module,
}

pub struct Records {
    pub modules: Vec<Arc<ModuleRecord>>,
    pub entry: Arc<ModuleRecord>,
    pub comments: StcComments,
}

/// A module loader.
#[auto_impl(&, Box, Arc)]
pub trait LoadModule: 'static + Send + Sync {
    /// This method should
    ///
    /// - Should never return empty vector.
    /// - Return **all modules in a cycle**.
    /// - Handle `declare module "foo"`.
    /// - Apply `resolver`.
    ///
    /// ## Tip
    ///
    /// Because of the cycles, this method would load all dependencies
    /// recursively.
    fn load_module(&self, filename: &Arc<FileName>, is_entry: bool) -> Result<Records>;

    /// Same constraints for [`LoadModule::load_module`] applies.
    fn load_dep(&self, base: &Arc<FileName>, module_specifier: &str) -> Result<Records>;
}

/// A simple implementation of [LoadModule].
pub struct ModuleLoader<R>
where
    R: 'static + Sync + Send + Resolve,
{
    cm: Arc<SourceMap>,
    env: Env,
    resolver: R,

    /// TODO(kdu1): Split the
    comments: StcComments,
    loading_started: DashSet<Arc<FileName>, FxBuildHasher>,
    dep_graph: RwLock<FastDiGraphMap<ModuleId, ()>>,
    cycles: RwLock<Vec<Vec<ModuleId>>>,

    ids: ModuleIdGenerator,
    parse_cache: DashMap<Arc<FileName>, (Arc<ModuleRecord>, StcComments), FxBuildHasher>,
    parsing_errors: Mutex<Vec<swc_ecma_parser::error::Error>>,
}

impl<R> ModuleLoader<R>
where
    R: Resolve,
{
    pub fn new(cm: Arc<SourceMap>, env: Env, resolver: R) -> Self {
        Self {
            cm,
            env,
            resolver,

            comments: Default::default(),
            loading_started: Default::default(),
            dep_graph: Default::default(),
            cycles: Default::default(),
            parse_cache: Default::default(),
            ids: Default::default(),
            parsing_errors: Default::default(),
        }
    }

    fn load_recursively(&self, filename: &Arc<FileName>, calc_cycles: bool) -> Result<ModuleId> {
        let (id, _) = self.ids.generate(filename);

        // This function works only once per file.
        if !self.loading_started.insert(filename.clone()) {
            return Ok(id);
        }

        let (entry, comments) = self.parse(filename).context("failed to parse entry")?;

        let (declared_modules, deps) = find_modules_and_deps(&comments, &entry.ast);

        let deps: Vec<ModuleId> = GLOBALS.with(|globals| {
            deps.par_iter()
                .map(|dep| {
                    GLOBALS.set(globals, || {
                        let dep_path = Arc::new(self.resolver.resolve(filename, dep)?);

                        self.load_recursively(&dep_path, false)
                    })
                })
                .collect::<Result<Vec<_>>>()
        })?;

        {
            // Add to the dependency graph

            let mut g = self.dep_graph.write().unwrap();

            for dep in deps.iter() {
                g.add_edge(id, *dep, ());
            }
        }

        if calc_cycles {
            let new = {
                let g = self.dep_graph.read().unwrap();
                kosaraju_scc(&*g)
            };

            let mut cycles = self.cycles.write().unwrap();
            for cycle in new {
                if cycle.len() > 1 {
                    cycles.push(cycle);
                }
            }
        }

        Ok(id)
    }

    fn parse(&self, filename: &Arc<FileName>) -> Result<(Arc<ModuleRecord>, StcComments)> {
        if let Some(cached) = self.parse_cache.get(filename).as_deref().cloned() {
            return Ok(cached);
        }

        let record = self.parse_inner(filename)?;
        self.parse_cache.insert(filename.clone(), record.clone());

        Ok(record)
    }

    /// This does not perform caching
    fn parse_inner(&self, filename: &Arc<FileName>) -> Result<(Arc<ModuleRecord>, StcComments)> {
        let comments = self.comments.clone();

        let (fm, syntax) = match &**filename {
            FileName::Real(path) => {
                let fm = self
                    .cm
                    .load_file(path)
                    .with_context(|| format!("failed to load module `{}`", path.display()))?;

                let syntax = Syntax::Typescript(TsConfig {
                    dts: path.as_os_str().to_string_lossy().ends_with(".d.ts"),
                    tsx: path.extension().map(|v| v == "tsx").unwrap_or(false),
                    ..Default::default()
                });

                (fm, syntax)
            }

            _ => {
                bail!("failed to load module `{}`", filename);
            }
        };

        let lexer = Lexer::new(syntax, EsVersion::latest(), StringInput::from(&*fm), Some(&comments));

        let mut parser = Parser::new_from(lexer);
        let result = parser.parse_module();

        let mut ast = match result {
            Ok(v) => v,
            Err(err) => {
                let mut errors = self.parsing_errors.lock().unwrap();
                errors.push(err);

                bail!("Failed to parse {}", filename)
            }
        };
        let extra_errors = parser.take_errors();
        if !extra_errors.is_empty() {
            let mut errors = self.parsing_errors.lock().unwrap();
            errors.extend(extra_errors);
        }

        let (id, top_level_mark) = self.ids.generate(filename);
        let top_level_ctxt = SyntaxContext::empty().apply_mark(top_level_mark);

        ast.visit_mut_with(&mut swc_ecma_transforms_base::resolver(
            self.env.shared().marks().unresolved_mark(),
            top_level_mark,
            true,
        ));

        Ok((
            Arc::new(ModuleRecord {
                id,
                filename: filename.clone(),
                top_level_ctxt,
                ast,
            }),
            comments,
        ))
    }
}

impl<R> LoadModule for ModuleLoader<R>
where
    R: 'static + Sync + Send + Resolve,
{
    fn load_module(&self, filename: &Arc<FileName>, is_entry: bool) -> Result<Records> {
        let entry_id = self
            .load_recursively(filename, is_entry)
            .with_context(|| format!("failed to load `{}` recursively", filename))?;

        let cycle = {
            let cycles = self.cycles.read().unwrap();
            cycles.iter().find(|c| c.contains(&entry_id)).cloned()
        };

        let (entry, comments) = self.parse(filename).context("failed to parse entry")?;

        match cycle {
            Some(cycle) => {
                let modules = cycle
                    .iter()
                    .copied()
                    .map(|id| {
                        let path = self.ids.path(id);

                        let (entry, _) = self.parse(&path)?;

                        Ok(entry)
                    })
                    .collect::<Result<_>>()?;

                Ok(Records { modules, entry, comments })
            }
            None => Ok(Records {
                modules: vec![entry.clone()],
                entry,
                comments,
            }),
        }
    }

    fn load_dep(&self, base: &Arc<FileName>, module_specifier: &str) -> Result<Records> {
        let filename = self
            .resolver
            .resolve(base, module_specifier)
            .with_context(|| format!("failed to resolve `{}` from `{}`", module_specifier, base))?;

        self.load_module(&Arc::new(filename), false)
    }
}
