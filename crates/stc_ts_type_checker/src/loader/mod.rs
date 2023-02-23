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
use swc_common::{input::SourceFileInput, FileName, SourceFile, SourceMap, Span, SyntaxContext, GLOBALS};
use swc_ecma_ast::Module;
use swc_ecma_loader::resolve::Resolve;
use swc_ecma_parser::{Parser, Syntax, TsConfig};
use swc_ecma_visit::VisitMutWith;
use swc_fast_graph::digraph::FastDiGraphMap;

use self::analyzer::find_modules_and_deps;

mod analyzer;
pub mod store;

pub struct ModuleRecord {
    pub id: ModuleId,
    pub is_dts: bool,
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
    /// - Apply `swc_ecma_transforms_base::resolver`.
    ///
    /// ## Tip
    ///
    /// Because of the cycles, this method would load all dependencies
    /// recursively.
    fn load_module(&self, filename: &Arc<FileName>, is_entry: bool) -> Result<Records>;

    /// Same constraints for [`LoadModule::load_module`] applies.
    fn load_dep(&self, base: &Arc<FileName>, module_specifier: &str) -> Result<Records>;
}

/// **NOTE**: [FileName::Custom] is not passed to this type.
pub trait LoadFile: 'static + Send + Sync {
    fn load_file(&self, cm: &Arc<SourceMap>, filename: &Arc<FileName>) -> Result<(Arc<SourceFile>, Syntax)>;
}

/// A simple implementation of [LoadModule].
pub struct ModuleLoader<L, R>
where
    L: LoadFile,
    R: 'static + Sync + Send + Resolve,
{
    cm: Arc<SourceMap>,
    env: Env,
    resolver: R,
    loader: L,

    /// TODO(kdu1): Split the
    comments: StcComments,
    loading_started: DashSet<Arc<FileName>, FxBuildHasher>,
    dep_graph: RwLock<FastDiGraphMap<ModuleId, ()>>,
    cycles: RwLock<Vec<Vec<ModuleId>>>,

    ids: ModuleIdGenerator,
    parse_cache: DashMap<Arc<FileName>, (Arc<ModuleRecord>, StcComments), FxBuildHasher>,
    parsing_errors: Mutex<Vec<swc_ecma_parser::error::Error>>,
}

impl<L, R> ModuleLoader<L, R>
where
    L: LoadFile,
    R: Resolve,
{
    pub fn new(cm: Arc<SourceMap>, env: Env, resolver: R, loader: L) -> Self {
        Self {
            cm,
            env,
            resolver,
            loader,

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

        let (entry, comments) = self.parse(filename)?;

        let (_declared_modules, references, deps) = find_modules_and_deps(&comments, &entry.ast);

        let deps = if cfg!(feature = "no-threading") {
            GLOBALS.with(|globals| {
                (references.iter().map(|v| (v, false)))
                    .chain(deps.iter().map(|v| (v, true)))
                    .map(|(dep, is_normal_dep)| {
                        GLOBALS.set(globals, || {
                            let dep_path = Arc::new(self.resolver.resolve(filename, dep)?);

                            self.load_recursively(&dep_path, false).map(|v| (v, is_normal_dep))
                        })
                    })
                    .collect::<Vec<_>>()
            })
        } else {
            GLOBALS.with(|globals| {
                (references.par_iter().map(|v| (v, false)))
                    .chain(deps.par_iter().map(|v| (v, true)))
                    .map(|(dep, is_normal_dep)| {
                        GLOBALS.set(globals, || {
                            let dep_path = Arc::new(self.resolver.resolve(filename, dep)?);

                            self.load_recursively(&dep_path, false).map(|v| (v, is_normal_dep))
                        })
                    })
                    .collect::<Vec<_>>()
            })
        };

        {
            // Add to the dependency graph

            let mut g = self.dep_graph.write().unwrap();

            for (dep, is_normal_dep) in deps.iter().flatten() {
                g.add_edge(id, *dep, ());

                if entry.is_dts && !*is_normal_dep {
                    // Treat d.ts references as a cycle.
                    g.add_edge(*dep, id, ());
                }
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

        let record = self
            .parse_inner(filename)
            .with_context(|| format!("failed to parse `{}`", filename))?;
        self.parse_cache.insert(filename.clone(), record.clone());

        Ok(record)
    }

    /// This does not perform caching
    fn parse_inner(&self, filename: &Arc<FileName>) -> Result<(Arc<ModuleRecord>, StcComments)> {
        let comments = self.comments.clone();

        let (fm, syntax) = match &**filename {
            FileName::Custom(..) => {
                let fm = self.cm.new_source_file((**filename).clone(), String::new());

                let (id, top_level_mark) = self.ids.generate(filename);

                return Ok((
                    Arc::new(ModuleRecord {
                        id,
                        filename: filename.clone(),
                        is_dts: false,
                        top_level_ctxt: SyntaxContext::empty().apply_mark(top_level_mark),
                        ast: Module {
                            span: Span::new(fm.start_pos, fm.end_pos, Default::default()),
                            body: Default::default(),
                            shebang: Default::default(),
                        },
                    }),
                    self.comments.clone(),
                ));
            }

            _ => self
                .loader
                .load_file(&self.cm, filename)
                .with_context(|| format!("failed to load module `{}`", filename))?,
        };

        let mut parser = Parser::new(
            Syntax::Typescript(TsConfig {
                tsx: fm.name.to_string().contains("tsx"),
                ..Default::default()
            }),
            SourceFileInput::from(&*fm),
            Some(&comments),
        );
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
                is_dts: syntax.dts(),
                filename: filename.clone(),
                top_level_ctxt,
                ast,
            }),
            comments,
        ))
    }
}

impl<L, R> LoadModule for ModuleLoader<L, R>
where
    L: LoadFile,
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

pub struct DefaultFileLoader;

impl LoadFile for DefaultFileLoader {
    fn load_file(&self, cm: &Arc<SourceMap>, filename: &Arc<FileName>) -> Result<(Arc<SourceFile>, Syntax)> {
        match &**filename {
            FileName::Real(path) => {
                let fm = cm
                    .load_file(path)
                    .with_context(|| format!("failed to load module `{}`", path.display()))?;

                let syntax = TsConfig {
                    dts: path.as_os_str().to_string_lossy().ends_with(".d.ts"),
                    tsx: path.extension().map(|v| v == "tsx").unwrap_or(false),
                    ..Default::default()
                };

                Ok((fm, Syntax::Typescript(syntax)))
            }
            _ => {
                bail!("DefaultFileLoader only supports real files")
            }
        }
    }
}
