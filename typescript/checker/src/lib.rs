#![allow(unused_variables)] // temporary
#![allow(unused_imports)] // temporary
#![allow(unused_mut)] // temporary
#![allow(dead_code)] // temporary
#![allow(incomplete_features)] // temporary
#![deny(unused_must_use)]
#![deny(unreachable_patterns)]
#![deny(mutable_borrow_reservation_conflict)]
#![deny(irrefutable_let_patterns)]
#![feature(box_patterns)]
#![feature(box_syntax)]
#![feature(try_blocks)]
#![feature(specialization)]
#![feature(vec_remove_item)]
#![feature(option_expect_none)]
#![recursion_limit = "1024"]

use self::dts::cleanup_module_for_dts;
use self::mode::ErrorStore;
use self::mode::File;
use self::mode::Group;
use self::mode::Single;
pub use self::{analyzer::Marks, env::Lib};
use self::{
    env::{Env, StableEnv},
    util::dashmap::DashMapExt,
};
use crate::{
    analyzer::{Analyzer, Info},
    errors::Error,
    ty::Type,
    validator::ValidateWith,
};
use dashmap::{DashMap, DashSet, SharedValue};
use errors::Errors;
use fxhash::FxHashMap;
use once_cell::sync::OnceCell;
use parking_lot::{Mutex, RwLock};
use rnode::RNode;
use rnode::VisitMutWith;
use slog::Logger;
use stc_ast_rnode::RModule;
use stc_checker_macros::validator;
use stc_module_graph::resolver::node::NodeResolver;
pub use stc_module_graph::{resolver::Resolve, ModuleGraph};
use stc_types::ModuleId;
pub use stc_types::{Id, ModuleTypeData};
use stc_utils::early_error;
use std::{mem::take, path::PathBuf, sync::Arc, time::Instant};
use swc_atoms::JsWord;
use swc_common::{
    comments::{Comment, Comments},
    errors::Handler,
    BytePos, Globals, SourceMap, Span, Spanned,
};
use swc_ecma_ast::Module;
use swc_ecma_parser::{lexer::Lexer, JscTarget, Parser, StringInput, Syntax, TsConfig};
use swc_ecma_transforms::resolver::ts_resolver;
use swc_ecma_visit::FoldWith;

#[macro_use]
mod debug;
pub mod analyzer;
mod dts;
pub mod env;
pub mod errors;
pub mod loader;
mod mode;
pub mod name;
#[cfg(test)]
mod tests;
pub mod ty;
mod type_facts;
pub mod util;
pub mod validator;

pub type ValidationResult<T = Box<Type>> = Result<T, Error>;

#[derive(Debug, PartialEq, Eq)]
pub struct DepInfo {
    pub span: Span,
    pub src: JsWord,
}

#[derive(Debug, PartialEq, Eq)]
pub struct Specifier {
    pub local: Id,
    pub export: Id,
}

#[derive(Debug)]
pub struct Config {
    /// Should we generate .d.ts?
    declaration: bool,
    /// Directory to store .d.ts files.
    declaration_dir: PathBuf,

    pub env: StableEnv,
}

#[derive(Debug, Clone, Copy, Default)]
pub struct Rule {
    pub no_implicit_any: bool,
    pub no_implicit_this: bool,
    pub always_strict: bool,
    pub strict_null_checks: bool,
    pub strict_function_types: bool,

    pub allow_unreachable_code: bool,
    pub allow_unused_labels: bool,
    pub no_fallthrough_cases_in_switch: bool,
    pub no_implicit_returns: bool,
    pub suppress_excess_property_errors: bool,
    pub suppress_implicit_any_index_errors: bool,
    pub no_strict_generic_checks: bool,
    pub no_unused_locals: bool,
    pub no_unused_parameters: bool,
}

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
                        let mut ids = set.iter().copied().collect::<Vec<_>>();
                        let mut modules = ids
                            .iter()
                            .map(|&id| self.module_graph.clone_module(id))
                            .map(|module| {
                                RModule::from_orig(module.fold_with(&mut ts_resolver(
                                    self.env.shared().marks().top_level_mark,
                                )))
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
            let mut module = self.module_graph.clone_module(id);
            module = module.fold_with(&mut ts_resolver(self.env.shared().marks().top_level_mark));
            let mut module = RModule::from_orig(module);

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

type CommentMap = Arc<DashMap<BytePos, Vec<Comment>>>;

/// Multi-threaded implementation of [Comments]
#[derive(Clone, Default)]
pub struct StcComments {
    leading: CommentMap,
    trailing: CommentMap,
}

impl Comments for StcComments {
    fn add_leading(&self, pos: BytePos, cmt: Comment) {
        self.leading.entry(pos).or_default().push(cmt);
    }

    fn add_leading_comments(&self, pos: BytePos, comments: Vec<Comment>) {
        self.leading.entry(pos).or_default().extend(comments);
    }

    fn has_leading(&self, pos: BytePos) -> bool {
        self.leading.contains_key(&pos)
    }

    fn move_leading(&self, from: BytePos, to: BytePos) {
        let cmt = self.leading.remove(&from);

        if let Some(cmt) = cmt {
            self.leading.entry(to).or_default().extend(cmt.1);
        }
    }

    fn take_leading(&self, pos: BytePos) -> Option<Vec<Comment>> {
        self.leading.remove(&pos).map(|v| v.1)
    }

    fn add_trailing(&self, pos: BytePos, cmt: Comment) {
        self.trailing.entry(pos).or_default().push(cmt)
    }

    fn add_trailing_comments(&self, pos: BytePos, comments: Vec<Comment>) {
        self.trailing.entry(pos).or_default().extend(comments)
    }

    fn has_trailing(&self, pos: BytePos) -> bool {
        self.trailing.contains_key(&pos)
    }

    fn move_trailing(&self, from: BytePos, to: BytePos) {
        let cmt = self.trailing.remove(&from);

        if let Some(cmt) = cmt {
            self.trailing.entry(to).or_default().extend(cmt.1);
        }
    }

    fn take_trailing(&self, pos: BytePos) -> Option<Vec<Comment>> {
        self.trailing.remove(&pos).map(|v| v.1)
    }
}
