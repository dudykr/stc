use crate::{
    analyzer::{Analyzer, ScopeKind},
    validator::ValidateWith,
    Marks, Rule,
};
use dashmap::DashMap;
use derivative::Derivative;
use fxhash::FxHashMap;
use once_cell::sync::Lazy;
use once_cell::sync::OnceCell;
use rnode::NodeIdGenerator;
use rnode::RNode;
use rnode::VisitMutWith;
use rnode::VisitWith;
use slog::Logger;
use stc_ts_ast_rnode::RDecl;
use stc_ts_ast_rnode::RIdent;
use stc_ts_ast_rnode::RModule;
use stc_ts_ast_rnode::RModuleItem;
use stc_ts_ast_rnode::RStmt;
use stc_ts_ast_rnode::RTsModuleName;
use stc_ts_ast_rnode::RVarDecl;
use stc_ts_builtin_types::Lib;
use stc_ts_errors::Error;
use stc_ts_storage::Builtin;
use stc_ts_types::{Id, ModuleTypeData, Type};
use std::time::Instant;
use std::{collections::hash_map::Entry, sync::Arc};
use swc_atoms::JsWord;
use swc_common::Spanned;
use swc_common::{Globals, Span, DUMMY_SP};
use swc_ecma_ast::*;
use swc_ecma_parser::JscTarget;

#[derive(Debug, Default)]
pub struct BuiltIn {
    vars: FxHashMap<JsWord, Box<Type>>,
    types: FxHashMap<JsWord, Box<Type>>,
}

impl BuiltIn {
    pub fn from_ts_libs(env: &StableEnv, libs: &[Lib]) -> Self {
        debug_assert_ne!(libs, &[], "No typescript library file is specified");

        let mut node_id_gen = NodeIdGenerator::default();

        slog::info!(env.logger, "Loading typescript builtins: {:?}", libs);

        let modules = stc_ts_builtin_types::load(libs);

        let iter = modules
            .iter()
            .map(|module| match &*module.body {
                TsNamespaceBody::TsModuleBlock(TsModuleBlock { body, .. }) => body,
                TsNamespaceBody::TsNamespaceDecl(_) => unreachable!(),
            })
            .flatten()
            .cloned()
            .map(|orig| RModuleItem::from_orig(&mut node_id_gen, orig));
        Self::from_module_items(env, iter)
    }

    pub fn from_modules(env: &StableEnv, modules: Vec<RModule>) -> Self {
        Self::from_module_items(env, modules.into_iter().flat_map(|module| module.body))
    }

    pub fn from_module_items<'a, I>(env: &StableEnv, items: I) -> Self
    where
        I: IntoIterator<Item = RModuleItem>,
    {
        slog::info!(env.logger, "Merging builtins");

        let start = Instant::now();

        let mut result = Self::default();
        let mut storage = Builtin::default();
        {
            let mut analyzer = Analyzer::for_builtin(env.clone(), &mut storage);

            for mut item in items {
                match item {
                    RModuleItem::ModuleDecl(ref md) => unreachable!("ModuleDecl: {:#?}", md),
                    RModuleItem::Stmt(ref mut stmt) => {
                        match *stmt {
                            RStmt::Decl(RDecl::Var(RVarDecl { ref decls, .. })) => {
                                assert_eq!(decls.len(), 1);
                                stmt.visit_with(&mut analyzer);
                            }

                            RStmt::Decl(RDecl::Fn(..)) => {
                                stmt.visit_with(&mut analyzer);
                            }

                            RStmt::Decl(RDecl::Class(ref c)) => {
                                debug_assert_eq!(result.types.get(&c.ident.sym.clone()), None);

                                // builtin libraries does not contain a class which extends
                                // other class.
                                debug_assert_eq!(c.class.super_class, None);
                                debug_assert_eq!(c.class.implements, vec![]);
                                let ty = analyzer
                                    .with_child(ScopeKind::Flow, Default::default(), |analyzer: &mut Analyzer| {
                                        Ok(Type::Class(stc_ts_types::Class {
                                            span: c.class.span,
                                            name: Some(c.ident.clone().into()),
                                            is_abstract: c.class.is_abstract,
                                            body: c
                                                .class
                                                .body
                                                .clone()
                                                .validate_with(analyzer)
                                                .unwrap()
                                                .into_iter()
                                                .filter_map(|v| v)
                                                .collect(),
                                            super_class: None,
                                            // implements: vec![],
                                            type_params: c.class.type_params.clone().validate_with(analyzer).map(
                                                |opt| opt.expect("builtin: failed to parse type parmas of a class"),
                                            ),
                                        }))
                                    })
                                    .unwrap();

                                result.types.insert(c.ident.sym.clone(), box ty);
                            }

                            RStmt::Decl(RDecl::TsModule(ref mut m)) => {
                                let id = match m.id {
                                    RTsModuleName::Ident(ref i) => i.sym.clone(),
                                    _ => unreachable!(),
                                };

                                let mut data = Builtin::default();
                                {
                                    let mut analyzer = Analyzer::for_builtin(env.clone(), &mut data);

                                    m.body.visit_mut_with(&mut analyzer);
                                }

                                match result.types.entry(id.clone()) {
                                    Entry::Occupied(mut e) => match &mut **e.get_mut() {
                                        Type::Module(module) => {
                                            //
                                            module.exports.types.extend(data.types);
                                            module.exports.vars.extend(data.vars);
                                        }

                                        ref e => unimplemented!("Merging module with {:?}", e),
                                    },
                                    Entry::Vacant(e) => {
                                        e.insert(
                                            box stc_ts_types::Module {
                                                span: DUMMY_SP,
                                                name: RTsModuleName::Ident(RIdent::new(id.clone(), DUMMY_SP)),
                                                exports: ModuleTypeData {
                                                    private_vars: Default::default(),
                                                    vars: data.vars,
                                                    private_types: Default::default(),
                                                    types: data.types,
                                                },
                                            }
                                            .into(),
                                        );
                                    }
                                }
                            }

                            RStmt::Decl(RDecl::TsTypeAlias(ref a)) => {
                                a.visit_with(&mut analyzer);

                                debug_assert_eq!(result.types.get(&a.id.sym.clone()), None);

                                let ty = a
                                    .clone()
                                    .validate_with(&mut analyzer)
                                    .map(Type::from)
                                    .expect("builtin: failed to process type alias");

                                result.types.insert(a.id.sym.clone(), box ty);
                            }

                            // Merge interface
                            RStmt::Decl(RDecl::TsInterface(ref i)) => {
                                if i.id.sym == *"Generator" {
                                    debug_assert!(
                                        i.type_params.is_some(),
                                        "builtin: Generator should have type parameter"
                                    )
                                }
                                i.visit_with(&mut analyzer);
                                let body = i
                                    .clone()
                                    .validate_with(&mut analyzer)
                                    .expect("builtin: failed to parse interface body");

                                match result.types.entry(i.id.sym.clone()) {
                                    Entry::Occupied(mut e) => match &mut **e.get_mut() {
                                        Type::Interface(ref mut v) => {
                                            v.body.extend(body.body);
                                        }
                                        _ => unreachable!("cannot merge interface with other type"),
                                    },
                                    Entry::Vacant(e) => {
                                        let ty = box i
                                            .clone()
                                            .validate_with(&mut analyzer)
                                            .expect("builtin: failed to parse interface")
                                            .into();

                                        e.insert(ty);
                                    }
                                }
                            }

                            _ => panic!("{:#?}", item),
                        }
                    }
                }
            }
        }

        for (id, ty) in storage.vars {
            //
            result.vars.insert(id, ty).unwrap_none();
        }

        for (_, ty) in result.types.iter_mut() {
            ty.make_cheap();
        }

        for (_, ty) in result.vars.iter_mut() {
            ty.make_cheap();
        }

        let dur = Instant::now() - start;
        eprintln!("[builtin] Took {:?}", dur);

        result
    }
}

/// Stuffs which can be changed between runs.
#[derive(Debug, Clone)]
pub struct Env {
    stable: StableEnv,
    rule: Rule,
    target: JscTarget,
    builtin: Arc<BuiltIn>,
    global_types: Arc<DashMap<JsWord, Box<Type>>>,
    global_vars: Arc<DashMap<JsWord, Box<Type>>>,
}

impl Env {
    pub fn new(env: StableEnv, rule: Rule, target: JscTarget, builtin: Arc<BuiltIn>) -> Self {
        Self {
            stable: env,
            builtin,
            target,
            global_types: Default::default(),
            global_vars: Default::default(),
            rule,
        }
    }

    pub fn simple(rule: Rule, target: JscTarget, libs: &[Lib]) -> Self {
        static STABLE_ENV: Lazy<StableEnv> = Lazy::new(Default::default);
        static CACHE: Lazy<DashMap<Vec<Lib>, OnceCell<Arc<BuiltIn>>>> = Lazy::new(Default::default);

        // TODO: Include `env` in cache
        let mut libs = libs.to_vec();
        libs.sort();
        libs.dedup();

        CACHE.entry(libs.clone()).or_default();
        let cell = CACHE.get(&libs).unwrap();

        let builtin = cell.get_or_init(|| {
            let builtin = BuiltIn::from_ts_libs(&STABLE_ENV, &libs);
            Arc::new(builtin)
        });
        let builtin = (*builtin).clone();

        Self {
            stable: STABLE_ENV.clone(),
            rule,
            target,
            builtin,
            global_types: Default::default(),
            global_vars: Default::default(),
        }
    }

    pub const fn shared(&self) -> &StableEnv {
        &self.stable
    }

    pub const fn target(&self) -> JscTarget {
        self.target
    }

    pub const fn rule(&self) -> Rule {
        self.rule
    }

    pub(crate) fn logger_for_builtin(&self) -> Logger {
        self.stable.logger_for_builtin().clone()
    }

    pub(crate) fn declare_global_var(&mut self, name: JsWord, ty: Box<Type>) {
        todo!("declare_global_var")
    }

    pub(crate) fn declare_global_type(&mut self, name: JsWord, ty: Box<Type>) {
        match self.get_global_type(ty.span(), &name) {
            Ok(prev_ty) => {
                self.global_types
                    .insert(name, Type::intersection(DUMMY_SP, vec![prev_ty, ty]).cheap());
            }
            Err(_) => {
                self.global_types.insert(name, ty);
            }
        }
    }

    pub(crate) fn get_global_var(&self, span: Span, name: &JsWord) -> Result<Box<Type>, Error> {
        if let Some(ty) = self.global_vars.get(name) {
            debug_assert!(ty.is_clone_cheap(), "{:?}", *ty);
            return Ok((*ty).clone());
        }

        if let Some(v) = self.builtin.vars.get(name) {
            debug_assert!(v.is_clone_cheap(), "{:?}", v);
            return Ok(v.clone());
        }

        Err(Error::NoSuchVar {
            span,
            name: Id::word(name.clone()),
        })
    }

    pub(crate) fn get_global_type(&self, span: Span, name: &JsWord) -> Result<Box<Type>, Error> {
        if let Some(ty) = self.global_types.get(name) {
            debug_assert!(ty.is_clone_cheap(), "{:?}", *ty);
            return Ok((*ty).clone());
        }

        if let Some(ty) = self.builtin.types.get(name) {
            debug_assert!(ty.is_clone_cheap(), "{:?}", ty);
            return Ok(ty.clone());
        }

        Err(Error::NoSuchType {
            span,
            name: Id::word(name.clone()),
        })
    }
}

/// Stuffs which are not changed regardless
#[derive(Clone, Derivative)]
#[derivative(Debug)]
pub struct StableEnv {
    /// Logger for builtins.
    logger: Logger,
    #[derivative(Debug = "ignore")]
    globals: Arc<Globals>,
    marks: Marks,
}

impl StableEnv {
    pub fn new(logger: Logger, globals: Arc<Globals>) -> Self {
        let marks = Marks::new(&globals);
        Self { logger, globals, marks }
    }

    /// Note: The return marks should not be modified as it will not has any
    /// effect.
    pub const fn marks(&self) -> Marks {
        self.marks
    }

    pub(crate) fn logger_for_builtin(&self) -> Logger {
        self.logger.clone()
    }

    pub fn swc_globals(&self) -> &Globals {
        &self.globals
    }
}

impl Default for StableEnv {
    fn default() -> Self {
        Self::new(Logger::root(slog::Discard, slog::o!()), Default::default())
    }
}
