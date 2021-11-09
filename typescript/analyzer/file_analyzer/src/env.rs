use crate::{
    analyzer::{Analyzer, ScopeKind},
    validator::ValidateWith,
};
use dashmap::DashMap;
use once_cell::sync::{Lazy, OnceCell};
use rnode::{NodeIdGenerator, RNode, VisitWith};
use rustc_hash::FxHashMap;
use stc_ts_ast_rnode::{RDecl, RIdent, RModule, RModuleItem, RStmt, RTsModuleName, RVarDecl};
use stc_ts_builtin_types::Lib;
use stc_ts_env::{BuiltIn, Env, ModuleConfig, Rule, StableEnv};
use stc_ts_storage::Builtin;
use stc_ts_type_ops::Fix;
use stc_ts_types::{ClassDef, ModuleTypeData, Type};
use stc_utils::stack;
use std::{collections::hash_map::Entry, sync::Arc, time::Instant};
use swc_atoms::JsWord;
use swc_common::DUMMY_SP;
use swc_ecma_ast::*;
use swc_ecma_parser::JscTarget;
use tracing::info;

pub trait BuiltInGen: Sized {
    fn new(vars: FxHashMap<JsWord, Type>, types: FxHashMap<JsWord, Type>) -> BuiltIn;

    fn from_ts_libs(env: &StableEnv, libs: &[Lib]) -> BuiltIn {
        debug_assert_ne!(libs, &[], "No typescript library file is specified");

        let _stack = stack::start(300);

        let mut node_id_gen = NodeIdGenerator::default();

        info!("Loading typescript builtins: {:?}", libs);

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

    fn from_modules(env: &StableEnv, modules: Vec<RModule>) -> BuiltIn {
        Self::from_module_items(env, modules.into_iter().flat_map(|module| module.body))
    }

    fn from_module_items<'a, I>(env: &StableEnv, items: I) -> BuiltIn
    where
        I: IntoIterator<Item = RModuleItem>,
    {
        info!("Merging builtins");

        let start = Instant::now();

        let mut types = FxHashMap::default();
        let mut vars = FxHashMap::default();
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
                                debug_assert_eq!(types.get(&c.ident.sym.clone()), None);

                                // builtin libraries does not contain a class which extends
                                // other class.
                                debug_assert_eq!(c.class.super_class, None);
                                debug_assert_eq!(c.class.implements, vec![]);
                                let ty = analyzer
                                    .with_child(ScopeKind::Flow, Default::default(), |analyzer: &mut Analyzer| {
                                        Ok(Type::ClassDef(ClassDef {
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
                                            type_params: c.class.type_params.validate_with(analyzer).map(|opt| {
                                                box opt.expect("builtin: failed to parse type parmas of a class")
                                            }),
                                            implements: c
                                                .class
                                                .implements
                                                .validate_with(analyzer)
                                                .map(Box::new)
                                                .unwrap(),
                                            metadata: Default::default(),
                                        }))
                                    })
                                    .unwrap();

                                types.insert(c.ident.sym.clone(), ty);
                            }

                            RStmt::Decl(RDecl::TsModule(ref mut m)) => {
                                let id = match m.id {
                                    RTsModuleName::Ident(ref i) => i.sym.clone(),
                                    _ => unreachable!(),
                                };

                                let mut data = Builtin::default();
                                {
                                    let mut analyzer = Analyzer::for_builtin(env.clone(), &mut data);

                                    m.body.visit_with(&mut analyzer);
                                }

                                assert!(!data.types.is_empty() || !data.vars.is_empty());

                                match types.entry(id.clone()) {
                                    Entry::Occupied(mut e) => match e.get_mut().normalize_mut() {
                                        Type::Module(module) => {
                                            //
                                            module.exports.types.extend(data.types);
                                            module.exports.vars.extend(data.vars);
                                        }

                                        ref e => unimplemented!("Merging module with {:?}", e),
                                    },
                                    Entry::Vacant(e) => {
                                        e.insert(
                                            Type::Module(stc_ts_types::Module {
                                                span: DUMMY_SP,
                                                name: RTsModuleName::Ident(RIdent::new(id.clone(), DUMMY_SP)),
                                                exports: box ModuleTypeData {
                                                    private_vars: Default::default(),
                                                    vars: data.vars,
                                                    private_types: Default::default(),
                                                    types: data.types,
                                                },
                                                metadata: Default::default(),
                                            })
                                            .cheap(),
                                        );
                                    }
                                }
                            }

                            RStmt::Decl(RDecl::TsTypeAlias(ref a)) => {
                                a.visit_with(&mut analyzer);

                                debug_assert_eq!(types.get(&a.id.sym.clone()), None);

                                let ty = a
                                    .clone()
                                    .validate_with(&mut analyzer)
                                    .map(Type::from)
                                    .expect("builtin: failed to process type alias");

                                types.insert(a.id.sym.clone(), ty);
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

                                match types.entry(i.id.sym.clone()) {
                                    Entry::Occupied(mut e) => match &mut *e.get_mut() {
                                        Type::Interface(ref mut v) => {
                                            v.body.extend(body.body);
                                        }
                                        _ => unreachable!("cannot merge interface with other type"),
                                    },
                                    Entry::Vacant(e) => {
                                        let ty = i
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
            let res = vars.insert(id, ty);
            assert_eq!(res, None, "duplicate");
        }

        for (_, ty) in types.iter_mut() {
            ty.fix();
            ty.make_cheap();
        }

        for (_, ty) in vars.iter_mut() {
            ty.fix();
            ty.make_cheap();
        }

        let dur = Instant::now() - start;
        eprintln!("[builtin] Took {:?}", dur);

        Self::new(vars, types)
    }
}

impl BuiltInGen for BuiltIn {
    fn new(vars: FxHashMap<JsWord, Type>, types: FxHashMap<JsWord, Type>) -> BuiltIn {
        BuiltIn::new(vars, types)
    }
}

pub trait EnvFactory {
    fn new(env: StableEnv, rule: Rule, target: EsVersion, module: ModuleConfig, builtin: Arc<BuiltIn>) -> Env;
    fn simple(rule: Rule, target: JscTarget, module: ModuleConfig, libs: &[Lib]) -> Env {
        static STABLE_ENV: Lazy<StableEnv> = Lazy::new(Default::default);
        static CACHE: Lazy<DashMap<Vec<Lib>, OnceCell<Arc<BuiltIn>>, ahash::RandomState>> = Lazy::new(Default::default);

        // TODO(kdy1): Include `env` in cache
        let mut libs = libs.to_vec();
        libs.sort();
        libs.dedup();

        CACHE.entry(libs.clone()).or_default();
        let cell = CACHE.get(&libs).unwrap();

        let builtin = swc_common::GLOBALS.set(STABLE_ENV.swc_globals(), || {
            let builtin = cell.get_or_init(|| {
                let builtin = BuiltIn::from_ts_libs(&STABLE_ENV, &libs);
                Arc::new(builtin)
            });
            (*builtin).clone()
        });

        Self::new(STABLE_ENV.clone(), rule, target, module, builtin)
    }
}

impl EnvFactory for Env {
    fn new(env: StableEnv, rule: Rule, target: EsVersion, module: ModuleConfig, builtin: Arc<BuiltIn>) -> Env {
        Env::new(env, rule, target, module, builtin)
    }
}
