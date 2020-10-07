use crate::mode::Builtin;
use crate::{
    analyzer::{Analyzer, ScopeKind},
    errors::Error,
    validator::{Validate, ValidateWith},
    Marks, Rule,
};
use dashmap::DashMap;
use derivative::Derivative;
use fxhash::FxHashMap;
use once_cell::sync::{Lazy, OnceCell};
use slog::Logger;
pub use stc_builtin_types::Lib;
use stc_types::{Id, ModuleTypeData, Static, Type};
use std::{collections::hash_map::Entry, sync::Arc};
use swc_atoms::{js_word, JsWord};
use swc_common::Spanned;
use swc_common::{Globals, Span, DUMMY_SP};
use swc_ecma_ast::*;
use swc_ecma_parser::JscTarget;
use swc_ecma_visit::VisitMutWith;

#[derive(Debug, Default)]
pub struct BuiltIn {
    vars: FxHashMap<JsWord, Box<Type>>,
    types: FxHashMap<JsWord, Box<Type>>,
}

impl BuiltIn {
    pub fn from_ts_libs(env: &StableEnv, libs: &[Lib]) -> Self {
        debug_assert_ne!(libs, &[], "No typescript library file is specified");

        slog::info!(env.logger, "Loading typescript builtins: {:?}", libs);

        let modules = stc_builtin_types::load(libs);

        let iter = modules
            .iter()
            .map(|module| match &*module.body {
                TsNamespaceBody::TsModuleBlock(TsModuleBlock { body, .. }) => body,
                TsNamespaceBody::TsNamespaceDecl(_) => unreachable!(),
            })
            .flatten();
        Self::from_module_items(env, iter)
    }

    pub fn from_modules(env: &StableEnv, modules: Vec<Module>) -> Self {
        Self::from_module_items(env, modules.iter().flat_map(|module| &module.body))
    }

    pub fn from_module_items<'a, I>(env: &StableEnv, items: I) -> Self
    where
        I: IntoIterator<Item = &'a ModuleItem>,
    {
        slog::info!(env.logger, "Merging builtins");

        let mut result = Self::default();
        let mut storage = crate::mode::Builtin::default();
        let mut analyzer = Analyzer::for_builtin(env.clone(), &mut storage);

        for item in items {
            match *item {
                ModuleItem::ModuleDecl(ref md) => unreachable!("ModuleDecl: {:#?}", md),
                ModuleItem::Stmt(ref stmt) => match *stmt {
                    Stmt::Decl(Decl::Var(VarDecl { ref decls, .. })) => {
                        assert_eq!(decls.len(), 1);
                        let decl = decls.iter().next().unwrap();
                        let name = match decl.name {
                            Pat::Ident(ref i) => i,
                            _ => unreachable!(),
                        };
                        result.vars.insert(
                            name.sym.clone(),
                            name.type_ann
                                .clone()
                                .validate_with(&mut analyzer)
                                .map(|res| {
                                    res.expect("builtin: failed to parse type of a variable")
                                })
                                .expect("builtin: all variables should have a type"),
                        );
                    }

                    Stmt::Decl(Decl::Fn(FnDecl {
                        ref ident,
                        ref function,
                        ..
                    })) => {
                        result.vars.insert(
                            ident.sym.clone(),
                            box function
                                .clone()
                                .validate_with(&mut analyzer)
                                .expect("builtin: failed to parse function")
                                .into(),
                        );
                    }

                    Stmt::Decl(Decl::Class(ref c)) => {
                        debug_assert_eq!(result.types.get(&c.ident.sym.clone()), None);

                        // builtin libraries does not contain a class which extends
                        // other class.
                        debug_assert_eq!(c.class.super_class, None);
                        debug_assert_eq!(c.class.implements, vec![]);
                        let ty = analyzer
                            .with_child(
                                ScopeKind::Flow,
                                Default::default(),
                                |analyzer: &mut Analyzer| {
                                    Ok(Type::Class(stc_types::Class {
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
                                        type_params: c
                                            .class
                                            .type_params
                                            .clone()
                                            .validate_with(analyzer)
                                            .map(|opt| {
                                                opt.expect(
                                                    "builtin: failed to parse type parmas of a \
                                                     class",
                                                )
                                            }),
                                    }))
                                },
                            )
                            .unwrap();

                        result.types.insert(c.ident.sym.clone(), box ty);
                    }

                    Stmt::Decl(Decl::TsModule(ref m)) => {
                        let id = match m.id {
                            TsModuleName::Ident(ref i) => i.sym.clone(),
                            _ => unreachable!(),
                        };

                        let mut data = Builtin::default();
                        {
                            let mut analyzer = Analyzer::for_builtin(env.clone(), &mut data);

                            m.body.clone().visit_mut_with(&mut analyzer);
                        }

                        match result.types.entry(id) {
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
                                    box stc_types::Module {
                                        span: DUMMY_SP,
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

                    Stmt::Decl(Decl::TsTypeAlias(ref a)) => {
                        debug_assert_eq!(result.types.get(&a.id.sym.clone()), None);

                        let ty = a
                            .clone()
                            .validate_with(&mut analyzer)
                            .map(Type::from)
                            .expect("builtin: failed to process type alias");

                        result.types.insert(a.id.sym.clone(), box ty);
                    }

                    // Merge interface
                    Stmt::Decl(Decl::TsInterface(ref i)) => {
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
                },
            }
        }

        for (_, ty) in result.types.iter_mut() {
            ty.make_cheap();
        }

        for (_, ty) in result.vars.iter_mut() {
            ty.make_cheap();
        }

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
        static CACHE: Lazy<DashMap<Vec<Lib>, Arc<BuiltIn>>> = Lazy::new(Default::default);

        // TODO: Include `env` in cache

        if let Some(v) = CACHE.get(libs) {
            let builtin = (*v).clone();
            return Self {
                stable: STABLE_ENV.clone(),
                rule,
                target,
                builtin,
                global_types: Default::default(),
                global_vars: Default::default(),
            };
        }
        let builtin = BuiltIn::from_ts_libs(&STABLE_ENV, libs);
        let builtin = Arc::new(builtin);
        CACHE.insert(libs.to_vec(), builtin.clone());

        Self {
            stable: STABLE_ENV.clone(),
            rule,
            target,
            builtin,
            global_types: Default::default(),
            global_vars: Default::default(),
        }
    }

    pub(crate) const fn shared(&self) -> &StableEnv {
        &self.stable
    }

    pub(crate) const fn target(&self) -> JscTarget {
        self.target
    }

    pub(crate) const fn rule(&self) -> Rule {
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
                self.global_types.insert(
                    name,
                    Type::intersection(DUMMY_SP, vec![prev_ty, ty]).cheap(),
                );
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
        Self {
            logger,
            globals,
            marks,
        }
    }

    /// Note: The return marks should not be modified as it will not has any
    /// effect.
    pub const fn marks(&self) -> Marks {
        self.marks
    }

    pub(crate) fn logger_for_builtin(&self) -> Logger {
        self.logger.clone()
    }

    pub(crate) fn swc_globals(&self) -> &Globals {
        &self.globals
    }
}

impl Default for StableEnv {
    fn default() -> Self {
        Self::new(Logger::root(slog::Discard, slog::o!()), Default::default())
    }
}
