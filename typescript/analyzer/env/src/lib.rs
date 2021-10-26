pub use self::marks::{MarkExt, Marks};
use derivative::Derivative;
use parking_lot::Mutex;
use rustc_hash::FxHashMap;
use stc_ts_errors::Error;
use stc_ts_type_ops::Fix;
use stc_ts_types::{Id, Type};
use std::sync::Arc;
use string_enum::StringEnum;
use swc_atoms::JsWord;
use swc_common::{Globals, Span, Spanned, DUMMY_SP};
use swc_ecma_ast::EsVersion;
use tracing::instrument;

mod marks;

#[derive(Debug, Default)]
pub struct BuiltIn {
    vars: FxHashMap<JsWord, Type>,
    types: FxHashMap<JsWord, Type>,
}

impl BuiltIn {
    pub fn new(vars: FxHashMap<JsWord, Type>, types: FxHashMap<JsWord, Type>) -> Self {
        BuiltIn { vars, types }
    }
}

/// Stuffs which can be changed between runs.
#[derive(Debug, Clone)]
pub struct Env {
    stable: StableEnv,
    rule: Rule,
    target: EsVersion,
    module: ModuleConfig,
    builtin: Arc<BuiltIn>,
    global_types: Arc<Mutex<FxHashMap<JsWord, Type>>>,
    global_vars: Arc<Mutex<FxHashMap<JsWord, Type>>>,
}

impl Env {
    pub fn new(env: StableEnv, rule: Rule, target: EsVersion, module: ModuleConfig, builtin: Arc<BuiltIn>) -> Self {
        Self {
            stable: env,
            builtin,
            target,
            module,
            global_types: Default::default(),
            global_vars: Default::default(),
            rule,
        }
    }

    pub const fn shared(&self) -> &StableEnv {
        &self.stable
    }

    pub const fn target(&self) -> EsVersion {
        self.target
    }

    pub const fn module(&self) -> ModuleConfig {
        self.module
    }

    pub const fn rule(&self) -> Rule {
        self.rule
    }

    pub fn declare_global_var(&mut self, _name: JsWord, _ty: Type) {
        unimplemented!("declare_global_var")
    }

    pub fn declare_global_type(&mut self, name: JsWord, ty: Type) {
        match self.get_global_type(ty.span(), &name) {
            Ok(prev_ty) => {
                self.global_types
                    .lock()
                    .insert(name, Type::intersection(DUMMY_SP, vec![prev_ty, ty]).fixed().cheap());
            }
            Err(_) => {
                self.global_types.lock().insert(name, ty);
            }
        }
    }

    #[instrument(skip(self, span))]
    pub fn get_global_var(&self, span: Span, name: &JsWord) -> Result<Type, Error> {
        if let Some(ty) = self.global_vars.lock().get(name) {
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

    #[instrument(skip(self, span))]
    pub fn get_global_type(&self, span: Span, name: &JsWord) -> Result<Type, Error> {
        if let Some(ty) = self.global_types.lock().get(name) {
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
    #[derivative(Debug = "ignore")]
    globals: Arc<Globals>,
    marks: Marks,
}

impl StableEnv {
    pub fn new(globals: Arc<Globals>) -> Self {
        let marks = Marks::new(&globals);
        Self { globals, marks }
    }

    /// Note: The return marks should not be modified as it will not has any
    /// effect.
    pub const fn marks(&self) -> Marks {
        self.marks
    }

    pub fn swc_globals(&self) -> &Arc<Globals> {
        &self.globals
    }
}

impl Default for StableEnv {
    fn default() -> Self {
        Self::new(Default::default())
    }
}

#[derive(Clone, Copy, StringEnum)]
pub enum ModuleConfig {
    /// `commonjs`
    CommonJs,
    /// `es6`
    Es6,
    /// `es2015`
    Es2015,
    /// `es2020`
    Es2020,
    /// `none`
    None,
    /// `umd`
    Umd,
    /// `amd`
    Amd,
    /// `system`
    System,
    /// `esnext`
    EsNext,
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
    pub use_define_property_for_class_fields: bool,
}
