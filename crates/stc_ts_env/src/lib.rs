use std::{str::FromStr, sync::Arc};

use parking_lot::Mutex;
use rustc_hash::FxHashMap;
use serde::{Deserialize, Serialize};
use stc_ts_errors::{Error, ErrorKind};
use stc_ts_type_ops::Fix;
use stc_ts_types::{Id, Type};
use stc_utils::{cache::Freeze, dev_span};
use string_enum::StringEnum;
use swc_atoms::JsWord;
use swc_common::{Span, Spanned, DUMMY_SP};
use swc_ecma_ast::EsVersion;
use tsconfig::{CompilerOptions, Jsx, Module};

pub use self::marks::{MarkExt, Marks};

mod marks;

#[derive(Debug, Default, Serialize, Deserialize)]
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

    pub fn declare_global_var(&mut self, name: JsWord, ty: Type) {
        ty.assert_clone_cheap();

        let _res = self.global_vars.lock().insert(name, ty);
        // debug_assert_eq!(res, None, "failed to declare a global var {}",
        // name);
    }

    pub fn declare_global_type(&mut self, name: JsWord, ty: Type) {
        ty.assert_clone_cheap();

        match self.get_global_type(ty.span(), &name) {
            Ok(prev_ty) => {
                self.global_types
                    .lock()
                    .insert(name, Type::new_intersection(DUMMY_SP, vec![prev_ty, ty]).fixed().freezed());
            }
            Err(_) => {
                self.global_types.lock().insert(name, ty);
            }
        }
    }

    pub fn get_global_var(&self, span: Span, name: &JsWord) -> Result<Type, Error> {
        let _tracing = dev_span!("get_global_var");

        if let Some(ty) = self.global_vars.lock().get(name) {
            debug_assert!(ty.is_clone_cheap(), "{:?}", *ty);
            return Ok((*ty).clone());
        }

        if let Some(v) = self.builtin.vars.get(name) {
            debug_assert!(v.is_clone_cheap(), "{:?}", v);
            return Ok(v.clone());
        }

        Err(ErrorKind::NoSuchVar {
            span,
            name: Id::word(name.clone()),
        }
        .into())
    }

    pub fn get_global_type(&self, span: Span, name: &JsWord) -> Result<Type, Error> {
        let _tracing = dev_span!("get_global_type");

        if let Some(ty) = self.global_types.lock().get(name) {
            debug_assert!(ty.is_clone_cheap(), "{:?}", *ty);
            return Ok((*ty).clone());
        }

        if let Some(ty) = self.builtin.types.get(name) {
            debug_assert!(ty.is_clone_cheap(), "{:?}", ty);
            return Ok(ty.clone());
        }

        Err(ErrorKind::NoSuchType {
            span,
            name: Id::word(name.clone()),
        }
        .into())
    }
}

/// Stuffs which are not changed regardless
#[derive(Clone, Debug)]
pub struct StableEnv {
    marks: Marks,
}

impl StableEnv {
    pub fn new() -> Self {
        let marks = Marks::default();
        Self { marks }
    }

    /// Note: The return marks should not be modified as it will not has any
    /// effect.
    pub const fn marks(&self) -> Marks {
        self.marks
    }
}

impl Default for StableEnv {
    fn default() -> Self {
        Self::new()
    }
}

#[derive(Clone, Copy, StringEnum, Default)]
pub enum ModuleConfig {
    /// `commonjs`
    CommonJs,
    /// `es6`
    Es6,
    /// `es2015`
    Es2015,
    /// `es2020`
    Es2020,
    /// `es2022`
    Es2022,
    /// `none`
    #[default]
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
    pub no_unchecked_indexed_access: bool,
    pub suppress_excess_property_errors: bool,
    pub suppress_implicit_any_index_errors: bool,
    pub no_strict_generic_checks: bool,
    pub no_unused_locals: bool,
    pub no_unused_parameters: bool,
    pub use_define_property_for_class_fields: bool,
    pub no_lib: bool,

    pub jsx: JsxMode,
}

impl From<&CompilerOptions> for Rule {
    fn from(v: &CompilerOptions) -> Self {
        let strict = v.strict.unwrap_or(true);

        Self {
            no_implicit_any: v.no_implicit_any.unwrap_or(strict),
            no_implicit_this: v.no_implicit_this.unwrap_or(strict),
            always_strict: v.always_strict.unwrap_or(strict),
            strict_null_checks: v.strict_null_checks.unwrap_or(strict),
            strict_function_types: v.strict_function_types.unwrap_or(strict),

            allow_unreachable_code: v.allow_unreachable_code.unwrap_or_default(),
            allow_unused_labels: v.allow_unused_labels.unwrap_or_default(),
            no_fallthrough_cases_in_switch: v.no_fallthrough_cases_in_switch.unwrap_or_default(),
            no_implicit_returns: v.no_implicit_returns.unwrap_or_default(),
            no_unchecked_indexed_access: false,
            suppress_excess_property_errors: v.suppress_excess_property_errors.unwrap_or_default(),
            suppress_implicit_any_index_errors: v.suppress_implicit_any_index_errors.unwrap_or_default(),
            no_strict_generic_checks: v.no_strict_generic_checks.unwrap_or_default(),
            no_unused_locals: v.no_unused_locals.unwrap_or_default(),
            no_unused_parameters: v.no_unused_locals.unwrap_or_default(),
            use_define_property_for_class_fields: v.use_define_for_class_fields.unwrap_or_default(),
            no_lib: v.no_lib.unwrap_or_default(),

            jsx: v.jsx.map(From::from).unwrap_or_default(),
        }
    }
}

#[derive(Debug, Clone, Copy, Default)]
pub enum JsxMode {
    #[default]
    Preserve,
    React,
    ReactNative,
    ReactJsx,
    ReactJsxdev,
}

impl FromStr for JsxMode {
    type Err = ();

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        match s {
            "preserve" => Ok(JsxMode::Preserve),
            "react" => Ok(JsxMode::React),
            "react-native" => Ok(JsxMode::ReactNative),
            "react-jsx" => Ok(JsxMode::ReactJsx),
            "react-jsxdev" => Ok(JsxMode::ReactJsxdev),
            _ => Err(()),
        }
    }
}

impl From<Jsx> for JsxMode {
    fn from(value: Jsx) -> Self {
        match value {
            Jsx::React => JsxMode::React,
            Jsx::ReactJsx => JsxMode::ReactJsx,
            Jsx::ReactJsxdev => JsxMode::ReactJsxdev,
            Jsx::ReactNative => JsxMode::ReactNative,
            Jsx::Preserve => JsxMode::Preserve,
        }
    }
}

impl From<Module> for ModuleConfig {
    fn from(value: Module) -> Self {
        match value {
            Module::CommonJs => ModuleConfig::CommonJs,
            Module::Es6 => ModuleConfig::Es6,
            Module::Es2015 => ModuleConfig::Es2015,
            Module::Es2020 => ModuleConfig::Es2020,
            Module::None => ModuleConfig::None,
            Module::Umd => ModuleConfig::Umd,
            Module::Amd => ModuleConfig::Amd,
            Module::System => ModuleConfig::System,
            Module::EsNext => ModuleConfig::EsNext,
            Module::Other(other) => todo!("ModuleConfig::from(Module::Other({:?}))", other),
        }
    }
}
