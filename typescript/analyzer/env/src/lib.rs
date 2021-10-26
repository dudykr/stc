use derivative::Derivative;
use std::sync::Arc;

/// Stuffs which can be changed between runs.
#[derive(Debug, Clone)]
pub struct Env {
    stable: StableEnv,
    rule: Rule,
    target: JscTarget,
    module: ModuleConfig,
    builtin: Arc<BuiltIn>,
    global_types: Arc<Mutex<FxHashMap<JsWord, Type>>>,
    global_vars: Arc<Mutex<FxHashMap<JsWord, Type>>>,
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
