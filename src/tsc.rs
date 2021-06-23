use std::{
    fmt,
    fmt::{Display, Formatter},
    str::FromStr,
};
use structopt::StructOpt;

#[derive(Debug, StructOpt)]
#[structopt(rename_all = "camel-case")]
pub struct TscCommand {
    /// Allow JavaScript files to be compiled.
    #[structopt(long = "allowJs")]
    pub allow_js: bool,

    /// Allow default imports from modules with no default export. This does not
    /// affect code emit, just typechecking.
    #[structopt(long = "allowSyntheticDefaultImports")]
    pub allow_synthetic_default_imports: bool,

    /// Allow accessing UMD globals from modules.
    #[structopt(long = "allowUmdGlobalAccess")]
    pub allow_umd_global_access: bool,

    /// Do not report errors on unreachable code.
    #[structopt(long = "allowUnreachableCode")]
    pub allow_unreachable_code: bool,

    /// Do not report errors on unused labels.
    #[structopt(long = "allowUnusedLabels")]
    pub allow_unused_labels: bool,

    /// Parse in strict mode and emit "use strict" for each source file
    #[structopt(long = "alwaysStrict")]
    pub always_strict: bool,

    /// Has no effect as stc is fast enough. Just for compatibility with tsc.
    ///
    /// Have recompiles in --incremental and --watch assume that changes within
    /// a file will only affect files directly depending on it
    #[structopt(long = "assumeChangesOnlyAffectDirectDependencies")]
    #[deprecated = "stc is fast enough"]
    pub assume_changes_only_affect_direct_dependencies: bool,

    /// Base directory to resolve non-relative module names. See Module
    /// Resolution documentation for more details.
    #[structopt(long = "baseUrl", default_value)]
    pub base_url: String,

    /// Builds this project and all of its dependencies specified by Project
    /// References. Note that this flag is not compatible with others on this
    /// page. See more https://www.typescriptlang.org/docs/handbook/project-references.html
    #[structopt(long = "build", short = "b")]
    pub build: bool,

    /// The character set of the input files.
    #[structopt(long = "charset", default_value = "utf8")]
    pub charset: String,

    /// Report errors in .js files. Use in conjunction with --allowJs.
    #[structopt(long = "check_js")]
    pub check_js: bool,

    /// Ensure TypeScript can determine where to find the outputs of the
    /// referenced project to compile project.
    #[structopt(long = "composite")]
    pub composite: TrueBool,

    /// Generates corresponding .d.ts file.
    #[structopt(long = "declaration", short = "d")]
    pub declaration: bool,

    /// Output directory for generated declaration files.
    #[structopt(long = "declarationDir")]
    pub declaration_dir: String,

    #[structopt(long = "declarationMap")]
    pub declaration_map: bool,

    /// Show diagnostic information.
    #[structopt(long)]
    pub diagnostics: bool,

    /// Disable size limitation on JavaScript project.
    #[structopt(long)]
    pub disable_size_limit: bool,

    /// Provide full support for iterables in for..of, spread and destructuring
    /// when targeting ES5 or ES3.
    #[structopt(long)]
    pub downlevel_iteration: bool,

    /// Emit a UTF-8 Byte Order Mark (BOM) in the beginning of output files.
    #[structopt(long = "emitBOM")]
    pub emit_bom: bool,

    /// Only emit .d.ts declaration files.
    #[structopt(long = "emitDeclarationOnly")]
    pub emit_declaration_only: bool,

    /// Emit design-type metadata for decorated declarations in source. See
    /// issue #2577 for details.
    #[structopt(long = "emitDecoratorMetadata")]
    pub emit_decorator_metadata: bool,

    /// Emit __importStar and __importDefault helpers for runtime babel
    /// ecosystem compatibility and enable --allowSyntheticDefaultImports for
    /// typesystem compatibility.
    #[structopt(long = "esModuleInterop")]
    pub es_module_interop: bool,

    /// Enables experimental support for ES decorators.
    #[structopt(long = "experimentalDecorators")]
    pub experimental_decorators: bool,

    /// Show verbose diagnostic information
    #[structopt(long = "extendedDiagnostics")]
    pub extended_diagnostics: bool,

    /// Disallow inconsistently-cased references to the same file.
    #[structopt(long = "forceConsistentCasingInFileNames")]
    pub force_consistent_casing_in_file_names: bool,

    /// Generates a cpu profile at the given path. Passing an existing directory
    /// name instead of a file path will cause a timestamp-named profile to be
    /// generated in that directory instead.
    #[structopt(long = "generateCpuProfile", default_value = "profile.cpuprofile")]
    pub generate_cpu_profile: String,

    /// Import emit helpers (e.g. __extends, __rest, etc..) from tslib
    #[structopt(long = "importHelpers")]
    pub import_helpers: bool,
}

#[derive(Debug)]
pub struct TrueBool(bool);

impl Default for TrueBool {
    fn default() -> Self {
        Self(true)
    }
}

impl FromStr for TrueBool {
    type Err = <bool as FromStr>::Err;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        Ok(Self(s.parse()?))
    }
}

impl Display for TrueBool {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        Display::fmt(&self.0, f)
    }
}
