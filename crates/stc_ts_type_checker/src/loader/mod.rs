use std::sync::{Arc, Mutex};

use anyhow::{bail, Context, Result};
use auto_impl::auto_impl;
use stc_ts_types::{module_id::ModuleIdGenerator, ModuleId};
use stc_ts_utils::StcComments;
use swc_common::{FileName, SourceMap, SyntaxContext};
use swc_ecma_ast::{EsVersion, Module};
use swc_ecma_loader::resolve::Resolve;
use swc_ecma_parser::{lexer::Lexer, Parser, StringInput, Syntax, TsConfig};

pub mod resolver;
pub mod store;

pub struct ModuleRecord {
    pub id: ModuleId,
    pub filename: Arc<FileName>,
    pub top_level_ctxt: SyntaxContext,
    pub ast: Module,
}

pub struct Records {
    pub modules: Vec<Arc<ModuleRecord>>,
    pub comments: StcComments,
}

/// A module loader.
#[auto_impl(&, Box, Arc)]
pub trait LoadModule: 'static + Send + Sync {
    /// This method should
    ///
    /// - Should never return empty vector.
    /// - The first item should be the file for `filename`.
    /// - Return **all modules in a cycle**.
    /// - Handle `declare module "foo"`.
    /// - Apply `resolver`.
    ///
    /// ## Tip
    ///
    /// Because of the cycles, this method would load all dependencies
    /// recursively.
    fn load_module(&self, filename: &Arc<FileName>) -> Result<Records>;

    /// Same constraints for [`LoadModule::load_module`] applies.
    fn load_dep(&self, base: &Arc<FileName>, module_specifier: &str) -> Result<Records>;
}

/// A simple implementation of [LoadModule].
pub struct ModuleLoader<R>
where
    R: 'static + Sync + Send + Resolve,
{
    cm: Arc<SourceMap>,
    resolver: R,

    ids: ModuleIdGenerator,
    parsing_errors: Mutex<Vec<swc_ecma_parser::error::Error>>,
}

impl<R> ModuleLoader<R>
where
    R: Resolve,
{
    pub fn new(cm: Arc<SourceMap>, resolver: R) -> Self {
        Self {
            cm,
            resolver,
            parsing_errors: Default::default(),
        }
    }
}

impl<R> LoadModule for ModuleLoader<R>
where
    R: 'static + Sync + Send + Resolve,
{
    fn load_module(&self, filename: &Arc<FileName>) -> Result<Records> {
        let comments = StcComments::default();

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
        let (id, top_level_mark) = self.ids.generate(&filename);

        let lexer = Lexer::new(syntax, EsVersion::latest(), StringInput::from(&*fm), Some(&comments));

        let mut parser = Parser::new_from(lexer);
        let result = parser.parse_module();

        let module = match result {
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
    }

    fn load_dep(&self, base: &Arc<FileName>, module_specifier: &str) -> Result<Records> {
        let filename = self
            .resolver
            .resolve(base, module_specifier)
            .with_context(|| format!("failed to resolve `{}` from `{}`", module_specifier, base))?;

        self.load_module(&Arc::new(filename))
    }
}
