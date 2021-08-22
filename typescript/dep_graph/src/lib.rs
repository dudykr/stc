use anyhow::Error;
use auto_impl::auto_impl;
use derivative::Derivative;
use stc_ts_utils::StcComments;
use stc_utils::path::intern::FileId;
use std::{
    fmt::{self, Display, Formatter},
    sync::Arc,
};
use swc_common::{sync::Lrc, SourceFile, SourceMap};
use swc_ecma_ast::Module;

mod deps;
pub mod loaders;
pub mod resolvers;

#[derive(Debug, Clone)]
pub enum Chunk {
    Cycle(Vec<ParsedModule>),
    Single(ParsedModule),
}

#[derive(Debug)]
pub struct MultiError {
    pub errors: Vec<Error>,
}

impl Display for MultiError {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        for e in &self.errors {
            write!(f, "{}", e)?;
        }

        Ok(())
    }
}

#[derive(Debug, Clone)]
pub struct ParsingError {
    pub errors: Vec<swc_ecma_parser::error::Error>,
}

impl Display for ParsingError {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        write!(f, "failed to parse")
    }
}

/// Cheap to clone. [SourceMap] and [SourceFile] are stored inline to make cache
/// efficient.
#[derive(Derivative)]
#[derivative(Debug)]
#[derive(Clone)]
pub struct ParsedModule {
    #[derivative(Debug = "ignore")]
    pub cm: Lrc<SourceMap>,

    #[derivative(Debug = "ignore")]
    pub fm: Lrc<SourceFile>,

    pub module: Arc<Module>,

    #[derivative(Debug = "ignore")]
    pub comments: Arc<StcComments>,
}

#[auto_impl(Arc, Box, &)]
pub trait Load: Send + Sync {
    fn load(&self, base: FileId, module_specifier: &str) -> Result<Chunk, Error>;
}

#[auto_impl(Arc, Box, &)]
pub trait Resolve: Send + Sync {
    fn resolve(&self, base: FileId, module_specifier: &str) -> Result<FileId, Error>;
}
