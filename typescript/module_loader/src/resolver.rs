use anyhow::Error;
use auto_impl::auto_impl;
use std::{
    path::{Path, PathBuf},
    sync::Arc,
};
use swc_atoms::JsWord;

#[auto_impl(&,Box,Arc)]
pub trait TsResolve: Send + Sync {
    fn declare_module(&self, pat: &JsWord) -> Result<(), Error>;
    fn resolve(&self, base: &Path, specifier: &JsWord) -> Result<Arc<PathBuf>, Error>;
}
