use anyhow::Error;
use std::path::Path;
use std::path::PathBuf;
use std::sync::Arc;
use swc_atoms::JsWord;

pub mod node;

pub trait Resolve: Send + Sync {
    fn resolve(&self, base: &Path, specifier: &JsWord) -> Result<Arc<PathBuf>, Error>;
}

impl<R> Resolve for &'_ R
where
    R: ?Sized + Resolve,
{
    fn resolve(&self, base: &Path, specifier: &JsWord) -> Result<Arc<PathBuf>, Error> {
        (**self).resolve(base, specifier)
    }
}

impl<R> Resolve for Arc<R>
where
    R: ?Sized + Resolve,
{
    fn resolve(&self, base: &Path, specifier: &JsWord) -> Result<Arc<PathBuf>, Error> {
        (**self).resolve(base, specifier)
    }
}

impl<R> Resolve for Box<R>
where
    R: ?Sized + Resolve,
{
    fn resolve(&self, base: &Path, specifier: &JsWord) -> Result<Arc<PathBuf>, Error> {
        (**self).resolve(base, specifier)
    }
}
