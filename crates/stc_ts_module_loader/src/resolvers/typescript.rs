use std::sync::Arc;

use anyhow::{Context, Error};
use parking_lot::RwLock;
use swc_atoms::JsWord;
use swc_common::FileName;
use swc_ecma_loader::resolve::Resolve;

pub(crate) struct TsResolver<R>
where
    R: Resolve,
{
    declared_modules: RwLock<Vec<(JsWord, Arc<FileName>)>>,
    resolver: R,
}
impl<R> TsResolver<R>
where
    R: Resolve,
{
    pub(crate) fn new(resolver: R) -> TsResolver<R> {
        TsResolver {
            resolver,
            declared_modules: Default::default(),
        }
    }

    /// This returns [FileName::Custom] for `declare module "http"`-s.
    pub(crate) fn resolve(&self, base: &FileName, module_specifier: &str) -> Result<Arc<FileName>, Error> {
        for (pat, path) in self.declared_modules.read().iter() {
            if matches(&pat, &module_specifier) {
                return Ok(path.clone());
            }
        }

        let resolved = self
            .resolver
            .resolve(&base, module_specifier)
            .with_context(|| "failed to resolve dependency")?;

        Ok(Arc::new(resolved))
    }

    pub(crate) fn declare_module(&self, decl: JsWord) {
        log::debug!("Declaring module '{}'", decl);

        self.declared_modules
            .write()
            .push((decl.clone(), Arc::new(FileName::Custom(decl.to_string()))));
    }
}

fn matches(pat: &JsWord, module_specifier: &str) -> bool {
    if &**pat == module_specifier {
        return true;
    }

    false
}
