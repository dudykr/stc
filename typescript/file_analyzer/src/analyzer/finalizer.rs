use super::Analyzer;
use crate::ty;

impl Analyzer<'_, '_> {
    pub(super) fn finalize(&mut self, module: ty::Module) -> ty::Module {
        module
    }
}
