use super::Analyzer;
use crate::{analyzer::util::ResultExt, ty, ty::Type};
use rnode::Fold;
use rnode::FoldWith;
use swc_common::Spanned;

impl Analyzer<'_, '_> {
    pub(super) fn finalize(&mut self, module: ty::Module) -> ty::Module {
        let mut v = ExpandAll { analyzer: self };
        module.fold_with(&mut v)
    }
}

struct ExpandAll<'a, 'b, 'm> {
    analyzer: &'m mut Analyzer<'a, 'b>,
}

impl Fold<Type> for ExpandAll<'_, '_, '_> {
    fn fold(&mut self, ty: Type) -> Type {
        let ty: Type = ty.fold_children_with(self);

        match ty {
            Type::Ref(..) => *self
                .analyzer
                .expand(ty.span(), box ty.clone())
                .report(&mut self.analyzer.storage)
                .unwrap_or_else(|| box ty),
            _ => *ty.cheap(),
        }
    }
}
