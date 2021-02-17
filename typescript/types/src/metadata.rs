use rnode::FoldWith;
use rnode::VisitMutWith;
use rnode::VisitWith;
use stc_visit::Visitable;
use swc_common::EqIgnoreSpan;
use swc_common::TypeEq;

/// Stores metadata of the type.
///
/// # Note
///
/// This struct is treated as a span while comparison. It means, [EqIgnoreSpan]
/// and [TypeEq] will always return true.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Metadata {}

impl EqIgnoreSpan for Metadata {
    #[inline]
    fn eq_ignore_span(&self, _: &Self) -> bool {
        true
    }
}

impl TypeEq for Metadata {
    #[inline]
    fn type_eq(&self, _: &Self) -> bool {
        true
    }
}

impl Visitable for Metadata {}

impl<F: ?Sized> FoldWith<F> for Metadata {
    #[inline]
    fn fold_children_with(self, _: &mut F) -> Self {
        self
    }
}

impl<F: ?Sized> VisitWith<F> for Metadata {
    #[inline]
    fn visit_children_with(&self, _: &mut F) {}
}

impl<F: ?Sized> VisitMutWith<F> for Metadata {
    #[inline]
    fn visit_mut_children_with(&mut self, _: &mut F) {}
}
