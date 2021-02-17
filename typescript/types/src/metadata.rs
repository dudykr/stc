use rnode::FoldWith;
use rnode::VisitMutWith;
use rnode::VisitWith;
use stc_visit::Visitable;
use swc_common::EqIgnoreSpan;
use swc_common::TypeEq;

macro_rules! metadata_traits {
    ($T:ty) => {
        impl EqIgnoreSpan for $T {
            #[inline]
            fn eq_ignore_span(&self, _: &Self) -> bool {
                true
            }
        }

        impl TypeEq for $T {
            #[inline]
            fn type_eq(&self, _: &Self) -> bool {
                true
            }
        }

        impl Visitable for $T {}

        impl<F: ?Sized> FoldWith<F> for $T {
            #[inline]
            fn fold_children_with(self, _: &mut F) -> Self {
                self
            }
        }

        impl<F: ?Sized> VisitWith<F> for $T {
            #[inline]
            fn visit_children_with(&self, _: &mut F) {}
        }

        impl<F: ?Sized> VisitMutWith<F> for $T {
            #[inline]
            fn visit_mut_children_with(&mut self, _: &mut F) {}
        }
    };
}

/// Stores metadata of the type.
///
/// # Note
///
/// This struct is treated as a span while comparison. It means, [EqIgnoreSpan]
/// and [TypeEq] will always return true.
#[derive(Debug, Clone, Default, PartialEq, Eq)]
pub struct TypeLitMetadata {}

metadata_traits!(TypeLitMetadata);
