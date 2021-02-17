//! # Rules
//!
//! All metadata structs should **derive** [Default].
//! It means, all field should be `false` by default.

use rnode::FoldWith;
use rnode::VisitMutWith;
use rnode::VisitWith;
use stc_visit::Visitable;
use swc_common::EqIgnoreSpan;
use swc_common::TypeEq;

macro_rules! impl_traits {
    ($T:ty) => {
        /// # Note
        ///
        /// This struct is treated as a span while comparison. It means, [EqIgnoreSpan]
        /// will always return true.
        impl EqIgnoreSpan for $T {
            #[inline]
            fn eq_ignore_span(&self, _: &Self) -> bool {
                true
            }
        }

        /// # Note
        ///
        /// This struct is treated as a span while comparison. It means, [TypeEq]
        /// will always return true.
        impl TypeEq for $T {
            #[inline]
            fn type_eq(&self, _: &Self) -> bool {
                true
            }
        }

        impl Visitable for $T {}

        /// Noop.
        impl<F: ?Sized> FoldWith<F> for $T {
            #[inline]
            fn fold_children_with(self, _: &mut F) -> Self {
                self
            }
        }

        /// Noop.
        impl<F: ?Sized> VisitWith<F> for $T {
            #[inline]
            fn visit_children_with(&self, _: &mut F) {}
        }

        /// Noop.
        impl<F: ?Sized> VisitMutWith<F> for $T {
            #[inline]
            fn visit_mut_children_with(&mut self, _: &mut F) {}
        }
    };
}

#[derive(Debug, Clone, Default, PartialEq, Eq)]
pub struct TypeLitMetadata {
    /// `true` if a spread element is used while initializing.
    pub inexact: bool,
    /// `true` if a type literal is modified by object union normalizer.
    pub normalized: bool,
}

impl_traits!(TypeLitMetadata);
