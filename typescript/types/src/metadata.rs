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

#[derive(Debug, Clone, Copy, Default, PartialEq, Eq)]
pub struct TypeLitMetadata {
    /// `true` if a spread element is used while initializing.
    pub inexact: bool,
    /// `true` if a type literal is modified by object union normalizer.
    pub normalized: bool,

    /// `true` if a type literal is declared by user or created from other
    /// user-declared types like [crate::Interface] or etc..
    ///
    /// # Note
    ///
    /// `specified` for `a` in the code below should be changed to `false`
    ///
    /// ```ts
    /// declare var a: { a: string }
    ///
    /// const b = [a, {b: 'foo'}]
    /// ```
    ///
    /// because `b` is normalized and so that the the code above is validated.
    ///
    /// But it should be `true` for the code below (copied from
    /// `objectLiteralNormalization.ts`).
    ///
    /// ```ts
    /// declare function f<T>(...items: T[]): T;
    /// declare let data: { a: 1, b: "abc", c: true };
    ///
    /// // Object literals are inferred as a single normalized union type
    /// let e1 = f({ a: 1, b: 2 }, { a: "abc" }, {});
    /// let e2 = f({}, { a: "abc" }, { a: 1, b: 2 });
    /// let e3 = f(data, { a: 2 }); // error
    /// let e4 = f({ a: 2 }, data); // error
    /// ```
    ///
    /// because tsc selects type of `data` instead of a normalized type ltieral
    /// union if one of inferred type literal is `specifiead`.
    pub specified: bool,
}

#[derive(Debug, Clone, Copy, Default, PartialEq, Eq)]
pub struct TypeElMetadata {
    /// If `true`, it means the element has a default value.
    ///
    /// While assignment, missing property error will not occurr by the element
    /// with this flag set to `true`.
    pub has_default: bool,
}

impl_traits!(TypeLitMetadata);
impl_traits!(TypeElMetadata);
