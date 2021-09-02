//! # Rules
//!
//! All metadata structs should **derive** [Default].
//! It means, all field should be `false` by default.

use rnode::{FoldWith, VisitMutWith, VisitWith};
use stc_visit::Visitable;
use swc_common::{EqIgnoreSpan, TypeEq};

pub trait TypeMetadata {
    fn common(&self) -> CommonTypeMetadata;
}

macro_rules! impl_basic_traits {
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

macro_rules! impl_traits {
    ($T:ty) => {
        impl_basic_traits!($T);

        impl TypeMetadata for $T {
            fn common(&self) -> CommonTypeMetadata {
                self.common
            }
        }
    };
}

/// Common metadata shared among [crate::Type]s.
#[derive(Debug, Clone, Copy, Default, PartialEq, Eq)]
pub struct CommonTypeMetadata {
    pub implicit: bool,

    /// This can be ignored based on the context.
    pub prevent_expansion: bool,

    pub contains_infer_type: bool,

    /// If this mark is applied, type will not be inferred (based on constraint)
    /// while simplifying.
    pub prevent_complex_simplification: bool,

    /// This mark is applied to types resolved from variables.
    ///
    /// Used to distinguish object literal with a reference to object literal.
    pub resolved_from_var: bool,
}

impl_basic_traits!(CommonTypeMetadata);

#[derive(Debug, Clone, Copy, Default, PartialEq, Eq)]
pub struct UnionMetadata {
    pub common: CommonTypeMetadata,
}

impl_traits!(UnionMetadata);

#[derive(Debug, Clone, Copy, Default, PartialEq, Eq)]
pub struct IntersectionMetadata {
    pub common: CommonTypeMetadata,
}

impl_traits!(IntersectionMetadata);

#[derive(Debug, Clone, Copy, Default, PartialEq, Eq)]
pub struct KeywordTypeMetadata {
    pub common: CommonTypeMetadata,
}

impl_traits!(KeywordTypeMetadata);

#[derive(Debug, Clone, Copy, Default, PartialEq, Eq)]
pub struct LitTypeMetadata {
    pub common: CommonTypeMetadata,

    /// If the mark is applied, it means that the literal should not be
    /// generalized.
    pub prevent_generalization: bool,
}

impl_traits!(LitTypeMetadata);

#[derive(Debug, Clone, Copy, Default, PartialEq, Eq)]
pub struct TupleMetadata {
    pub common: CommonTypeMetadata,

    pub prevent_tuple_to_array: bool,
}

impl_traits!(TupleMetadata);

#[derive(Debug, Clone, Copy, Default, PartialEq, Eq)]
pub struct SymbolMetadata {
    pub common: CommonTypeMetadata,
}

impl_traits!(SymbolMetadata);

#[derive(Debug, Clone, Copy, Default, PartialEq, Eq)]
pub struct TypeLitMetadata {
    pub common: CommonTypeMetadata,

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

impl_traits!(TypeLitMetadata);

#[derive(Debug, Clone, Copy, Default, PartialEq, Eq)]
pub struct TypeElMetadata {
    /// If `true`, it means the element has a default value.
    ///
    /// While assignment, missing property error will not occurr by the element
    /// with this flag set to `true`.
    pub has_default: bool,
}

impl_basic_traits!(TypeElMetadata);
