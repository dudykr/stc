use std::sync::atomic::{AtomicU64, Ordering::SeqCst};

use stc_visit::Visit;
use swc_common::{EqIgnoreSpan, TypeEq};

/// Used to distinguish private class properties.
///
/// # Context
///
/// `derivedClassTransitivity.ts`:
///
///
/// ```ts
/// // subclassing is not transitive when you can remove required parameters and add optional parameters
///
/// class C {
///     foo(x: number) { }
/// }
///
/// class D extends C {
///     foo() { } // ok to drop parameters
/// }
///
/// class E extends D {
///     foo(x?: string) { } // ok to add optional parameters
/// }
///
/// var c: C;
/// var d: D;
/// var e: E;
/// c = e;
/// var r = c.foo(1);
/// var r2 = e.foo('');
/// ```
///
/// ---
///
///
/// As derived class may have different signature than parent class, we can't
/// use super class to assign classes.
///
/// Because of it, we should assign properties directly.
///
/// It means we need a way to know if two class properties are exactly identical
/// i.e. declared in same class.
///
///
/// # Note
///
/// ## Alternative
///
/// [WIP] (Seeing if span is enough)
///
/// `lo` and `hi` of a [swc_common::Span] can be also used to check if two
/// private properties are identical, but I(kdy1) decided to use a new type
/// because
///
/// ## Not data
/// This is not `data` part of class and as a result `type_eq` and
/// `eq_ignore_span` always return `true`.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, Visit)]
pub struct ClassId(u64);

impl ClassId {
    pub fn generate() -> Self {
        static GENERATOR: AtomicU64 = AtomicU64::new(0);

        let id = GENERATOR.fetch_add(1, SeqCst);

        ClassId(id)
    }
}

/// Always true.
impl TypeEq for ClassId {
    fn type_eq(&self, _: &Self) -> bool {
        true
    }
}

/// Always true.
impl EqIgnoreSpan for ClassId {
    fn eq_ignore_span(&self, _: &Self) -> bool {
        true
    }
}
