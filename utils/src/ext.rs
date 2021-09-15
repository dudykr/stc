use swc_common::{Span, TypeEq};
use tracing::instrument;

pub trait ValueExt: Sized {
    fn as_ok<E>(self) -> Result<Self, E> {
        Ok(self)
    }

    fn as_some(self) -> Option<Self> {
        Some(self)
    }
}

impl<T> ValueExt for T {}

pub trait SpanExt: Into<Span> + Copy {
    /// If `self` is dummy, use span from `other`.
    fn or_else<F>(self, other: F) -> Span
    where
        F: FnOnce() -> Span,
    {
        let s = self.into();
        if !s.is_dummy() {
            return s;
        }

        other()
    }
}

impl<T> SpanExt for T where T: Into<Span> + Copy {}

pub trait TypeVecExt {
    fn dedup_type(&mut self);
}

impl<T> TypeVecExt for Vec<T>
where
    T: TypeEq,
{
    #[instrument(skip(self))]
    fn dedup_type(&mut self) {
        let mut types: Vec<T> = Vec::with_capacity(self.capacity());
        for ty in self.drain(..) {
            if types.iter().any(|stored| stored.type_eq(&ty)) {
                continue;
            }
            types.push(ty);
        }
        *self = types;
    }
}
