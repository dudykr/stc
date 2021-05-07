use swc_common::Span;

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
