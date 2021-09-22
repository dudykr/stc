use triomphe::{Arc, UniqueArc};

pub enum BoxedArcCow<T>
where
    T: 'static,
{
    Arc(Arc<T>),
    Boxed(UniqueArc<T>),
}

impl_traits!(BoxedArcCow, Boxed);

impl<T> BoxedArcCow<T> {
    pub fn freezed(self) -> Self {
        match self {
            BoxedArcCow::Boxed(v) => Self::Arc(v.shareable()),
            _ => self,
        }
    }
}

impl<T> From<T> for BoxedArcCow<T> {
    fn from(data: T) -> Self {
        BoxedArcCow::Boxed(UniqueArc::new(data))
    }
}

impl<T> BoxedArcCow<T>
where
    T: Clone,
{
    #[inline]
    pub fn into_inner(self) -> T {
        match self {
            Self::Arc(v) => match Arc::try_unwrap(v) {
                Ok(v) => v,
                Err(v) => (*v).clone(),
            },
            Self::Boxed(v) => UniqueArc::into_inner(v),
        }
    }
}
