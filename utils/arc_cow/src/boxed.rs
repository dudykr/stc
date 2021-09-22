use servo_arc::{Arc, UniqueArc};

pub enum BoxedArcCow<T>
where
    T: 'static,
{
    Arc(Arc<T>),
    Boxed(UniqueArc<T>),
}

impl_traits!(BoxedArcCow, Boxed);

impl<T> BoxedArcCow<T> {
    pub fn freezed(mut self) {
        match self {
            BoxedArcCow::Arc(_) => {}
            BoxedArcCow::Boxed(v) => {}
        }
    }
}

impl<T> From<T> for BoxedArcCow<T> {
    fn from(data: T) -> Self {
        BoxedArcCow::Boxed(UniqueArc::new(data))
    }
}
