pub use self::boxed::BoxedArcCow;
use servo_arc::Arc;

#[macro_use]
mod macros;
mod boxed;

pub enum ArcCow<T>
where
    T: 'static,
{
    Arc(Arc<T>),
    Raw(T),
}

impl_traits!(ArcCow, Raw);

impl<T> ArcCow<T> {
    #[inline]
    pub fn freezed(self) -> Self {
        match self {
            ArcCow::Raw(v) => Self::Arc(Arc::new(v)),
            _ => self,
        }
    }
}

impl<T> From<T> for ArcCow<T> {
    #[inline]
    fn from(data: T) -> Self {
        ArcCow::Raw(data)
    }
}

pub fn _assert_trait_impls() {
    fn _assert<P>()
    where
        P: Default + std::fmt::Debug + Clone + std::hash::Hash + PartialEq + Eq + Ord,
    {
    }

    _assert::<ArcCow<String>>();
    _assert::<BoxedArcCow<String>>();
}
