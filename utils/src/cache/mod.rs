use scoped_tls::scoped_thread_local;
use stc_arc_cow::{freeze::Freezer, ArcCow, BoxedArcCow};
use stc_visit::FoldWith;
use std::borrow::Cow;
use swc_common::util::map::Map;

scoped_thread_local!(pub static ALLOW_DEEP_CLONE: ());

pub trait Freeze: Sized + Clone {
    fn is_clone_cheap(&self) -> bool;

    fn freezed(self) -> Self;
}

impl<T> Freeze for ArcCow<T>
where
    T: Clone + FoldWith<Freezer>,
{
    fn is_clone_cheap(&self) -> bool {
        matches!(self, ArcCow::Arc(..))
    }

    fn freezed(self) -> Self {
        self.freezed()
    }
}

impl<T> Freeze for BoxedArcCow<T>
where
    T: Clone + FoldWith<Freezer>,
{
    fn is_clone_cheap(&self) -> bool {
        matches!(self, BoxedArcCow::Arc(..))
    }

    fn freezed(mut self) -> Self {
        self.freezed()
    }
}

impl<T> Freeze for Option<T>
where
    T: Freeze,
{
    fn is_clone_cheap(&self) -> bool {
        match self {
            Some(v) => v.is_clone_cheap(),
            None => true,
        }
    }

    fn freezed(self) -> Self {
        self.map(Freeze::freezed)
    }
}

impl<T> Freeze for Vec<T>
where
    T: Freeze,
{
    fn is_clone_cheap(&self) -> bool {
        self.iter().all(|v| v.is_clone_cheap())
    }

    fn freezed(self) -> Self {
        self.into_iter().map(Freeze::freezed).collect()
    }
}

impl<T> Freeze for Box<T>
where
    T: Freeze,
{
    fn is_clone_cheap(&self) -> bool {
        (**self).is_clone_cheap()
    }

    fn freezed(self) -> Self {
        self.map(|v| v.freezed())
    }
}

/// TODO(kdy1): This can be confusing.
impl<T> Freeze for Cow<'_, T>
where
    T: Clone + Freeze,
{
    fn is_clone_cheap(&self) -> bool {
        (**self).is_clone_cheap()
    }

    fn freezed(self) -> Self {
        match self {
            Cow::Borrowed(v) => {
                if v.is_clone_cheap() {
                    return Cow::Borrowed(v);
                }

                let v = ALLOW_DEEP_CLONE.set(&(), || v.clone());
                let v = v.freezed();
                Cow::Owned(v)
            }
            Cow::Owned(v) => Cow::Owned(v.freezed()),
        }
    }
}

#[macro_export]
macro_rules! try_cache {
    ($cache:expr, $key:expr, $default_op:expr) => {{
        let key = $key;

        let cached = if $cache.can_cache(&key) { $cache.get(&key) } else { None };

        if let Some(v) = cached {
            v
        } else {
            let v: Result<_, _> = (|| $default_op)();
            let v = match v {
                Ok(v) => v,
                Err(err) => return Err(err),
            };
            let v = $cache.insert(key, v);

            v
        }
    }};
}
