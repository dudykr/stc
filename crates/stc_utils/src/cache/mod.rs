use std::borrow::Cow;

use scoped_tls::scoped_thread_local;

scoped_thread_local!(pub static ALLOW_DEEP_CLONE: ());

pub trait Freeze: Sized + Clone {
    fn is_clone_cheap(&self) -> bool;

    fn make_clone_cheap(&mut self);

    #[inline]
    fn freezed(mut self) -> Self {
        self.make_clone_cheap();
        self
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

    fn make_clone_cheap(&mut self) {
        match self {
            Some(v) => v.make_clone_cheap(),
            None => {}
        }
    }
}

impl<T> Freeze for Vec<T>
where
    T: Freeze,
{
    fn is_clone_cheap(&self) -> bool {
        self.iter().all(|v| v.is_clone_cheap())
    }

    fn make_clone_cheap(&mut self) {
        self.iter_mut().for_each(|v| v.make_clone_cheap())
    }
}

impl<T> Freeze for Box<T>
where
    T: Freeze,
{
    fn make_clone_cheap(&mut self) {
        (**self).make_clone_cheap()
    }

    fn is_clone_cheap(&self) -> bool {
        (**self).is_clone_cheap()
    }
}

/// TODO(kdy1): This can be confusing.
impl<T> Freeze for Cow<'_, T>
where
    T: Clone + Freeze,
{
    fn make_clone_cheap(&mut self) {
        match self {
            Cow::Borrowed(v) => {
                if !v.is_clone_cheap() {
                    let mut v = ALLOW_DEEP_CLONE.set(&(), || v.clone());
                    v.make_clone_cheap();
                    *self = Cow::Owned(v);
                }
            }
            Cow::Owned(v) => {
                v.make_clone_cheap();
            }
        }
    }

    fn is_clone_cheap(&self) -> bool {
        (**self).is_clone_cheap()
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
