use triomphe::Arc;

pub struct Freezed<T>(pub(crate) Arc<T>);

impl<T> Clone for Freezed<T> {
    fn clone(&self) -> Self {
        Self(self.0.clone())
    }
}
