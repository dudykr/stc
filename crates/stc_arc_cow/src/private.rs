use triomphe::Arc;

pub struct PrivateArc<T>(pub(crate) Arc<T>);

impl<T> Clone for PrivateArc<T> {
    fn clone(&self) -> Self {
        Self(self.0.clone())
    }
}
