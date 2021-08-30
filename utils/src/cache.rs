pub trait Freeze: Sized + Clone {
    fn make_clone_cheap(&mut self);
}

impl<T> Freeze for Option<T>
where
    T: Freeze,
{
    fn make_clone_cheap(&mut self) {
        match self {
            Some(v) => v.make_clone_cheap(),
            None => {}
        }
    }
}
