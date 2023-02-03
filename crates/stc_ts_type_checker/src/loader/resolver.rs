use swc_ecma_loader::resolve::Resolve;

pub struct TsResolver<R: Resolve> {
    inner: R,
}

impl<R> TsResolver<R>
where
    R: Resolve,
{
    pub fn new(inner: R) -> Self {
        Self { inner }
    }
}
