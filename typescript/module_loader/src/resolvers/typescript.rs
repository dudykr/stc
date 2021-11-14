pub struct Wrapper<R>
where
    R: Resolver,
{
    pub resolver: R,
}
