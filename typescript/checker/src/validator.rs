use swc_ecma_ast::{
    ClassMember, Param, Pat, TsExprWithTypeArgs, TsFnParam, TsTupleElement, TsType, TsTypeElement,
    TsTypeParam,
};

/// Visit with output
pub trait Validate<'context, T: ?Sized> {
    type Output;
    type Context: 'context + Copy;

    fn validate(&mut self, node: &mut T, ctxt: Self::Context) -> Self::Output;
}

impl<'c, T, V> Validate<'c, Box<T>> for V
where
    Self: Validate<'c, T>,
{
    type Output = <Self as Validate<'c, T>>::Output;
    type Context = <Self as Validate<'c, T>>::Context;

    fn validate(&mut self, node: &mut Box<T>, ctxt: Self::Context) -> Self::Output {
        self.validate(&mut **node, ctxt)
    }
}

impl<'c, T, V> Validate<'c, Option<T>> for V
where
    Self: Validate<'c, T>,
{
    type Output = Option<<Self as Validate<'c, T>>::Output>;
    type Context = <Self as Validate<'c, T>>::Context;

    fn validate(&mut self, node: &mut Option<T>, ctxt: Self::Context) -> Self::Output {
        match node {
            Some(ref mut n) => Some(self.validate(n, ctxt)),
            None => None,
        }
    }
}

pub trait ValidateInDeclOrder {}

impl<T> ValidateInDeclOrder for Box<T> where T: ValidateInDeclOrder {}

impl<T> ValidateInDeclOrder for Option<T> where T: ValidateInDeclOrder {}

impl ValidateInDeclOrder for Param {}

impl ValidateInDeclOrder for Pat {}

impl ValidateInDeclOrder for TsType {}

impl ValidateInDeclOrder for TsFnParam {}

impl ValidateInDeclOrder for TsTypeParam {}

impl ValidateInDeclOrder for TsExprWithTypeArgs {}

impl ValidateInDeclOrder for TsTupleElement {}

impl ValidateInDeclOrder for TsTypeElement {}

/// TODO: Remove this
impl ValidateInDeclOrder for ClassMember {}

impl<'c, T, V, O, E> Validate<'c, Vec<T>> for V
where
    Self: Validate<'c, T, Output = Result<O, E>>,
    T: ValidateInDeclOrder,
{
    type Output = Result<Vec<O>, E>;
    type Context = <Self as Validate<'c, T>>::Context;

    fn validate(&mut self, nodes: &mut Vec<T>, ctxt: Self::Context) -> Self::Output {
        nodes
            .iter_mut()
            .map(|node| self.validate(node, ctxt))
            .collect()
    }
}

pub trait ValidateWith<'c, V> {
    type Output;
    type Context: 'c + Copy;
    fn validate_with(&mut self, v: &mut V) -> Self::Output
    where
        Self::Context: Unit,
    {
        self.validate_with_args(v, Unit::make())
    }

    fn validate_with_default(&mut self, v: &mut V) -> Self::Output
    where
        Self::Context: Default,
    {
        self.validate_with_args(v, Default::default())
    }

    fn validate_with_args(&mut self, v: &mut V, ctxt: Self::Context) -> Self::Output;
}

pub trait Unit {
    fn make() -> Self;
}
impl Unit for () {
    fn make() -> Self {}
}

impl<'c, V, T> ValidateWith<'c, V> for T
where
    V: Validate<'c, T>,
{
    type Output = V::Output;
    type Context = V::Context;

    #[inline]
    fn validate_with_args(&mut self, v: &mut V, ctxt: Self::Context) -> Self::Output {
        v.validate(self, ctxt)
    }
}
