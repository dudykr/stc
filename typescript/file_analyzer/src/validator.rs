use rnode::RNode;
use stc_ts_ast_rnode::RClassMember;
use stc_ts_ast_rnode::RParam;
use stc_ts_ast_rnode::RPat;
use stc_ts_ast_rnode::RTsExprWithTypeArgs;
use stc_ts_ast_rnode::RTsFnParam;
use stc_ts_ast_rnode::RTsTupleElement;
use stc_ts_ast_rnode::RTsType;
use stc_ts_ast_rnode::RTsTypeElement;
use stc_ts_ast_rnode::RTsTypeParam;

/// Visit with output
pub trait Validate<'context, T: ?Sized>
where
    T: RNode,
{
    type Output;
    type Context: 'context + Copy;

    fn validate(&mut self, node: &T, ctxt: Self::Context) -> Self::Output;
}

impl<'c, T, V> Validate<'c, Box<T>> for V
where
    Self: Validate<'c, T>,
    T: RNode,
{
    type Output = <Self as Validate<'c, T>>::Output;
    type Context = <Self as Validate<'c, T>>::Context;

    fn validate(&mut self, node: &Box<T>, ctxt: Self::Context) -> Self::Output {
        self.validate(&**node, ctxt)
    }
}

impl<'c, T, V> Validate<'c, Option<T>> for V
where
    Self: Validate<'c, T>,
    T: RNode,
{
    type Output = Option<<Self as Validate<'c, T>>::Output>;
    type Context = <Self as Validate<'c, T>>::Context;

    fn validate(&mut self, node: &Option<T>, ctxt: Self::Context) -> Self::Output {
        match node {
            Some(ref n) => Some(self.validate(n, ctxt)),
            None => None,
        }
    }
}

pub trait ValidateInDeclOrder: RNode {}

impl<T> ValidateInDeclOrder for Box<T> where T: ValidateInDeclOrder {}

impl<T> ValidateInDeclOrder for Option<T> where T: ValidateInDeclOrder {}

impl ValidateInDeclOrder for RParam {}

impl ValidateInDeclOrder for RPat {}

impl ValidateInDeclOrder for RTsType {}

impl ValidateInDeclOrder for RTsFnParam {}

impl ValidateInDeclOrder for RTsTypeParam {}

impl ValidateInDeclOrder for RTsExprWithTypeArgs {}

impl ValidateInDeclOrder for RTsTupleElement {}

impl ValidateInDeclOrder for RTsTypeElement {}

/// TODO: Remove this
impl ValidateInDeclOrder for RClassMember {}

impl<'c, T, V, O, E> Validate<'c, Vec<T>> for V
where
    Self: Validate<'c, T, Output = Result<O, E>>,
    T: ValidateInDeclOrder,
    Vec<T>: RNode,
{
    type Output = Result<Vec<O>, E>;
    type Context = <Self as Validate<'c, T>>::Context;

    fn validate(&mut self, nodes: &Vec<T>, ctxt: Self::Context) -> Self::Output {
        nodes.iter().map(|node| self.validate(node, ctxt)).collect()
    }
}

pub trait ValidateWith<'c, V> {
    type Output;
    type Context: 'c + Copy;
    fn validate_with(&self, v: &mut V) -> Self::Output
    where
        Self::Context: Unit,
    {
        self.validate_with_args(v, Unit::make())
    }

    fn validate_with_default(&self, v: &mut V) -> Self::Output
    where
        Self::Context: Default,
    {
        self.validate_with_args(v, Default::default())
    }

    fn validate_with_args(&self, v: &mut V, ctxt: Self::Context) -> Self::Output;
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
    T: RNode,
{
    type Output = V::Output;
    type Context = V::Context;

    #[inline]
    fn validate_with_args(&self, v: &mut V, ctxt: Self::Context) -> Self::Output {
        v.validate(self, ctxt)
    }
}
