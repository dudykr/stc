#![allow(incomplete_features)]
#![feature(specialization)]

use num_bigint::BigInt;
pub use stc_visit_macros::Visit;
use swc_atoms::{Atom, JsWord};
use swc_common::SyntaxContext;

pub use self::{
    fold::{Fold, FoldWith},
    visit::{Visit, VisitWith},
    visit_mut::{VisitMut, VisitMutWith},
    visitable::Visitable,
};

mod fold;
mod visit;
mod visit_mut;
mod visitable;

macro_rules! noop {
    ($T:ty) => {
        impl Visitable for $T {}

        impl<V: ?Sized> FoldWith<V> for $T {
            ///  Noop
            #[inline]
            fn fold_children_with(self, _: &mut V) -> Self {
                self
            }
        }

        impl<V: ?Sized> VisitMutWith<V> for $T {
            ///  Noop
            #[inline]
            fn visit_mut_children_with(&mut self, _: &mut V) {}
        }

        impl<V: ?Sized> VisitWith<V> for $T {
            ///  Noop
            #[inline]
            fn visit_children_with(&self, _: &mut V) {}
        }
    };
}
macro_rules! primitives {
    (
        $(
            $T:ty
        ),*
    ) => {
        $(
            noop!($T);
        )*
    };
}

primitives!(bool, char);
primitives!(u8, u16, u32, u64, u128, usize);
primitives!(i8, i16, i32, i64, i128, isize);
primitives!(f32, f64);

noop!(String);
noop!(SyntaxContext);
noop!(JsWord);
noop!(BigInt);
noop!(&'_ str);
noop!(Atom);

noop!(swc_ecma_ast::TruePlusMinus);
noop!(swc_ecma_ast::TsTypeOperatorOp);
noop!(swc_ecma_ast::TsKeywordTypeKind);
noop!(swc_ecma_ast::BinaryOp);
noop!(swc_ecma_ast::UnaryOp);
noop!(swc_ecma_ast::Accessibility);
noop!(swc_ecma_ast::AssignOp);
noop!(swc_ecma_ast::VarDeclKind);
noop!(swc_ecma_ast::MethodKind);
noop!(swc_ecma_ast::UpdateOp);
noop!(swc_ecma_ast::MetaPropKind);
