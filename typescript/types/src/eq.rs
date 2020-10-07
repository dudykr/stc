use crate::{Class, FnParam, Type, TypeElement, TypeParamInstantiation};
use swc_common::{Span, DUMMY_SP};
use swc_ecma_ast::*;
use swc_ecma_utils::drop_span;

pub trait EqIgnoreSpan {
    fn eq_ignore_span(&self, to: &Self) -> bool;
}

pub trait TypeEq<T = Self> {
    fn type_eq(&self, to: &T) -> bool;
}

macro_rules! impl_by_clone {
    ($T:ty) => {
        impl EqIgnoreSpan for $T {
            fn eq_ignore_span(&self, to: &Self) -> bool {
                drop_span(self.clone()) == drop_span(to.clone())
            }
        }

        impl TypeEq<$T> for $T {
            fn type_eq(&self, to: &$T) -> bool {
                use swc_ecma_visit::FoldWith;

                let l = self.clone().fold_with(&mut TypeEqHelper);
                let r = to.clone().fold_with(&mut TypeEqHelper);

                l == r
            }
        }
    };
}

macro_rules! impl_ty_by_clone {
    ($T:ty) => {
        impl EqIgnoreSpan for $T {
            fn eq_ignore_span(&self, to: &Self) -> bool {
                use crate::visit::FoldWith;
                self.clone().fold_with(&mut SpanRemover) == to.clone().fold_with(&mut SpanRemover)
            }
        }

        impl TypeEq<$T> for $T {
            fn type_eq(&self, to: &$T) -> bool {
                use crate::visit::FoldWith;

                let l = self.clone().fold_with(&mut TypeEqHelper);
                let r = to.clone().fold_with(&mut TypeEqHelper);

                l == r
            }
        }
    };
}

impl_ty_by_clone!(Type);
impl_by_clone!(Expr);
impl_ty_by_clone!(TypeElement);
impl_by_clone!(TsLit);
impl_by_clone!(TsLitType);
impl_by_clone!(PropName);
impl_ty_by_clone!(Class);
impl_ty_by_clone!(FnParam);
impl_by_clone!(ComputedPropName);
impl_by_clone!(TsEntityName);
impl_ty_by_clone!(TypeParamInstantiation);
impl_by_clone!(TsTupleElement);

struct SpanRemover;
impl crate::visit::Fold for SpanRemover {
    fn fold_span(&mut self, _: Span) -> Span {
        DUMMY_SP
    }
}

struct TypeEqHelper;
impl crate::visit::Fold for TypeEqHelper {
    fn fold_span(&mut self, _: Span) -> Span {
        DUMMY_SP
    }

    fn fold_fn_param(&mut self, mut p: FnParam) -> FnParam {
        p.pat = Pat::Invalid(Invalid { span: DUMMY_SP });
        p
    }

    fn fold_expr(&mut self, node: Expr) -> Expr {
        use swc_ecma_visit::FoldWith;

        node.fold_with(self)
    }

    fn fold_ident(&mut self, node: Ident) -> Ident {
        use swc_ecma_visit::FoldWith;

        node.fold_with(self)
    }
}

impl swc_ecma_visit::Fold for TypeEqHelper {
    fn fold_span(&mut self, _: Span) -> Span {
        DUMMY_SP
    }
}

impl<T> EqIgnoreSpan for Box<T>
where
    T: EqIgnoreSpan,
{
    fn eq_ignore_span(&self, to: &Self) -> bool {
        (**self).eq_ignore_span(&**to)
    }
}

impl<T> EqIgnoreSpan for Option<T>
where
    T: EqIgnoreSpan,
{
    fn eq_ignore_span(&self, to: &Self) -> bool {
        match (self.as_ref(), to.as_ref()) {
            (Some(l), Some(r)) => l.eq_ignore_span(r),
            (None, None) => true,
            _ => false,
        }
    }
}

impl<T> EqIgnoreSpan for Vec<T>
where
    T: EqIgnoreSpan,
{
    fn eq_ignore_span(&self, to: &Self) -> bool {
        if self.len() != to.len() {
            return false;
        }

        self.iter()
            .zip(to.iter())
            .all(|(l, r)| l.eq_ignore_span(&r))
    }
}

impl<T> TypeEq for Box<T>
where
    T: TypeEq,
{
    fn type_eq(&self, to: &Self) -> bool {
        (**self).type_eq(&**to)
    }
}

impl<T> TypeEq for Option<T>
where
    T: TypeEq,
{
    fn type_eq(&self, to: &Self) -> bool {
        match (self.as_ref(), to.as_ref()) {
            (Some(l), Some(r)) => l.type_eq(r),
            (None, None) => true,
            _ => false,
        }
    }
}

impl<T> TypeEq for Vec<T>
where
    T: TypeEq,
{
    fn type_eq(&self, to: &Self) -> bool {
        if self.len() != to.len() {
            return false;
        }

        self.iter().zip(to.iter()).all(|(l, r)| l.type_eq(&r))
    }
}
