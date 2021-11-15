macro_rules! impl_traits {
    ($Ty:tt, $Raw:ident) => {
        use stc_visit::{FoldWith, VisitMutWith, VisitWith, Visitable};
        use std::ops::Deref;
        use swc_common::{EqIgnoreSpan, Spanned, TypeEq};

        impl<T> Spanned for $Ty<T>
        where
            T: Spanned,
        {
            #[inline]
            fn span(&self) -> swc_common::Span {
                (**self).span()
            }
        }

        impl<T> Deref for $Ty<T> {
            type Target = T;

            #[inline]
            fn deref(&self) -> &Self::Target {
                match self {
                    $Ty::Arc(v) => &v,
                    $Ty::$Raw(v) => &v,
                }
            }
        }

        impl<T> PartialEq<T> for $Ty<T>
        where
            T: PartialEq,
        {
            #[inline]
            fn eq(&self, other: &T) -> bool {
                (**self).eq(&*other)
            }

            #[inline]
            fn ne(&self, other: &T) -> bool {
                (**self).ne(&*other)
            }
        }

        impl<T> PartialEq for $Ty<T>
        where
            T: PartialEq,
        {
            #[inline]
            fn eq(&self, other: &Self) -> bool {
                match (self, other) {
                    ($Ty::Arc(l), $Ty::Arc(r)) => {
                        if Arc::ptr_eq(l, r) {
                            return true;
                        }
                    }
                    _ => {}
                }
                (**self).eq(&**other)
            }

            #[inline]
            fn ne(&self, other: &Self) -> bool {
                (**self).ne(&**other)
            }
        }

        impl<T> Eq for $Ty<T> where T: Eq {}

        impl<T> Default for $Ty<T>
        where
            T: Default,
        {
            #[inline]
            fn default() -> Self {
                $Ty::from(T::default())
            }
        }
        impl<T> Clone for $Ty<T>
        where
            T: Clone,
        {
            #[inline]
            fn clone(&self) -> Self {
                match self {
                    $Ty::Arc(v) => $Ty::Arc(v.clone()),
                    $Ty::$Raw(v) => Self::from((*v).clone()),
                }
            }
        }

        impl<T> PartialOrd for $Ty<T>
        where
            T: PartialOrd,
        {
            #[inline]
            fn partial_cmp(&self, r: &Self) -> Option<std::cmp::Ordering> {
                (**self).partial_cmp(&**r)
            }
        }

        impl<T> Ord for $Ty<T>
        where
            T: Ord,
        {
            #[inline]
            fn cmp(&self, r: &Self) -> std::cmp::Ordering {
                (**self).cmp(&**r)
            }
        }

        impl<T> std::hash::Hash for $Ty<T>
        where
            T: std::hash::Hash,
        {
            #[inline]
            fn hash<H>(&self, hasher: &mut H)
            where
                H: std::hash::Hasher,
            {
                (**self).hash(hasher)
            }
        }

        impl<T> std::fmt::Debug for $Ty<T>
        where
            T: std::fmt::Debug,
        {
            fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
                match self {
                    $Ty::Arc(..) => write!(f, "arc: ")?,
                    $Ty::$Raw(..) => write!(f, "raw: ")?,
                }
                std::fmt::Debug::fmt(&**self, f)
            }
        }

        impl<T> From<Arc<T>> for $Ty<T> {
            #[inline]
            fn from(arc: Arc<T>) -> Self {
                Self::Arc(arc)
            }
        }

        impl<T> TypeEq for $Ty<T>
        where
            T: TypeEq,
        {
            #[inline]
            fn type_eq(&self, other: &Self) -> bool {
                match (self, other) {
                    ($Ty::Arc(l), $Ty::Arc(r)) => {
                        if Arc::ptr_eq(l, r) {
                            return true;
                        }
                    }
                    _ => {}
                }

                (**self).type_eq(&**other)
            }
        }

        impl<T> EqIgnoreSpan for $Ty<T>
        where
            T: EqIgnoreSpan,
        {
            #[inline]
            fn eq_ignore_span(&self, other: &Self) -> bool {
                (**self).eq_ignore_span(&**other)
            }
        }

        impl<T> Visitable for $Ty<T> {}

        impl<T, V> VisitWith<V> for $Ty<T>
        where
            V: ?Sized,
            T: VisitWith<V>,
        {
            #[inline]
            fn visit_children_with(&self, v: &mut V) {
                (**self).visit_children_with(v)
            }
        }

        impl<T, V> VisitMutWith<V> for $Ty<T>
        where
            V: ?Sized,
            T: Clone + VisitMutWith<V>,
        {
            #[inline]
            fn visit_mut_children_with(&mut self, v: &mut V) {
                self.make_mut().visit_mut_children_with(v)
            }
        }

        impl<T, V> FoldWith<V> for $Ty<T>
        where
            V: ?Sized,
            T: Clone + FoldWith<V>,
        {
            #[inline]
            fn fold_children_with(self, v: &mut V) -> Self {
                Self::from(self.into_inner().fold_children_with(v))
            }
        }

        impl<T> $Ty<T>
        where
            T: Clone,
        {
            #[inline]
            pub fn make_mut(&mut self) -> &mut T {
                match self {
                    $Ty::Arc(v) => Arc::make_mut(v),
                    $Ty::$Raw(v) => &mut *v,
                }
            }
        }
    };
}
