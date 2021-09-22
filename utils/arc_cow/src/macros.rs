macro_rules! impl_traits {
    ($Ty:tt, $Raw:ident) => {
        use std::ops::Deref;
        use swc_common::Spanned;

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
    };
}
