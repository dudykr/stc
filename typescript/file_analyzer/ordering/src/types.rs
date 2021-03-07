use fxhash::FxHashSet;
use std::hash::Hash;

pub trait Sortable {
    type Id: Eq + Hash;

    /// This returns the name of a property if it's [RProp] or [RClassMember].
    fn declares(&self) -> FxHashSet<Self::Id>;

    fn uses(&self) -> FxHashSet<Self::Id>;
}

impl<T> Sortable for &'_ T
where
    T: Sortable,
{
    type Id = T::Id;

    fn declares(&self) -> FxHashSet<Self::Id> {
        (**self).declares()
    }

    fn uses(&self) -> FxHashSet<Self::Id> {
        (**self).uses()
    }
}
