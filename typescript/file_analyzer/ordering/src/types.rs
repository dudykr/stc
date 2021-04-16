use fxhash::FxHashMap;
use fxhash::FxHashSet;
use std::hash::Hash;

pub trait Sortable {
    type Id: Eq + Hash;

    /// Returns `Set<(var_id, vars_required_for_var_id)>`.
    ///
    /// This returns the name of a property if it's [RProp] or [RClassMember].
    fn get_decls(&self) -> FxHashMap<Self::Id, FxHashSet<Self::Id>>;

    fn uses(&self) -> FxHashSet<Self::Id>;
}

impl<T> Sortable for &'_ T
where
    T: Sortable,
{
    type Id = T::Id;

    fn get_decls(&self) -> FxHashMap<Self::Id, FxHashSet<Self::Id>> {
        (**self).get_decls()
    }

    fn uses(&self) -> FxHashSet<Self::Id> {
        (**self).uses()
    }
}
