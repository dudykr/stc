use std::hash::Hash;
use swc_common::collections::{AHashMap, AHashSet};

pub trait Sortable: Send + Sync {
    type Id: Eq + Hash + Send + Sync;

    /// Returns `Set<(var_id, vars_required_for_var_id)>`.
    ///
    /// This returns the name of a property if it's [RProp] or [RClassMember].
    fn get_decls(&self) -> AHashMap<Self::Id, AHashSet<Self::Id>>;

    fn uses(&self) -> AHashSet<Self::Id>;
}

impl<T> Sortable for &'_ T
where
    T: Sortable,
{
    type Id = T::Id;

    fn get_decls(&self) -> AHashMap<Self::Id, AHashSet<Self::Id>> {
        (**self).get_decls()
    }

    fn uses(&self) -> AHashSet<Self::Id> {
        (**self).uses()
    }
}
