use std::hash::Hash;

use fxhash::{FxHashMap, FxHashSet};

pub trait Sortable: Send + Sync {
    type Id: Eq + Hash + Send + Sync;

    /// Higher value means higher precedence.
    fn precedence(&self) -> u8;

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

    fn precedence(&self) -> u8 {
        (**self).precedence()
    }

    fn get_decls(&self) -> FxHashMap<Self::Id, FxHashSet<Self::Id>> {
        (**self).get_decls()
    }

    fn uses(&self) -> FxHashSet<Self::Id> {
        (**self).uses()
    }
}
