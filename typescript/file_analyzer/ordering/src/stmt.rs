//! Dependency analyzer for statements.

use crate::types::Sortable;
use fxhash::FxHashSet;
use stc_ts_ast_rnode::RModuleItem;
use stc_ts_ast_rnode::RStmt;
use stc_ts_types::Id;
use stc_ts_utils::AsModuleDecl;

impl Sortable for RStmt {
    type Id = Id;
}

impl Sortable for RModuleItem {
    type Id = Id;

    fn declares(&self) -> FxHashSet<Self::Id> {
        todo!()
    }

    fn uses(&self) -> FxHashSet<Self::Id> {
        todo!()
    }
}

fn declared_by<T>(node: &T) -> FxHashSet<Id>
where
    T: AsModuleDecl,
{
}
