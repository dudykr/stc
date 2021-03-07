//! Dependency analyzer for statements.

use stc_ts_ast_rnode::RModuleItem;
use stc_ts_ast_rnode::RStmt;

use crate::types::Sortable;

impl Sortable for RStmt {}

impl Sortable for RModuleItem {}
