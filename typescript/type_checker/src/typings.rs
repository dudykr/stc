use crate::Checker;
use std::{path::PathBuf, sync::Arc};

impl Checker {
    fn load_typing_dir(&self, dir: &Arc<PathBuf>) {}

    /// Load typings from node_modules.
    ///
    /// - https://www.typescriptlang.org/tsconfig#typeRoots
    /// - https://www.typescriptlang.org/tsconfig#types
    pub fn load_typings(&self, type_roots: &[PathBuf], types: &[String]) {}
}
