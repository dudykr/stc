use crate::Checker;
use rayon::prelude::*;
use std::{
    env,
    fs::read_dir,
    path::{Path, PathBuf},
};

impl Checker {
    fn try_loading_typing_of_one_package(&self, dir: &Path) {}

    fn load_typings_from_dir(&self, dir: &Path, types: Option<&[String]>) {
        let types_dir = dir.join("node_modules").join("@types");

        if !types_dir.is_dir() {
            return Default::default();
        }

        let dirs = types
            .map(|s| s.iter().map(|s| PathBuf::from(s.clone())).collect())
            .or_else(|| {
                let pkgs = read_dir(&types_dir).ok()?;

                let f = pkgs
                    .into_iter()
                    .filter_map(Result::ok)
                    .map(|e| e.path())
                    .collect::<Vec<_>>();

                Some(f)
            });

        if let Some(dirs) = dirs {
            dirs.into_par_iter().for_each(|dir| {
                self.try_loading_typing_of_one_package(&types_dir.join(dir));
            });
        }
    }

    /// Load typings from node_modules.
    ///
    /// - https://www.typescriptlang.org/tsconfig#typeRoots
    /// - https://www.typescriptlang.org/tsconfig#types
    pub fn load_typings(&self, type_roots: Option<&[PathBuf]>, types: Option<&[String]>) {
        let dir =
            env::current_dir().expect("failed to get current directory which is required to load typing packages");
        self.load_typings_from_dir(&dir, types)
    }
}
