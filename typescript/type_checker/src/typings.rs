use crate::Checker;
use rayon::prelude::*;
use stc_ts_module_loader::resolvers::node::NodeResolver;
use std::{
    fs::read_dir,
    path::{Path, PathBuf},
    sync::Arc,
    time::Instant,
};
use swc_common::FileName;

impl Checker {
    fn try_loading_typing_of_one_package(&self, dir: &Path) {
        if !dir.is_dir() {
            return;
        }

        let result = NodeResolver
            .resolve_as_file(&dir)
            .or_else(|_| NodeResolver.resolve_as_directory(&dir));

        match result {
            Ok(entry) => {
                let entry = Arc::new(FileName::Real(entry));
                let start = Instant::now();
                self.module_graph.load_all(&entry).unwrap();

                self.analyze_module(None, entry.clone());

                let end = Instant::now();
                log::debug!("Loading typings at `{}` took {:?}", dir.display(), end - start);
            }
            Err(_) => {}
        }
    }

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
    pub fn load_typings(&self, base: &Path, _type_roots: Option<&[PathBuf]>, types: Option<&[String]>) {
        let mut dirs = vec![];

        let mut cur = Some(base);
        while let Some(c) = cur {
            dirs.push(c.to_path_buf());
            cur = c.parent();
        }

        dirs.into_par_iter().for_each(|dir| {
            self.load_typings_from_dir(&dir, types);
        });
    }
}
