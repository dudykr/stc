use anyhow::Error;
use rayon::prelude::*;
use std::{
    fs::{copy, create_dir_all},
    panic::{catch_unwind, AssertUnwindSafe},
    path::{Path, PathBuf},
    process::Command,
    sync::Arc,
};
use structopt::StructOpt;
use swc_common::{
    errors::{ColorConfig, Handler},
    input::SourceFileInput,
    SourceMap, DUMMY_SP,
};
use swc_ecma_ast::*;
use swc_ecma_parser::{lexer::Lexer, Parser, Syntax, TsConfig};
use swc_ecma_visit::{Node, Visit, VisitWith};
use walkdir::WalkDir;

#[derive(Debug, StructOpt)]
pub struct CopyTests {
    /// Include tests only if there's no import or export from other file.
    #[structopt(long)]
    no_dep_only: bool,

    /// Include tests only if there's no import or export from other file.
    #[structopt(long)]
    dep_only: bool,

    /// Include tests only if there's no error.
    #[structopt(long)]
    no_error_only: bool,

    src: PathBuf,

    dst: PathBuf,
}

impl CopyTests {
    fn should_include_cheap(&self, path: &Path) -> Result<bool, Error> {
        let cm = Arc::new(SourceMap::default());
        let _handler = Handler::with_tty_emitter(ColorConfig::Always, true, false, Some(cm.clone()));

        let fm = cm.load_file(path)?;

        if !self.no_dep_only && !self.dep_only && !self.no_error_only {
            return Ok(true);
        }

        let lexer = Lexer::new(
            Syntax::Typescript(TsConfig {
                dynamic_import: true,
                ..Default::default()
            }),
            EsVersion::latest(),
            SourceFileInput::from(&*fm),
            None,
        );
        let mut parser = Parser::new_from(lexer);
        let program = catch_unwind(AssertUnwindSafe(|| {
            parser.parse_program().map_err(|_| Error::msg("failed to parse"))
        }))
        .map_err(|_| Error::msg("panic while parsing"))?;

        let program = match program {
            Ok(v) => v,
            Err(..) => {
                if self.no_error_only {
                    return Ok(false);
                }
                return Ok(true);
            }
        };

        let has_dep = {
            let mut v = DepFinder { found: false };
            program.visit_with(&Invalid { span: DUMMY_SP }, &mut v);
            v.found
        };
        if (self.no_dep_only && has_dep) || (self.dep_only && !has_dep) {
            return Ok(false);
        }

        Ok(true)
    }

    fn get_files_to_copy_cheap(&self) -> Result<Vec<PathBuf>, Error> {
        let mut files = vec![];
        for entry in WalkDir::new(&self.src) {
            let entry = entry?;

            if entry.file_type().is_dir() {
                continue;
            }

            let path_str = entry.path().to_string_lossy();
            if !path_str.ends_with(".ts") && !path_str.ends_with(".ts") {
                continue;
            }

            if !self.should_include_cheap(entry.path()).unwrap_or_else(|_| false) {
                continue;
            }

            files.push(entry.into_path());
        }

        Ok(files)
    }

    pub fn run(self) -> Result<(), Error> {
        let files = self.get_files_to_copy_cheap()?;
        eprintln!("{:?}", files);
        let _ = create_dir_all(&self.dst);

        files.into_par_iter().for_each(|file| {
            let rel_path = file.strip_prefix(&self.src);
            let rel_path = match rel_path {
                Ok(v) => v,
                Err(_) => return,
            };

            let to = self.dst.join(rel_path);
            if to.exists() {
                return;
            }

            if self.no_error_only && has_error(&file) {
                return;
            }

            let _ = create_dir_all(&to.parent().unwrap());

            let _ = copy(&file, &to);
        });

        Ok(())
    }
}

struct DepFinder {
    found: bool,
}

impl Visit for DepFinder {
    fn visit_import_decl(&mut self, _: &ImportDecl, _parent: &dyn Node) {
        self.found = true;
    }

    fn visit_export_all(&mut self, _: &ExportAll, _parent: &dyn Node) {
        self.found = true;
    }

    fn visit_named_export(&mut self, n: &NamedExport, _parent: &dyn Node) {
        self.found |= n.src.is_some();
    }
}

fn has_error(path: &Path) -> bool {
    Command::new("tsc")
        .arg(path)
        .arg("--strict")
        .arg("--target")
        .arg("es2020")
        .arg("--lib")
        .arg("es2020")
        .status()
        .map(|s| s.success())
        .unwrap_or(true)
}
