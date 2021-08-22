use crate::{deps::find_deps, Chunk, Load, MultiError, ParsedModule, ParsingError, Resolve};
use anyhow::{anyhow, bail, Context, Error};
use dashmap::DashMap;
use fxhash::FxHashSet;
use petgraph::graphmap::DiGraphMap;
use rayon::prelude::*;
use stc_ts_utils::StcComments;
use stc_utils::path::intern::FileId;
use std::sync::{Arc, Mutex};
use swc_common::{input::SourceFileInput, sync::Lrc, FileName, FilePathMapping, SourceMap};
use swc_ecma_ast::EsVersion;
use swc_ecma_parser::{lexer::Lexer, Parser, Syntax, TsConfig};

#[derive(Debug, Default)]
struct DepData {
    cycles: Vec<FxHashSet<FileId>>,
    graph: DiGraphMap<FileId, ()>,
}

impl DepData {
    fn insert(&mut self, base: FileId, dep: FileId) {}
}

/// The layer connecting [Load] and [Resolve].
///
/// This struct manages the cache.
#[derive(Debug)]
pub struct ParsingLoader<R>
where
    R: Resolve,
{
    cache: DashMap<FileId, ParsedModule, fxhash::FxBuildHasher>,
    resolver: R,
    parser_config: TsConfig,
    parser_target: EsVersion,

    deps: Mutex<DepData>,
}

impl<R> ParsingLoader<R>
where
    R: Resolve,
{
    pub fn new(resolver: R, parser_config: TsConfig, parser_target: EsVersion) -> Self {
        ParsingLoader {
            resolver,
            parser_config,
            parser_target,
            cache: Default::default(),
            deps: Default::default(),
        }
    }
}

impl<R> ParsingLoader<R>
where
    R: Resolve,
{
    fn parse_file_without_caching(&self, file: FileId) -> Result<ParsedModule, Error> {
        (|| -> Result<_, Error> {
            let path = file.path();

            let path = match &*path {
                FileName::Real(p) => p,
                _ => {
                    bail!("{:?} is not suppport", path)
                }
            };

            let cm = Lrc::new(SourceMap::new(FilePathMapping::empty()));
            let fm = cm
                .load_file(&path)
                .with_context(|| format!("failed to load file at `{}`", path.display()))?;
            let comments = StcComments::default();

            let lexer = Lexer::new(
                Syntax::Typescript(self.parser_config),
                self.parser_target,
                SourceFileInput::from(&*fm),
                Some(&comments),
            );
            let mut parser = Parser::new_from(lexer);

            let mut errors = vec![];
            let module = parser.parse_module().map_err(|err| {
                errors.push(err);
                ()
            });

            errors.extend(parser.take_errors());

            if !errors.is_empty() {
                let err = ParsingError { errors };
                return Err(anyhow!(err));
            }

            Ok(ParsedModule {
                cm: cm.clone(),
                fm: fm.clone(),
                module: Lrc::new(module.unwrap()),
                comments: Arc::new(comments),
            })
        })()
        .with_context(|| format!("failed to parse file at `{}`", file))
    }

    fn parse_file(&self, file: FileId) -> Result<(ParsedModule, bool), Error> {
        let mut fresh = false;
        let res = self.cache.entry(file).or_try_insert_with(|| {
            let r = self.parse_file_without_caching(file);
            fresh = true;
            r
        });

        let res = res?;

        Ok(((*res).clone(), fresh))
    }

    fn load_file_recursively(&self, file: FileId) -> Result<(), Error> {
        let (module, fresh) = self.parse_file(file)?;

        if fresh {
            let mut deps = find_deps(&module.module);
            deps.dedup();

            let errors = deps
                .into_par_iter()
                .map(|dep| -> Result<_, Error> {
                    self.load_dep(file, &dep)?;

                    Ok(())
                })
                .filter_map(|r| r.err())
                .collect::<Vec<_>>();

            if !errors.is_empty() {
                bail!(MultiError { errors })
            }
        }

        Ok(())
    }

    fn load_dep(&self, base: FileId, module_specifier: &str) -> Result<FileId, Error> {
        (|| -> Result<_, Error> {
            let dep = self
                .resolver
                .resolve(base, &module_specifier)
                .with_context(|| format!("failed to resolve `{}` from `{}`", module_specifier, base))?;

            self.load_file_recursively(dep)?;

            {
                let mut lock = self
                    .deps
                    .lock()
                    .expect("failed to lock the mutex for a dependency graph");

                lock.insert(base, dep);
            }

            Ok(dep)
        })()
        .with_context(|| format!("failed to load `{}` from `{}`", module_specifier, base))
    }
}

impl<R> Load for ParsingLoader<R>
where
    R: Resolve,
{
    fn load(&self, base: FileId, module_specifier: &str) -> Result<Chunk, Error> {
        let dep_id = self.load_dep(base, module_specifier)?;

        let m = self
            .cache
            .get(&dep_id)
            .ok_or_else(|| anyhow!("Not loaded: {}", dep_id))?;

        let m = (*m).clone();

        // TODO: Detect cycle

        Ok(Chunk::Single(m))
    }
}
