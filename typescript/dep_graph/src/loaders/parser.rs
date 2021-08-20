use crate::{Chunk, Load, ParsedModule, ParsingError, Resolve};
use anyhow::{anyhow, bail, Context, Error};
use stc_ts_utils::StcComments;
use stc_utils::path::intern::FileId;
use std::sync::Arc;
use swc_common::{input::SourceFileInput, sync::Lrc, FileName, FilePathMapping, SourceMap};
use swc_ecma_ast::EsVersion;
use swc_ecma_parser::{lexer::Lexer, Parser, Syntax, TsConfig};

/// The layer connecting [Load] and [Resolve].
///
/// This struct manages the cache.
#[derive(Debug)]
pub struct ParsingLoader<R>
where
    R: Resolve,
{
    resolver: R,
    parser_config: TsConfig,
    parser_target: EsVersion,
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
        }
    }
}

impl<R> ParsingLoader<R>
where
    R: Resolve,
{
    fn parse_file(&self, file: FileId) -> Result<ParsedModule, Error> {
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

    fn load_file_recursively(&mut self, file: FileId) -> Result<ParsedModule, Error> {}
}

impl<R> Load for ParsingLoader<R>
where
    R: Resolve,
{
    fn load(&self, base: FileId, module_specifier: &str) -> Result<Chunk, Error> {
        (|| -> Result<_, Error> {
            let target = self
                .resolver
                .resolve(base, module_specifier)
                .context("failed to resolve")?;
        })()
        .with_context(|| format!("failed to load `{}` from `{}`", module_specifier, base))
    }
}
