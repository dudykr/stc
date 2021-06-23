//! Support for official typescript tests.

use anyhow::{bail, Context, Error};
use std::{fs::read_to_string, path::Path, sync::Arc};
use swc_common::{comments::Comments, errors::Handler, input::SourceFileInput, FileName, SourceMap};
use swc_ecma_ast::*;
use swc_ecma_parser::{lexer::Lexer, Parser, Syntax, TsConfig};

/// Simple typescript test case.
///
/// Expects typescript
#[derive(Debug)]
pub struct TsTestCase {
    pub program: Program,
    pub type_data: Vec<TypeInfo>,
}

#[derive(Debug)]
pub struct TypeInfo {
    pub expr: String,
    pub ty: String,
}

impl TsTestCase {
    pub fn parse(
        cm: &Arc<SourceMap>,
        handler: &Handler,
        file_name: &Path,
        comments: Option<&dyn Comments>,
    ) -> Result<Self, Error> {
        let s = read_to_string(&file_name)
            .with_context(|| format!("failed to parse typescript test file at `{}`", file_name.display()))?;
        let mut code = String::new();
        let mut type_data = vec![];

        for line in s.lines().skip(1) {
            if line.starts_with('>') {
                let idx = line.find(" : ");
                let idx = match idx {
                    Some(idx) => idx,
                    None => {
                        bail!("failed to find the separator of expression and type from `{}`", line)
                    }
                };

                let expr = line[1..idx].to_string();
                let ty = line[idx + 3..].to_string();

                type_data.push(TypeInfo { expr, ty });
            } else {
                code.push_str(line);
                code.push('\n');
            }
        }

        let fm = cm.new_source_file(FileName::Real(file_name.to_path_buf()), code);

        let lexer = Lexer::new(
            Syntax::Typescript(TsConfig {
                tsx: file_name.to_string_lossy().contains("tsx"),
                dynamic_import: true,
                ..Default::default()
            }),
            swc_ecma_parser::JscTarget::Es2020,
            SourceFileInput::from(&*fm),
            comments,
        );

        let mut parser = Parser::new_from(lexer);
        let program = parser.parse_program();
        let program = match program {
            Ok(program) => program,
            Err(err) => {
                err.into_diagnostic(handler).emit();
                bail!("failed to parse typescript test case")
            }
        };

        Ok(Self { program, type_data })
    }
}
