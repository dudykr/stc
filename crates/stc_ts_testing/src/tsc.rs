//! Support for official typescript tests.

use std::{fs::read_to_string, path::Path, sync::Arc};

use anyhow::{bail, Context, Error};
use swc_common::{
    comments::Comments, errors::Handler, input::SourceFileInput, FileName, SourceMap,
};
use swc_ecma_ast::*;
use swc_ecma_parser::{lexer::Lexer, Parser, Syntax, TsConfig};

/// Error from `tsc`.
#[derive(Debug, Default, Clone, PartialEq, Eq)]
pub struct TscError {
    pub file: String,
    pub line: usize,
    pub col: usize,
    pub code: usize,
    pub msg: String,
}

impl TscError {
    pub fn parse_all(output: &str) -> Vec<Self> {
        let mut errors = vec![];

        'outer: for line in output.lines() {
            if line.trim().is_empty() || line.starts_with(|c: char| c.is_whitespace()) {
                continue;
            }
            let mut error = TscError::default();

            for (idx, item) in line.split(":").enumerate() {
                match idx {
                    0 => {
                        let item = item.strip_prefix("\u{001b}[96m");

                        let item = match item {
                            Some(v) => v,
                            None => continue 'outer,
                        };
                        let item = item
                            .strip_suffix("\u{001b}[0m")
                            .expect("expected colored output");

                        error.file = item.to_string();
                    }
                    1 => {
                        let item = item
                            .strip_prefix("\u{001b}[93m")
                            .expect("expected colored output")
                            .strip_suffix("\u{001b}[0m")
                            .expect("expected colored output");
                        error.line = item.parse().expect("failed to parse line");
                    }
                    2 => {
                        for (j, item) in item.split(' ').enumerate() {
                            if item.starts_with("TS") {
                                error.code =
                                    item[2..].parse().expect("failed to parse ts error code");
                                continue;
                            }

                            if j == 0 {
                                let item = item
                                    .strip_prefix("\u{001b}[93m")
                                    .expect("expected colored output")
                                    .strip_suffix("\u{001b}[0m")
                                    .expect("expected colored output");

                                error.col = item.parse().expect("failed to parse column");
                            }
                        }
                    }
                    3 => {
                        error.msg = line.to_string();
                    }
                    _ => {}
                }
            }

            errors.push(error);
        }

        errors
    }
}

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
        let s = read_to_string(&file_name).with_context(|| {
            format!(
                "failed to parse typescript test file at `{}`",
                file_name.display()
            )
        })?;
        let mut code = String::new();
        let mut type_data = vec![];

        for line in s.lines().skip(1) {
            if line.starts_with('>') {
                let idx = line.find(" : ");
                let idx = match idx {
                    Some(idx) => idx,
                    None => {
                        bail!(
                            "failed to find the separator of expression and type from `{}`",
                            line
                        )
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
            EsVersion::latest(),
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
