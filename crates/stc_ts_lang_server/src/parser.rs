use swc_common::{util::take::Take, FileName};
use swc_ecma_ast::{EsVersion, Module, Program};
use swc_ecma_parser::Syntax;

use crate::{ir::SourceText, Db};

#[salsa::input]
pub struct ParserInput {
    pub filename: FileName,
    pub src: SourceText,
    pub syntax: Syntax,
    pub target: EsVersion,
}

#[salsa::tracked]
pub struct ParsedFile {
    /// This is `no_eq` because if the input content is not equal the AST cannot
    /// be equal.
    #[no_eq]
    #[return_ref]
    pub program: Program,
}

#[salsa::tracked]
pub(crate) fn parse_ast(db: &dyn Db, input: ParserInput) -> ParsedFile {
    let fm = db
        .shared()
        .cm
        .new_source_file(input.filename(db), input.src(db).content(db).to_string());

    let mut errors = vec![];

    let program = swc_ecma_parser::parse_file_as_program(&fm, input.syntax(db), input.target(db), Some(&db.shared().comments), &mut errors);

    let program = match program {
        Ok(v) => v,
        Err(err) => {
            // TODO: Handle errorx
            Program::Module(Module::dummy())
        }
    };

    ParsedFile::new(db, program)
}
