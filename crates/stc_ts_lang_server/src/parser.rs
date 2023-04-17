use swc_ecma_ast::Program;
use swc_ecma_parser::Syntax;

use crate::{ir::SourceText, Db};

#[salsa::input]
pub struct ParserInput {
    pub content: SourceText,
    pub syntax: Syntax,
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
pub(crate) fn parse_ast(db: &dyn Db, input: ParserInput) -> ParsedFile {}
