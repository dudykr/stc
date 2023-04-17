use swc_ecma_ast::Program;

#[salsa::input]
pub struct SourceText {
    #[return_ref]
    pub content: String,
}

#[salsa::tracked]
pub struct ParsedFile {
    #[return_ref]
    pub program: Program,
}
