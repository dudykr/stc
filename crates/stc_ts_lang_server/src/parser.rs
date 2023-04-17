use swc_ecma_ast::Program;

#[salsa::tracked]
pub struct ParsedFile {
    /// This is `no_eq` because if the input content is not equal the AST cannot
    /// be equal.
    #[no_eq]
    #[return_ref]
    pub program: Program,
}
