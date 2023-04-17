#[salsa::input]
pub struct SourceText {
    #[return_ref]
    pub content: String,
}
