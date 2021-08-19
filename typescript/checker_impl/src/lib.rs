use async_trait::async_trait;
use stc_ts_checker_api::{cache::Cached, dedup::Deduplicated, FileData, TypeChecker};
use stc_utils::path::intern::FileId;
use std::sync::Arc;

pub type FastTypeChecker = Cached<Deduplicated<Arc<Checker>>>;

#[derive(Debug)]
pub struct Checker {}

#[async_trait]
impl TypeChecker for Checker {
    async fn check(&self, name: &FileId, src: &str) -> FileData {}
}
