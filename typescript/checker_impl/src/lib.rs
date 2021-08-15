use async_trait::async_trait;
use stc_ts_checker_api::{cache::Cached, dedup::Deduplicated, FileData, TypeChecker};
use std::sync::Arc;
use swc_common::FileName;

pub type FastTypeChecker = Cached<Deduplicated<Arc<Checker>>>;

#[derive(Debug)]
pub struct Checker {}

#[async_trait]
impl TypeChecker for Checker {
    async fn check(&self, name: &FileName, src: &str) -> FileData {}
}
