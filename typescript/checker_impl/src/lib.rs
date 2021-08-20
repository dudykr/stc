use async_trait::async_trait;
use stc_ts_checker_api::{cache::Cached, dedup::Deduplicated, FileData, TypeChecker};
use stc_ts_dep_graph::Load;
use stc_utils::path::intern::FileId;
use std::sync::Arc;

pub type FastTypeChecker<L> = Cached<Deduplicated<Arc<Checker<L>>>>;

#[derive(Debug)]
pub struct Checker<L>
where
    L: Load,
{
    loader: L,
}

#[async_trait]
impl<L> TypeChecker for Checker<L>
where
    L: Load,
{
    async fn check(&self, name: &FileId, src: &str) -> FileData {}
}
