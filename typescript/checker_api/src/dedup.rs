use crate::{FileData, TypeChecker};
use async_trait::async_trait;
use double_checked_cell_async::DoubleCheckedCell;
use std::sync::Arc;
use swc_common::{collections::AHashMap, FileName};
use tokio::sync::Mutex;

#[derive(Clone)]
pub struct Deduplicated<C>
where
    C: TypeChecker,
{
    cur_tasks: Arc<Mutex<AHashMap<FileName, DoubleCheckedCell<FileData>>>>,

    inner: C,
}

#[async_trait]
impl<C> TypeChecker for Deduplicated<C>
where
    C: TypeChecker,
{
    async fn check(&self, name: &FileName, src: &str) -> FileData {
        let mut lock = self.cur_tasks.clone().lock_owned().await;

        let entry = lock.entry(name.clone()).or_default();

        entry
            .get_or_init(async { self.inner.check(name, src).await })
            .await
            .clone()
    }
}
