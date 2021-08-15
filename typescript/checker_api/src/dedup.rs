use crate::{FileData, TypeChecker};
use async_trait::async_trait;
use double_checked_cell_async::DoubleCheckedCell;
use std::sync::Arc;
use swc_common::{collections::AHashMap, FileName};
use tokio::sync::RwLock;

#[derive(Clone)]
pub struct Deduplicated<C>
where
    C: TypeChecker,
{
    cur_tasks: Arc<RwLock<AHashMap<FileName, DoubleCheckedCell<FileData>>>>,

    inner: C,
}

#[async_trait]
impl<C> TypeChecker for Deduplicated<C>
where
    C: TypeChecker,
{
    async fn check(&self, name: &FileName, src: &str) -> FileData {
        let result = {
            let global_read_lock = {
                let mut lock = self.cur_tasks.clone().write_owned().await;

                // Insert
                lock.entry(name.clone()).or_default();

                lock.downgrade()
            };

            let cell = global_read_lock.get(&name).unwrap();

            cell.get_or_init(async { self.inner.check(name, src).await })
                .await
                .clone()
        };
        let mut lock = self.cur_tasks.clone().write_owned().await;
        lock.remove(name);

        result
    }
}
