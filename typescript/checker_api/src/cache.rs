use crate::{FileData, TypeChecker};
use async_trait::async_trait;
use stc_utils::path::intern::FileId;
use std::{collections::hash_map::Entry, sync::Arc};
use swc_common::collections::AHashMap;
use tokio::sync::Mutex;

#[derive(Clone)]
pub struct Cached<C>
where
    C: TypeChecker,
{
    capacity: usize,
    cache: Arc<Mutex<AHashMap<FileId, FileData>>>,
    inner: C,
}

impl<C> Cached<C>
where
    C: TypeChecker,
{
    pub fn new(inner: C, capacity: usize) -> Self {
        Cached {
            capacity,
            cache: Arc::new(Mutex::new(AHashMap::with_capacity_and_hasher(
                capacity,
                Default::default(),
            ))),
            inner,
        }
    }
}

#[async_trait]
impl<C> TypeChecker for Cached<C>
where
    C: TypeChecker,
{
    async fn check(&self, name: &FileId, src: &str) -> FileData {
        {
            let mut cache = self.cache.clone().lock_owned().await;

            let cached = cache.entry(name.clone());

            match cached {
                Entry::Occupied(e) => {
                    if &**e.get().fm.src == src {
                        return e.get().clone();
                    } else {
                        // We should update cache.
                        e.remove_entry();
                    }
                }
                Entry::Vacant(..) => {}
            }
        }

        let result = self.inner.check(name, src).await;

        {
            let mut cache = self.cache.clone().lock_owned().await;
            cache.insert(name.clone(), result.clone());
        }

        result
    }
}
