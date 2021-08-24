#![feature(box_syntax)]

use async_trait::async_trait;
use stc_ts_checker_api::{cache::Cached, dedup::Deduplicated, FileData, TypeChecker};
use stc_ts_dep_graph::{Chunk, Load};
use stc_ts_dts::{apply_mutations, cleanup_module_for_dts};
use stc_ts_file_analyzer::{analyzer::Analyzer, env::Env};
use stc_ts_storage::Single;
use stc_ts_types::module_id;
use stc_utils::path::intern::FileId;
use std::sync::Arc;
use tokio::task::spawn_blocking;

pub type FastTypeChecker<L> = Cached<Deduplicated<Arc<Checker<L>>>>;

#[derive(Debug)]
pub struct Checker<L>
where
    L: Load,
{
    env: Env,

    loader: L,
}

#[async_trait]
impl<L> TypeChecker for Checker<L>
where
    L: Load,
{
    async fn check(&self, name: FileId, _src: &str) -> FileData {
        let res = spawn_blocking(|| {
            let module = self.loader.load(name)?;

            match module {
                Chunk::Cycle(_) => todo!(),
                Chunk::Single(parsed) => {
                    let mut m = (*parsed.module).clone();

                    let mut storage = Single {
                        parent: None,
                        id: parsed.id,
                        path: parsed.file_id,
                        info: Default::default(),
                    };
                    let mut mutations;
                    {
                        let mut a = Analyzer::root(
                            self.logger.clone(),
                            self.env.clone(),
                            parsed.cm.clone(),
                            box &mut storage,
                            self,
                            self.debugger.clone(),
                        );

                        parsed.module.visit_with(&mut a);
                        mutations = a.mutations.unwrap();
                    }

                    {
                        // Get .d.ts file
                        apply_mutations(&mut mutations, &mut m);
                        cleanup_module_for_dts(&mut m.body, &storage.info.exports);
                    }
                }
            }

            Ok(())
        })
        .await;
    }
}

impl<L: 'static> stc_ts_file_analyzer::loader::Load for Checker<L> where L: Load {}
