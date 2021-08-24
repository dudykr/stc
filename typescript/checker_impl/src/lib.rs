#![feature(box_syntax)]

use async_trait::async_trait;
use stc_ts_checker_api::{cache::Cached, dedup::Deduplicated, FileData, TypeChecker};
use stc_ts_dep_graph::{Chunk, Load};
use stc_ts_file_analyzer::{analyzer::Analyzer, env::Env};
use stc_ts_storage::Single;
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
                Chunk::Single(m) => {
                    let mut storage = Single {
                        parent: None,
                        id,
                        path: path.clone(),
                        info: Default::default(),
                    };
                    let mut mutations;
                    {
                        let mut a = Analyzer::root(
                            self.logger.new(slog::o!("file" => path.to_string_lossy().to_string())),
                            self.env.clone(),
                            m.cm.clone(),
                            box &mut storage,
                            self,
                            self.debugger.clone(),
                        );

                        m.module.visit_with(&mut a);
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

impl<L> stc_ts_file_analyzer::loader::Load for Checker<L> where L: Load {}
