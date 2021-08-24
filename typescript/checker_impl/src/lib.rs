#![feature(box_syntax)]

use async_trait::async_trait;
use stc_ts_checker_api::{cache::Cached, dedup::Deduplicated, FileData, TypeChecker};
use stc_ts_dep_graph::{Chunk, Load};
use stc_ts_dts::{apply_mutations, cleanup_module_for_dts};
use stc_ts_errors::debug::debugger::Debugger;
use stc_ts_file_analyzer::{analyzer::Analyzer, env::Env};
use stc_ts_storage::Single;
use stc_ts_types::module_id;
use stc_utils::path::intern::FileId;
use stc_visit::VisitWith;
use std::sync::Arc;
use tokio::task::spawn_blocking;

pub type FastTypeChecker<L> = Cached<Deduplicated<Arc<Checker<L>>>>;

pub struct Checker<L>
where
    L: Load,
{
    env: Env,

    debugger: Option<Debugger>,

    loader: L,
}

impl<L> Checker<L>
where
    L: Load,
{
    pub fn new(loader: L, env: Env, debugger: Option<Debugger>) -> Self {
        Checker { env, debugger, loader }
    }
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
                            slog::Logger::root(slog::Discard, slog::o!()),
                            self.env.clone(),
                            parsed.cm.clone(),
                            box &mut storage,
                            self,
                            self.debugger.clone(),
                        );

                        m.visit_with(&mut a);
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
