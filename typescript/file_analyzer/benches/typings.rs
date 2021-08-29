#![feature(box_syntax)]
#![feature(test)]

extern crate test;

use rnode::{NodeIdGenerator, RNode, VisitWith};
use stc_ts_ast_rnode::RModule;
use stc_ts_file_analyzer::{
    analyzer::{Analyzer, NoopLoader},
    env::{Env, ModuleConfig},
};
use stc_ts_storage::Single;
use stc_ts_types::ModuleId;
use std::{
    path::{Path, PathBuf},
    sync::Arc,
};
use swc_common::{input::SourceFileInput, GLOBALS};
use swc_ecma_ast::EsVersion;
use swc_ecma_parser::{lexer::Lexer, Parser, Syntax, TsConfig};
use swc_ecma_transforms::resolver::ts_resolver;
use swc_ecma_visit::FoldWith;
use test::Bencher;

fn bench_lib(b: &mut Bencher, path: &Path) {
    b.iter(|| {
        testing::run_test2(false, |cm, _handler| {
            let fm = cm.load_file(path).unwrap();

            let env = Env::simple(Default::default(), EsVersion::latest(), ModuleConfig::None, &[]);

            let mut node_id_gen = NodeIdGenerator::default();
            let mut module = {
                let lexer = Lexer::new(
                    Syntax::Typescript(TsConfig { ..Default::default() }),
                    EsVersion::Es2021,
                    SourceFileInput::from(&*fm),
                    None,
                );
                let mut parser = Parser::new_from(lexer);

                parser.parse_module().unwrap()
            };
            module = GLOBALS.set(env.shared().swc_globals(), || {
                module.fold_with(&mut ts_resolver(env.shared().marks().top_level_mark()))
            });
            let module = RModule::from_orig(&mut node_id_gen, module);

            let mut storage = Single {
                parent: None,
                id: ModuleId::builtin(),
                path: Arc::new(PathBuf::from(path)),
                info: Default::default(),
            };

            {
                let mut analyzer = Analyzer::root(env, cm.clone(), box &mut storage, &NoopLoader, None);
                module.visit_with(&mut analyzer);
            }

            Ok(())
        })
        .unwrap()
    });
}

#[bench]
fn bench_csstypes(b: &mut Bencher) {
    bench_lib(
        b,
        &PathBuf::new().join("node_modules").join("csstype").join("index.d.ts"),
    );
}
