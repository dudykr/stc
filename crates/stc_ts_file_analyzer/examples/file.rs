#![feature(box_syntax)]

use std::{
    env,
    path::{Path, PathBuf},
    sync::Arc,
};

use rnode::{NodeIdGenerator, RNode, VisitWith};
use stc_testing::init_logger;
use stc_ts_ast_rnode::RModule;
use stc_ts_builtin_types::Lib;
use stc_ts_env::{Env, ModuleConfig};
use stc_ts_file_analyzer::{
    analyzer::{Analyzer, NoopLoader},
    env::EnvFactory,
};
use stc_ts_storage::Single;
use stc_ts_types::ModuleId;
use swc_common::{input::SourceFileInput, FileName, GLOBALS};
use swc_ecma_ast::EsVersion;
use swc_ecma_parser::{lexer::Lexer, Parser, Syntax, TsConfig};
use swc_ecma_transforms::resolver::ts_resolver;
use swc_ecma_visit::FoldWith;
use tracing::Level;

fn profile_file(path: &Path) {
    testing::run_test2(false, |cm, _handler| {
        let fm = cm.load_file(path).unwrap();

        let env = Env::simple(Default::default(), EsVersion::latest(), ModuleConfig::None, &[Lib::Es5]);

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

        // Don't print logs from builtin modules.
        let _guard = init_logger(Level::DEBUG);

        let mut storage = Single {
            parent: None,
            id: ModuleId::builtin(),
            path: Arc::new(FileName::Real(PathBuf::from(path))),
            is_dts: false,
            info: Default::default(),
        };

        {
            let mut analyzer = Analyzer::root(env, cm.clone(), Default::default(), box &mut storage, &NoopLoader, None);
            module.visit_with(&mut analyzer);
        }

        Ok(())
    })
    .unwrap()
}

fn main() {
    let input = env::args().nth(1).expect("failed to analyze first file");
    profile_file(Path::new(&input));
}
