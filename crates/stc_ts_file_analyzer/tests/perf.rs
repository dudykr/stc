use std::{
    path::{Path, PathBuf},
    sync::Arc,
};

use rnode::{NodeIdGenerator, RNode, VisitWith};
use stc_testing::init_tracing;
use stc_ts_ast_rnode::RModule;
use stc_ts_builtin_types::Lib;
use stc_ts_env::{Env, ModuleConfig};
use stc_ts_file_analyzer::{
    analyzer::{Analyzer, NoopLoader},
    env::EnvFactory,
};
use stc_ts_storage::Single;
use stc_ts_types::ModuleId;
use swc_common::{input::SourceFileInput, FileName, Mark, SyntaxContext};
use swc_ecma_ast::EsVersion;
use swc_ecma_parser::{lexer::Lexer, Parser, Syntax, TsConfig};
use swc_ecma_transforms::resolver;
use swc_ecma_visit::FoldWith;

fn profile_file(name: &str, path: &Path) {
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
        let top_level_mark = Mark::new();
        module = module.fold_with(&mut resolver(env.shared().marks().unresolved_mark(), top_level_mark, true));
        let module = RModule::from_orig(&mut node_id_gen, module);

        // Don't print logs from builtin modules.
        let _guard = init_tracing(format!("file/{}", name));

        let mut storage = Single {
            parent: None,
            id: ModuleId::builtin(),
            top_level_ctxt: SyntaxContext::empty().apply_mark(top_level_mark),
            path: Arc::new(FileName::Real(path.to_path_buf())),
            is_dts: false,
            info: Default::default(),
        };

        {
            let mut analyzer = Analyzer::root(env, cm, Default::default(), Box::new(&mut storage), &NoopLoader, None);
            module.visit_with(&mut analyzer);
        }

        Ok(())
    })
    .unwrap()
}

#[test]
#[ignore = "Currently broken"]
fn profile_csstype() {
    profile_file("csstype", &PathBuf::new().join("node_modules").join("csstype").join("index.d.ts"));
}
