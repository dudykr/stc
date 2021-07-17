use crate::{
    analyzer::Analyzer,
    env::{Env, ModuleConfig},
    loader::{Load, ModuleInfo},
    tests::{GLOBALS, MARKS},
    ValidationResult,
};
use once_cell::sync::Lazy;
use rnode::{NodeIdGenerator, RNode};
use stc_testing::logger;
use stc_ts_ast_rnode::RModule;
use stc_ts_builtin_types::Lib;
use stc_ts_storage::Single;
use stc_ts_types::{ModuleId, ModuleTypeData};
use std::{path::PathBuf, sync::Arc};
use swc_atoms::JsWord;
use swc_common::{FileName, SourceMap};
use swc_ecma_parser::{lexer::Lexer, JscTarget, Parser, StringInput, Syntax, TsConfig};
use swc_ecma_transforms::resolver::ts_resolver;
use swc_ecma_visit::FoldWith;
use testing::StdErr;

static ENV: Lazy<Env> = Lazy::new(|| {
    Env::simple(
        Default::default(),
        JscTarget::Es2020,
        ModuleConfig::None,
        &Lib::load("es2020.full"),
    )
});

pub struct Tester<'a, 'b> {
    cm: Arc<SourceMap>,
    pub analyzer: Analyzer<'a, 'b>,
    pub node_id_gen: NodeIdGenerator,
}

pub fn run_test<F, Ret>(op: F) -> Result<Ret, StdErr>
where
    F: FnOnce(&mut Tester) -> Ret,
{
    ::testing::run_test2(false, |cm, handler| {
        let mut storage = Single {
            parent: None,
            id: ModuleId::builtin(),
            path: Arc::new(PathBuf::new()),
            info: Default::default(),
        };

        let log = logger();
        let handler = Arc::new(handler);
        swc_common::GLOBALS.set(&crate::tests::GLOBALS, || {
            let analyzer = Analyzer::root(log.logger, ENV.clone(), cm.clone(), box &mut storage, &Loader {}, None);
            let mut tester = Tester {
                cm: cm.clone(),
                analyzer,
                node_id_gen: Default::default(),
            };
            let ret = op(&mut tester);

            Ok(ret)
        })
    })
}

impl Tester<'_, '_> {
    pub fn parse(&self, name: &str, src: &str) -> RModule {
        swc_common::GLOBALS.set(&GLOBALS, || {
            let fm = self.cm.new_source_file(FileName::Real(name.into()), src.into());

            let lexer = Lexer::new(
                Syntax::Typescript(TsConfig {
                    tsx: false,
                    decorators: true,
                    dynamic_import: true,
                    dts: false,
                    no_early_errors: false,
                    import_assertions: false,
                }),
                JscTarget::Es2020,
                StringInput::from(&*fm),
                None,
            );
            let mut parser = Parser::new_from(lexer);

            let module = parser
                .parse_module()
                .unwrap()
                .fold_with(&mut ts_resolver(MARKS.top_level_mark));

            RModule::from_orig(&mut NodeIdGenerator::invalid(), module)
        })
    }
}

struct Loader {}

impl Load for Loader {
    fn module_id(&self, base: &Arc<PathBuf>, src: &JsWord) -> Option<ModuleId> {
        unimplemented!()
    }

    fn is_in_same_circular_group(&self, base: ModuleId, dep: ModuleId) -> bool {
        unimplemented!()
    }

    fn load_circular_dep(
        &self,
        base: ModuleId,
        dep: ModuleId,
        partial: &ModuleTypeData,
    ) -> ValidationResult<ModuleInfo> {
        unimplemented!()
    }

    fn load_non_circular_dep(&self, base: ModuleId, dep: ModuleId) -> ValidationResult<ModuleInfo> {
        unimplemented!()
    }
}
