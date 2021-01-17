#![deny(unused)]

use fxhash::FxHashMap;
use once_cell::sync::Lazy;
use rayon::iter::{IntoParallelIterator, ParallelIterator};
use stc_ts_builtin_macro::builtin;
use std::cmp::Ordering;
use std::sync::{Arc, RwLock};
use swc_atoms::js_word;
use swc_common::{FileName, FilePathMapping, SourceMap};
use swc_ecma_ast::*;
use swc_ecma_parser::{
    lexer::{input::StringInput, Lexer},
    Parser, Syntax, TsConfig,
};
use swc_ecma_visit::{span_remover, FoldWith};

builtin!();

impl Lib {
    fn body(self) -> &'static TsNamespaceDecl {
        static CACHE: Lazy<RwLock<FxHashMap<Lib, &'static TsNamespaceDecl>>> = Lazy::new(Default::default);

        {
            let read = CACHE.read().expect("no panic is expected");
            if let Some(v) = read.get(&self) {
                return v;
            }
        }

        let mut write = CACHE.write().expect("no panic is expect4ed");

        {
            if let Some(v) = write.get(&self) {
                return v;
            }
        }

        let v = Box::leak(Box::new(parse(self.content())));
        assert_eq!(write.insert(self, v), None);

        v
    }

    fn prec(&self) -> usize {
        match self {
            Self::Es5 => 1,
            Self::Es2015Core => 2,
            Self::Es2015Collection => 3,
            Self::Es2015Generator => 4,
            Self::Es2015Promise => 5,
            Self::Es2015Symbol => 6,
            Self::Es2015Iterable => 7,
            Self::Es2015Proxy => 8,
            Self::Es2015Reflect => 9,
            Self::Es2015SymbolWellknown => 10,
            Self::Es2015 => 11,
            Self::Es2016ArrayInclude => 12,
            Self::Es2016 => 13,
            Self::Es2017Object => 14,
            Self::Es2017Sharedmemory => 15,
            Self::Es2017String => 16,
            Self::Es2017Intl => 17,
            Self::Es2017Typedarrays => 18,
            Self::Es2017 => 19,
            Self::Es2018Asynciterable => 20,
            Self::Es2018Promise => 21,
            Self::Es2018Regexp => 22,
            Self::Es2018Intl => 23,
            Self::Es2018 => 24,
            Self::Es2019Array => 25,
            Self::Es2019Object => 26,
            Self::Es2019String => 27,
            Self::Es2019Symbol => 28,
            Self::Es2019 => 29,
            Self::Es2020 => 30,
            Self::Es2020String => 31,
            Self::Es2020SymbolWellknown => 32,
            Self::EsnextBigint => 33,
            Self::EsnextIntl => 34,
            Self::Esnext => 35,

            Self::Dom => 50,
            Self::WebworkerImportscripts => 51,
            Self::Scripthost => 52,
            Self::DomIterable => 53,
            Self::DomIterableGenerated => 54,
            Self::Header => 55,
            Self::Importes5 => 56,
            Self::WebworkerGenerated => 57,

            Self::Es5Full => 100,
            Self::Es2015Full => 101,
            Self::Es2016Full => 102,
            Self::Es2017Full => 103,
            Self::Es2018Full => 104,
            Self::Es2019Full => 105,
            Self::Es2020Full => 106,
            Self::EsnextFull => 107,
        }
    }
}

impl PartialOrd for Lib {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        Some(self.cmp(other))
    }
}

impl Ord for Lib {
    fn cmp(&self, other: &Self) -> Ordering {
        self.prec().cmp(&other.prec())
    }
}

/// Merge definitions
pub fn load(libs: &[Lib]) -> Vec<&'static TsNamespaceDecl> {
    libs.into_par_iter().map(|lib| lib.body()).collect()
}

fn parse(content: &str) -> TsNamespaceDecl {
    let cm = Arc::new(SourceMap::new(FilePathMapping::empty()));

    let fm = cm.new_source_file(FileName::Anon, content.to_string());
    let lexer = Lexer::new(
        Syntax::Typescript(TsConfig {
            dts: true,
            ..Default::default()
        }),
        Default::default(),
        StringInput::from(&*fm),
        None,
    );

    let mut parser = Parser::new_from(lexer);

    // We cannot use parse_module because of `eval`
    let script = parser.parse_script().expect("failed to parse module");

    TsNamespaceDecl {
        span: Default::default(),
        declare: true,
        global: true,
        id: Ident::new(js_word!(""), Default::default()),
        body: Box::new(TsNamespaceBody::TsModuleBlock(TsModuleBlock {
            span: Default::default(),
            body: script
                .body
                .fold_with(&mut span_remover())
                .into_iter()
                .map(ModuleItem::Stmt)
                .collect(),
        })),
    }
}

#[test]
fn test_deps() {
    let libs = Lib::load("esnext.full");
    let mut sorted = libs.clone();
    sorted.sort();
    assert_eq!(libs, sorted);
}
