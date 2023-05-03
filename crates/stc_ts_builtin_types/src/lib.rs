#![deny(unused)]

use std::{
    cmp::Ordering,
    sync::{Arc, RwLock},
};

use fxhash::FxHashMap;
use once_cell::sync::Lazy;
use rayon::iter::{IntoParallelIterator, ParallelIterator};
use stc_ts_builtin_macro::builtin;
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
    pub fn load(lib_str: &str) -> Vec<Self> {
        let lib: Self = match lib_str.parse() {
            Ok(lib) => lib,
            Err(..) => return vec![],
        };

        lib.load_deps()
    }

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

    fn precedence(&self) -> usize {
        match self {
            Self::DecoratorsLegacy => 1,
            Self::Decorators => 2,
            Self::Es5 => 3,
            Self::Es2015Core => 4,
            Self::Es2015Collection => 5,
            Self::Es2015Symbol => 6,
            Self::Es2015Iterable => 7,
            Self::Es2015Generator => 8,
            Self::Es2015Promise => 9,
            Self::Es2015Proxy => 10,
            Self::Es2015Reflect => 11,
            Self::Es2015SymbolWellknown => 12,
            Self::Es2015 => 13,
            Self::Es2016ArrayInclude => 14,
            Self::Es2016 => 15,
            Self::Es2017Object => 16,
            Self::Es2017Sharedmemory => 17,
            Self::Es2017String => 18,
            Self::Es2017Intl => 19,
            Self::Es2017Typedarrays => 20,
            Self::Es2017 => 21,
            Self::Es2018Asynciterable => 22,
            Self::Es2018Asyncgenerator => 23,
            Self::Es2018Promise => 24,
            Self::Es2018Regexp => 25,
            Self::Es2018Intl => 26,
            Self::Es2018 => 27,
            Self::Es2019Array => 28,
            Self::Es2019Object => 29,
            Self::Es2019String => 30,
            Self::Es2019Symbol => 31,
            Self::Es2019Intl => 32,
            Self::Es2019 => 33,
            Self::Es2020Intl => 34,
            Self::Es2020Bigint => 35,
            Self::Es2020Date => 36,
            Self::Es2020Number => 37,
            Self::Es2020Promise => 38,
            Self::Es2020Sharedmemory => 39,
            Self::Es2020String => 40,
            Self::Es2020SymbolWellknown => 41,
            Self::Es2020 => 42,
            Self::Es2021Promise => 43,
            Self::Es2021Intl => 44,
            Self::Es2021String => 45,
            Self::Es2021Weakref => 46,
            Self::Es2021 => 47,
            Self::Es2022Array => 48,
            Self::Es2022Error => 49,
            Self::Es2022Object => 50,
            Self::Es2022Sharedmemory => 51,
            Self::Es2022String => 52,
            Self::Es2022Regexp => 53,
            Self::Es2022Intl => 54,
            Self::Es2022 => 55,
            Self::Es2023Array => 56,
            Self::Es2023 => 57,
            Self::EsnextIntl => 58,
            Self::EsnextString => 59,
            Self::EsnextPromise => 60,
            Self::EsnextWeakref => 61,
            Self::Esnext => 62,
            Self::Dom => 63,
            Self::WebworkerImportscripts => 64,
            Self::Scripthost => 65,
            Self::DomIterable => 66,
            Self::DomIterableGenerated => 67,
            Self::Header => 68,
            Self::WebworkerGenerated => 69,
            Self::WebworkerIterableGenerated => 70,

            Self::Es5Full => 100,
            Self::Es2015Full => 101,
            Self::Es2016Full => 102,
            Self::Es2017Full => 103,
            Self::Es2018Full => 104,
            Self::Es2019Full => 105,
            Self::Es2020Full => 106,
            Self::Es2021Full => 107,
            Self::Es2022Full => 108,
            Self::Es2023Full => 109,
            Self::EsnextFull => 110,
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
        self.precedence().cmp(&other.precedence())
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

impl From<&'_ tsconfig::Lib> for Lib {
    fn from(v: &'_ tsconfig::Lib) -> Self {
        match v {
            tsconfig::Lib::Es5 => Self::Es5,
            tsconfig::Lib::Es2015 => Self::Es2015,
            tsconfig::Lib::Es6 => Self::Es2015,
            tsconfig::Lib::Es2016 => Self::Es2016,
            tsconfig::Lib::Es7 => Self::Es2016,
            tsconfig::Lib::Es2017 => Self::Es2017,
            tsconfig::Lib::Es2018 => Self::Es2018,
            tsconfig::Lib::Es2019 => Self::Es2019,
            tsconfig::Lib::Es2020 => Self::Es2020,
            tsconfig::Lib::EsNext => Self::Esnext,
            tsconfig::Lib::Dom => Self::Dom,
            tsconfig::Lib::WebWorker => Self::WebworkerGenerated,
            tsconfig::Lib::ScriptHost => Self::Scripthost,
            tsconfig::Lib::DomIterable => Self::DomIterable,
            tsconfig::Lib::Es2015Core => Self::Es2015Core,
            tsconfig::Lib::Es2015Generator => Self::Es2015Generator,
            tsconfig::Lib::Es2015Iterable => Self::Es2015Iterable,
            tsconfig::Lib::Es2015Promise => Self::Es2015Promise,
            tsconfig::Lib::Es2015Proxy => Self::Es2015Proxy,
            tsconfig::Lib::Es2015Reflect => Self::Es2015Reflect,
            tsconfig::Lib::Es2015Symbol => Self::Es2015Symbol,
            tsconfig::Lib::Es2015SymbolWellKnown => Self::Es2015SymbolWellknown,
            tsconfig::Lib::Es2016ArrayInclude => Self::Es2016ArrayInclude,
            tsconfig::Lib::Es2017Object => Self::Es2017Object,
            tsconfig::Lib::Es2017Intl => Self::Es2017Intl,
            tsconfig::Lib::Es2017SharedMemory => Self::Es2017Sharedmemory,
            tsconfig::Lib::Es2017String => Self::Es2017String,
            tsconfig::Lib::Es2017TypedArrays => Self::Es2017Typedarrays,
            tsconfig::Lib::Es2018Intl => Self::Es2018Intl,
            tsconfig::Lib::Es2018Promise => Self::Es2018Promise,
            tsconfig::Lib::Es2018RegExp => Self::Es2018Regexp,
            tsconfig::Lib::Es2019Array => Self::Es2019Array,
            tsconfig::Lib::Es2019Object => Self::Es2019Object,
            tsconfig::Lib::Es2019String => Self::Es2019String,
            tsconfig::Lib::Es2019Symbol => Self::Es2019Symbol,
            tsconfig::Lib::Es2020String => Self::Es2020String,
            tsconfig::Lib::Es2020SymbolWellknown => Self::Es2020SymbolWellknown,
            tsconfig::Lib::EsNextAsyncIterable => Self::Es2018Asynciterable,
            tsconfig::Lib::EsNextArray => Self::Es2023Array,
            tsconfig::Lib::EsNextIntl => Self::EsnextIntl,
            tsconfig::Lib::EsNextSymbol => Self::Es2019Symbol,
            tsconfig::Lib::Other(s) => s.parse().expect("failed to parse lib"),
        }
    }
}
