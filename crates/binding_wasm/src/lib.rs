use std::{
    path::PathBuf,
    str::FromStr,
    sync::{Arc, Mutex},
};

use anyhow::{anyhow, Result};
use serde::{Deserialize, Serialize};
use stc_ts_env::{BuiltIn, Env, ModuleConfig, Rule, StableEnv};
use stc_ts_type_checker::{
    loader::{LoadFile, ModuleLoader},
    Checker,
};
use swc_common::{
    errors::{EmitterWriter, Handler},
    FileName, Globals, SourceFile, SourceMap, GLOBALS,
};
use swc_ecma_ast::EsVersion;
use swc_ecma_loader::resolve::Resolve;
use swc_ecma_parser::Syntax;

const ENTRY_FILENAME: &str = "binding-wasm-entry-module";

#[cfg(target_arch = "wasm32")]
type Out = wasm_bindgen::JsValue;
#[cfg(not(target_arch = "wasm32"))]
type Out = CheckAndAnnotateOutput;

#[cfg_attr(target_arch = "wasm32", wasm_bindgen::prelude::wasm_bindgen)]
pub fn check_and_annotate_types(src: &str) -> Out {
    let globals = Arc::<Globals>::default();
    let cm = Arc::new(SourceMap::default());
    let buf = Arc::new(Mutex::new(Vec::new()));
    let env = GLOBALS.set(&globals, || {
        Env::new(
            StableEnv::new(),
            Rule { ..Default::default() },
            EsVersion::latest(),
            ModuleConfig::None,
            Arc::new(BuiltIn::default()),
        )
    });
    let emitter = Box::new(EmitterWriter::new(
        Box::new(PassThroughWriter { buf: buf.clone() }),
        Some(cm.clone()),
        false,
        false,
    ));
    let handler = { Arc::new(Handler::with_emitter(true, false, emitter)) };
    let (types, errors) = GLOBALS.set(&globals, || {
        let loader = SingleEntryLoader::new(cm.as_ref(), src);
        let mut checker = Checker::new(
            cm.clone(),
            handler.clone(),
            env.clone(),
            None,
            Box::new(ModuleLoader::new(cm, env, VoidResolver, loader)),
        );

        let entry_module = checker.check(Arc::new(FileName::Real(PathBuf::from_str(ENTRY_FILENAME).unwrap())));
        let types = checker.get_types(entry_module);

        (types, checker.take_errors())
    });
    let _types = types.map(|types| format!("{:?}", types)).unwrap_or_else(|| "".to_owned());

    for err in &errors {
        err.emit(&handler);
    }

    let out = CheckAndAnnotateOutput {
        error: String::from_utf8_lossy(&buf.lock().unwrap()).into_owned(),
    };

    #[cfg(target_arch = "wasm32")]
    {
        serde_wasm_bindgen::to_value(&out).unwrap()
    }

    #[cfg(not(target_arch = "wasm32"))]
    {
        out
    }
}

#[derive(Debug, Serialize, Deserialize)]
pub struct CheckAndAnnotateOutput {
    error: String,
}

pub struct PassThroughWriter {
    pub buf: Arc<Mutex<Vec<u8>>>,
}

impl std::io::Write for PassThroughWriter {
    fn write(&mut self, buf: &[u8]) -> std::io::Result<usize> {
        self.buf.lock().unwrap().write(buf)
    }

    fn flush(&mut self) -> std::io::Result<()> {
        self.buf.lock().unwrap().flush()
    }
}

pub struct VoidResolver;

impl Resolve for VoidResolver {
    fn resolve(&self, base: &FileName, _module_specifier: &str) -> std::result::Result<FileName, anyhow::Error> {
        if base != &FileName::Real(PathBuf::from_str(ENTRY_FILENAME).unwrap()) {
            return Err(anyhow!("you cannot load any module"));
        }

        Ok(base.clone())
    }
}

pub struct SingleEntryLoader {
    src: Arc<SourceFile>,
}

impl SingleEntryLoader {
    pub fn new(source_map: &SourceMap, source: impl Into<String>) -> Self {
        Self {
            src: source_map.new_source_file(FileName::Real(PathBuf::from_str(ENTRY_FILENAME).unwrap()), source.into()),
        }
    }
}

impl LoadFile for SingleEntryLoader {
    fn load_file(&self, _cm: &Arc<SourceMap>, filename: &Arc<FileName>) -> Result<(Arc<SourceFile>, Syntax)> {
        if filename.as_ref() != &FileName::Real(PathBuf::from_str(ENTRY_FILENAME).unwrap()) {
            return Err(anyhow!("you cannot load any file"));
        }

        Ok((self.src.clone(), Syntax::Typescript(Default::default())))
    }
}

#[cfg(test)]
mod test {
    use super::*;

    #[test]
    fn it_should_be_called() {
        println!("Error: {:#?}", check_and_annotate_types("export const a: string = 1;"));
    }
}
