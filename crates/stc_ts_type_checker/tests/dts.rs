#![recursion_limit = "256"]
#![feature(box_syntax)]
#![feature(box_patterns)]
#![feature(test)]
#![allow(clippy::vec_box)]

extern crate test;

use std::{
    env,
    fs::{self, canonicalize, read_to_string, File},
    io::Read,
    path::{Path, PathBuf},
    process::Command,
    sync::Arc,
};

use anyhow::Context;
use stc_testing::get_git_root;
use stc_ts_builtin_types::Lib;
use stc_ts_env::{Env, ModuleConfig};
use stc_ts_file_analyzer::env::EnvFactory;
use stc_ts_module_loader::resolvers::node::NodeResolver;
use stc_ts_type_checker::Checker;
use swc_common::{input::SourceFileInput, FileName, SyntaxContext};
use swc_ecma_ast::{EsVersion, Ident, Module, TsIntersectionType, TsKeywordTypeKind, TsLit, TsLitType, TsType, TsUnionType};
use swc_ecma_codegen::{text_writer::JsWriter, Emitter};
use swc_ecma_parser::{lexer::Lexer, Parser, StringInput, Syntax, TsConfig};
use swc_ecma_utils::drop_span;
use swc_ecma_visit::{Fold, FoldWith};
use testing::{assert_eq, NormalizedOutput, StdErr};

#[testing_macros::fixture("tests/dts/**/*.ts", exclude(".*.d\\.ts"))]
fn fixture(input: PathBuf) {
    // Currently disabled because .d.ts is broken
    if true {
        return;
    }

    do_test(&input).unwrap();
}

fn do_test(file_name: &Path) -> Result<(), StdErr> {
    if let Ok(test) = env::var("TEST") {
        if !file_name.to_string_lossy().replace('/', "::").contains(&test) {
            return Ok(());
        }
    }

    let file_name = canonicalize(file_name).unwrap();

    {
        let s = read_to_string(&file_name).unwrap();
        println!("---------- Input ----------\n{}", s);
    }

    let fname = file_name.display().to_string();
    let (expected_code, expected_module) = get_correct_dts(&file_name);
    let expected_module = drop_span(expected_module.fold_with(&mut Normalizer));
    println!("---------- Expected ----------\n{}", expected_code);

    let mut generated = NormalizedOutput::from(String::from("<iinvalid>"));
    let mut expected = NormalizedOutput::from(String::from("<iinvalid>"));

    let res = testing::Tester::new().print_errors(|cm, handler| {
        let handler = Arc::new(handler);

        let mut checker = Checker::new(
            cm.clone(),
            handler.clone(),
            Env::simple(
                Default::default(),
                EsVersion::latest(),
                ModuleConfig::None,
                &Lib::load("es2019.full"),
            ),
            TsConfig {
                tsx: fname.contains("tsx"),
                decorators: true,
                ..Default::default()
            },
            None,
            Arc::new(NodeResolver),
        );

        let id = checker.check(Arc::new(file_name.clone().into()));

        let errors = ::stc_ts_errors::ErrorKind::flatten(checker.take_errors());

        let expected_module = {
            let mut buf = vec![];
            {
                let mut emitter = Emitter {
                    cfg: Default::default(),
                    comments: None,
                    cm: cm.clone(),
                    wr: box JsWriter::new(cm.clone(), "\n", &mut buf, None),
                };

                emitter
                    .emit_module(&drop_span(expected_module.clone()))
                    .context("failed to emit module")
                    .unwrap();
            }
            String::from_utf8(buf).unwrap()
        };

        let dts: Module = checker.take_dts(id).unwrap().fold_with(&mut Normalizer);

        let generated_module = {
            let mut buf = vec![];
            {
                let mut emitter = Emitter {
                    cfg: Default::default(),
                    comments: None,
                    cm: cm.clone(),
                    wr: box JsWriter::new(cm.clone(), "\n", &mut buf, None),
                };

                emitter
                    .emit_module(&drop_span(dts.clone()))
                    .context("failed to emit module")
                    .unwrap();
            }
            String::from_utf8(buf).unwrap()
        };

        if env::var("PRINT_HYGIENE").unwrap_or_else(|_| "".into()) == "1" {
            let hygiene = {
                let mut buf = vec![];
                {
                    let mut emitter = Emitter {
                        cfg: Default::default(),
                        comments: None,
                        cm: cm.clone(),
                        wr: box JsWriter::new(cm, "\n", &mut buf, None),
                    };

                    emitter
                        .emit_module(&dts.fold_with(&mut HygieneVisualizer))
                        .context("failed to emit module")
                        .unwrap();
                }
                String::from_utf8(buf).unwrap()
            };

            println!("---------- HYGIENE ----------\n{}", hygiene);
        }

        println!("---------- Generated ----------\n{}", generated_module);

        generated = NormalizedOutput::from(generated_module);
        expected = NormalizedOutput::from(expected_module);
        if generated == expected {
            return Ok(());
        }

        checker.run(|| {
            for e in errors {
                e.emit(&handler);
            }
        });

        let expected_dts = parse_dts(&expected);
        let generated_dts = parse_dts(&generated);

        if expected_dts == generated_dts {
            return Ok(());
        }

        Err(())
    });

    if res.is_ok() && file_name.as_os_str().to_string_lossy().contains("fuzzed") {
        fs::remove_file(&file_name).expect("failed to remove the fixed fuzzing test");
    }

    let msg = match res {
        Ok(()) => String::from("").into(),
        Err(v) => v,
    };

    assert_eq!(generated, expected, "\n{}", msg);

    Ok(())
}

fn parse_dts(src: &str) -> Module {
    ::testing::run_test2(false, |cm, handler| {
        let fm = cm.new_source_file(FileName::Anon, src.to_string());

        let lexer = Lexer::new(
            Syntax::Typescript(TsConfig {
                dts: true,
                ..Default::default()
            }),
            EsVersion::latest(),
            StringInput::from(&*fm),
            None,
        );

        let mut parser = Parser::new_from(lexer);
        let module = match parser.parse_module() {
            Ok(v) => v,
            Err(err) => {
                err.into_diagnostic(&handler).emit();
                return Err(());
            }
        };
        let module = module.fold_with(&mut Normalizer);
        Ok(drop_span(module))
    })
    .unwrap()
}

fn get_correct_dts(path: &Path) -> (Arc<String>, Module) {
    testing::run_test2(false, |cm, handler| {
        let dts_file = path
            .parent()
            .unwrap()
            .join(format!("{}.d.ts", path.file_stem().unwrap().to_string_lossy()));

        if !dts_file.exists() {
            let mut c = Command::new(get_git_root().join("node_modules").join(".bin").join("tsc"));
            c.arg(path)
                .arg("--jsx")
                .arg("preserve")
                .arg("-d")
                .arg("--emitDeclarationOnly")
                .arg("--target")
                .arg("es2020")
                .arg("--lib")
                .arg("es2020");
            let output = c.output().unwrap();

            if !dts_file.exists() && !output.status.success() {
                panic!(
                    "Failed to get correct dts file\n{}\n{}",
                    String::from_utf8_lossy(&output.stdout),
                    String::from_utf8_lossy(&output.stderr),
                );
            }
        }

        let content = {
            let mut buf = String::new();
            let mut f = File::open(&dts_file).expect("failed to open generated .d.ts file");
            f.read_to_string(&mut buf).unwrap();
            buf.replace("export {};", "")
        };

        let fm = cm.new_source_file(FileName::Real(dts_file), content);

        let mut p = Parser::new(
            Syntax::Typescript(TsConfig {
                tsx: true,
                decorators: true,
                dts: true,
                no_early_errors: true,
            }),
            SourceFileInput::from(&*fm),
            None,
        );

        let m = p.parse_typescript_module().map_err(|e| e.into_diagnostic(&handler).emit())?;

        Ok((fm.src.clone(), m))
    })
    .unwrap()
}

struct Normalizer;
impl Normalizer {
    fn sort(&mut self, mut types: Vec<Box<TsType>>) -> Vec<Box<TsType>> {
        fn kwd_rank(kind: TsKeywordTypeKind) -> u8 {
            match kind {
                TsKeywordTypeKind::TsNumberKeyword => 0,
                TsKeywordTypeKind::TsStringKeyword => 1,
                TsKeywordTypeKind::TsBooleanKeyword => 2,
                _ => 4,
            }
        }

        fn rank(ty: &TsType) -> usize {
            match ty {
                TsType::TsKeywordType(_) => 1000,
                TsType::TsThisType(_) => 2000,
                TsType::TsFnOrConstructorType(_) => 3000,
                TsType::TsTypeRef(_) => 4000,
                TsType::TsTypeQuery(_) => 5000,
                TsType::TsTypeLit(_) => 6000,
                TsType::TsArrayType(_) => 7000,
                TsType::TsTupleType(_) => 8000,
                TsType::TsOptionalType(_) => 9000,
                TsType::TsRestType(_) => 10000,
                TsType::TsUnionOrIntersectionType(_) => 11000,
                TsType::TsConditionalType(_) => 12000,
                TsType::TsInferType(_) => 13000,
                TsType::TsParenthesizedType(_) => 14000,
                TsType::TsTypeOperator(_) => 15000,
                TsType::TsIndexedAccessType(_) => 16000,
                TsType::TsMappedType(_) => 17000,
                TsType::TsLitType(ty) => match &ty.lit {
                    TsLit::Number(v) => 18000 + v.value.round() as usize,
                    TsLit::Str(_) => 18100,
                    TsLit::Bool(_) => 18200,
                    TsLit::BigInt(_) => 18300,
                    TsLit::Tpl(_) => 18400,
                },
                TsType::TsTypePredicate(_) => 19000,
                TsType::TsImportType(_) => 20000,
            }
        }

        types.sort_by(|a, b| match (&**a, &**b) {
            (&TsType::TsKeywordType(ref a), &TsType::TsKeywordType(ref b)) => kwd_rank(a.kind).cmp(&kwd_rank(b.kind)),
            (
                &TsType::TsLitType(TsLitType {
                    lit: TsLit::Str(ref a), ..
                }),
                &TsType::TsLitType(TsLitType {
                    lit: TsLit::Str(ref b), ..
                }),
            ) => a.value.cmp(&b.value),

            _ => rank(&a).cmp(&rank(&b)),
        });

        types
    }
}

/// Sorts the type.
impl swc_ecma_visit::Fold for Normalizer {
    fn fold_ts_union_type(&mut self, mut ty: TsUnionType) -> TsUnionType {
        ty = ty.fold_children_with(self);
        ty.types = self.sort(ty.types);
        ty
    }

    fn fold_ts_intersection_type(&mut self, mut ty: TsIntersectionType) -> TsIntersectionType {
        ty = ty.fold_children_with(self);
        ty.types = self.sort(ty.types);
        ty
    }
}

struct HygieneVisualizer;

impl Fold for HygieneVisualizer {
    fn fold_ident(&mut self, mut i: Ident) -> Ident {
        if i.span.ctxt == SyntaxContext::empty() {
            return i;
        }

        i.sym = format!("{}{:?}", i.sym, i.span.ctxt).into();

        i
    }
}
