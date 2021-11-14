#![recursion_limit = "256"]
#![feature(box_syntax)]
#![feature(box_patterns)]
#![feature(test)]

extern crate test;

use anyhow::Context;
use rnode::{NodeIdGenerator, RNode};
use stc_testing::get_git_root;
use stc_ts_ast_rnode::RModule;
use stc_ts_builtin_types::Lib;
use stc_ts_dts::{apply_mutations, cleanup_module_for_dts};
use stc_ts_env::{Env, ModuleConfig};
use stc_ts_file_analyzer::{
    analyzer::{Analyzer, NoopLoader},
    env::EnvFactory,
    validator::ValidateWith,
};
use stc_ts_storage::Single;
use stc_ts_types::module_id;
use stc_ts_utils::StcComments;
use std::{
    env,
    fs::{self, canonicalize, read_to_string, File},
    io::Read,
    path::{Path, PathBuf},
    process::Command,
    sync::Arc,
};
use swc_common::{input::SourceFileInput, FileName, SyntaxContext, GLOBALS};
use swc_ecma_ast::{
    EsVersion, Ident, Module, TsIntersectionType, TsKeywordTypeKind, TsLit, TsLitType, TsType, TsUnionType,
};
use swc_ecma_codegen::{text_writer::JsWriter, Emitter};
use swc_ecma_parser::{lexer::Lexer, Parser, StringInput, Syntax, TsConfig};
use swc_ecma_transforms::resolver::ts_resolver;
use swc_ecma_utils::drop_span;
use swc_ecma_visit::{Fold, FoldWith};
use testing::{assert_eq, NormalizedOutput, StdErr};

#[testing_macros::fixture("fixture/**/*.ts", exclude(".*.d\\.ts"))]
fn fixture(input: PathBuf) {
    do_test(&input).unwrap();
}

fn do_test(file_name: &Path) -> Result<(), StdErr> {
    if let Ok(test) = env::var("TEST") {
        if !file_name.to_string_lossy().replace("/", "::").contains(&test) {
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
        let fm = cm.load_file(&file_name).unwrap();
        let env = Env::simple(
            Default::default(),
            EsVersion::latest(),
            ModuleConfig::None,
            &Lib::load("es2019.full"),
        );
        let stable_env = env.shared().clone();
        let generator = module_id::ModuleIdGenerator::default();
        let path = Arc::new(FileName::Real(file_name.clone()));

        let mut storage = Single {
            parent: None,
            id: generator.generate(&path),
            path,
            info: Default::default(),
        };

        let mut node_id_gen = NodeIdGenerator::default();
        let comments = StcComments::default();

        let lexer = Lexer::new(
            Syntax::Typescript(TsConfig {
                tsx: fname.contains("tsx"),
                decorators: true,
                ..Default::default()
            }),
            EsVersion::latest(),
            SourceFileInput::from(&*fm),
            Some(&comments),
        );
        let mut parser = Parser::new_from(lexer);
        let module = parser.parse_module().unwrap();
        let module = GLOBALS.set(stable_env.swc_globals(), || {
            module.fold_with(&mut ts_resolver(stable_env.marks().top_level_mark()))
        });
        let mut module = RModule::from_orig(&mut node_id_gen, module);
        let mut mutations;
        {
            let mut analyzer = Analyzer::root(env, cm.clone(), box &mut storage, &NoopLoader, None);
            GLOBALS.set(stable_env.swc_globals(), || {
                module.validate_with(&mut analyzer).unwrap();
            });

            mutations = analyzer.mutations.unwrap()
        }

        {
            apply_mutations(&mut mutations, &mut module);
            cleanup_module_for_dts(&mut module.body, &storage.info.exports);
        }

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

        // Actual dts file.
        let dts = module.into_orig().fold_with(&mut Normalizer);

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
                        wr: box JsWriter::new(cm.clone(), "\n", &mut buf, None),
                    };

                    emitter
                        .emit_module(&dts.clone().fold_with(&mut HygieneVisualizer))
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

        for e in storage.info.errors {
            e.emit(&handler);
        }

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
                dynamic_import: true,
                dts: true,
                no_early_errors: true,
                import_assertions: false,
            }),
            SourceFileInput::from(&*fm),
            None,
        );

        let m = p
            .parse_typescript_module()
            .map_err(|e| e.into_diagnostic(&handler).emit())?;

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
                TsType::TsLitType(ty) => match ty.lit {
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
