#![feature(box_syntax)]

use anyhow::Context;
use anyhow::Error;
use ignore::WalkBuilder;
use stc_testing::get_git_root;
use stc_testing::term_logger;
use stc_ts_builtin_types::Lib;
use stc_ts_file_analyzer::env::Env;
use stc_ts_type_checker::Checker;
use std::{
    fs::read_to_string,
    path::{Path, PathBuf},
    process::{Command, Stdio},
    sync::Arc,
};
use swc_common::{
    errors::{ColorConfig, Handler},
    FileName, SourceMap, Spanned,
};
use swc_ecma_ast::{Module, ModuleDecl, ModuleItem, NamedExport, Stmt};
use swc_ecma_codegen::{text_writer::JsWriter, Emitter};
use swc_ecma_parser::{lexer::Lexer, JscTarget, Parser, StringInput, Syntax, TsConfig};
use swc_ecma_utils::drop_span;
use testing::{assert_eq, NormalizedOutput};

#[test]
#[ignore = "Not implemented yet"]
fn rxjs() -> Result<(), Error> {
    let dir = get_git_root()
        .join("vendor")
        .join("rxjs")
        .join("src")
        .canonicalize()
        .unwrap();

    tsc(&dir.join("index.ts"), &[]).unwrap();
    test_project(
        "rxjs",
        &dir,
        vec![dir.join("index.ts"), dir.join("webSocket").join("index.ts")],
    );

    Ok(())
}

#[test]
#[ignore = "Not implemented yet"]
fn vite_js() {
    let dir = get_git_root()
        .join("vendor")
        .join("vite")
        .join("src")
        .join("node")
        .canonicalize()
        .unwrap();

    tsc(&dir.join("index.ts"), &["--p", "tsconfig.base.json"]).unwrap();
    test_project("vitejs", &dir, vec![dir.join("index.ts")]);
}

#[test]
#[ignore = "Not done yet"]
fn redux() {
    let dir = get_git_root()
        .join("vendor")
        .join("redux")
        .join("src")
        .canonicalize()
        .unwrap();

    tsc(&dir.join("index.ts"), &[]).unwrap();
    test_project("redux", &dir, vec![dir.join("index.ts")]);
}

/// Invoke tsc
fn tsc(path: &Path, args: &[&str]) -> anyhow::Result<()> {
    eprintln!("tsc: {}", path.display());
    let mut c = Command::new(get_git_root().join("node_modules").join(".bin").join("tsc"));
    c.arg(path)
        .arg("--jsx")
        .arg("preserve")
        .arg("-d")
        .arg("--emitDeclarationOnly")
        .arg("--target")
        .arg("es2020")
        .arg("--lib")
        .arg("es2020,dom")
        .args(args)
        .stdout(Stdio::inherit())
        .stderr(Stdio::inherit());
    let status = c.status().context("failed to get output from tsc")?;

    dbg!(status);
    // assert!(status.success());

    Ok(())
}

fn test_project(_name: &str, dir: &Path, entries: Vec<PathBuf>) {
    ::testing::run_test2(false, |cm, _| {
        let handler = Handler::with_tty_emitter(ColorConfig::Always, true, false, Some(cm.clone()));

        let handler = Arc::new(handler);
        let mut checker = Checker::new(
            term_logger(),
            cm.clone(),
            handler.clone(),
            Env::simple(Default::default(), JscTarget::Es2020, &Lib::load("es2020.full")),
            TsConfig { ..Default::default() },
            None,
        );

        for main in entries {
            let main = Arc::new(main);

            checker.check(main);
        }

        for err in checker.take_errors() {
            handler.struct_span_err(err.span(), &format!("{:?}", err)).emit();
        }

        for entry in WalkBuilder::new(dir).git_ignore(false).build() {
            let entry = entry.unwrap();

            if entry.file_name().to_string_lossy().ends_with(".d.ts") {
                continue;
            }

            if !(entry.file_name().to_string_lossy().ends_with(".ts")
                || entry.file_name().to_string_lossy().ends_with(".tsx"))
            {
                continue;
            }

            let file_path = Arc::new(entry.path().to_path_buf());

            let id = checker.id(&file_path);
            let dts_module = match checker.take_dts(id) {
                Some(v) => v,
                None => {
                    eprintln!("Skipping: ({:?}): {}", id, file_path.display());
                    continue;
                }
            };
            eprintln!("Checking: ({:?}): {}", id, file_path.display());

            let generated_dts = drop_span(dts_module);
            let expected_dts = parse_dts(
                &cm,
                &read_to_string(file_path.with_extension("d.ts")).unwrap_or_else(|err| {
                    panic!("Failed to read .d.ts file for {}: {}", file_path.display(), err);
                }),
            );
            if generated_dts == expected_dts {
                continue;
            }

            let generated = print(&cm, &generated_dts);
            let expected = print(&cm, &expected_dts);

            if generated == expected {
                continue;
            }

            println!("---------- Input ----------\n{}", read_to_string(entry.path()).unwrap());
            println!("---------- Expected ----------\n{}", expected);
            println!("---------- Generated ----------\n{}", generated);

            assert_eq!(generated, expected);
        }

        Ok(())
    })
    .unwrap();
}

fn parse_dts(cm: &SourceMap, src: &str) -> Module {
    let fm = cm.new_source_file(FileName::Anon, src.to_string());

    let lexer = Lexer::new(
        Syntax::Typescript(TsConfig {
            dts: true,
            ..Default::default()
        }),
        JscTarget::Es2020,
        StringInput::from(&*fm),
        None,
    );

    let mut parser = Parser::new_from(lexer);
    let mut module = parser.parse_module().unwrap();

    module.body.retain(|item| match item {
        ModuleItem::ModuleDecl(ModuleDecl::ExportNamed(NamedExport { specifiers, .. })) if specifiers.is_empty() => {
            false
        }
        ModuleItem::Stmt(Stmt::Empty(..)) => false,
        _ => true,
    });

    drop_span(module)
}

fn print(cm: &Arc<SourceMap>, m: &Module) -> NormalizedOutput {
    let mut buf = vec![];
    {
        let mut emitter = Emitter {
            cfg: Default::default(),
            comments: None,
            cm: cm.clone(),
            wr: box JsWriter::new(cm.clone(), "\n", &mut buf, None),
        };

        emitter.emit_module(&m).context("failed to emit module").unwrap();
    }
    String::from_utf8(buf).unwrap().into()
}
