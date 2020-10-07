#![feature(box_syntax)]
#![feature(box_patterns)]

use anyhow::{bail, Context, Error};
use sha1::{Digest, Sha1};
use slog::{Discard, Logger};
use stc_checker::{env::Env, Checker, Lib};
use std::{fs, panic::AssertUnwindSafe, path::Path, process::Command, sync::Arc};
use swc_common::{
    errors::{DiagnosticBuilder, Handler},
    sync::Lazy,
    SourceMap,
};
pub use swc_ecma_ast::Module;
use swc_ecma_ast::{ClassMember, Invalid, ModuleItem, Stmt, Tpl, VarDeclarator};
use swc_ecma_codegen::{text_writer::JsWriter, Emitter};
use swc_ecma_parser::{lexer::Lexer, JscTarget, Parser, StringInput, Syntax, TsConfig};
use swc_ecma_utils::drop_span;
use swc_ecma_visit::{VisitMut, VisitMutWith};

pub static ENV: Lazy<Env> = Lazy::new(|| {
    Env::simple(
        Default::default(),
        JscTarget::Es2020,
        &Lib::load("es2020.full"),
    )
});

pub fn to_str(cm: &Arc<SourceMap>, m: &Module) -> Result<String, Error> {
    let buf = std::panic::catch_unwind(AssertUnwindSafe(|| -> Result<_, Error> {
        {
            let mut buf = vec![];
            {
                let mut emitter = Emitter {
                    cfg: Default::default(),
                    comments: None,
                    // It's safe as we dropped span above.
                    cm: cm.clone(),
                    wr: box JsWriter::new(cm.clone(), "\n", &mut buf, None),
                };

                emitter.emit_module(&m).context("failed to emit module")?;
            }

            Ok(buf)
        }
    }))
    .unwrap_or_else(|_| Ok(vec![]))
    .unwrap_or_default();
    let src = String::from_utf8_lossy(&buf);

    Ok(src.to_string())
}

struct Validator {
    /// True if we cannot fix.
    invalid: bool,
}

impl VisitMut for Validator {
    fn visit_mut_invalid(&mut self, _: &mut Invalid) {
        self.invalid = true;
    }

    fn visit_mut_tpl(&mut self, t: &mut Tpl) {
        if t.quasis.len() != t.exprs.len() + 1 {
            self.invalid = true;
            return;
        }

        t.visit_mut_children_with(self);
    }

    fn visit_mut_stmts(&mut self, stmts: &mut Vec<Stmt>) {
        stmts.visit_mut_children_with(self);

        stmts.retain(|stmt| match stmt {
            Stmt::Empty(..) => false,
            _ => true,
        });
    }

    fn visit_mut_module_items(&mut self, items: &mut Vec<ModuleItem>) {
        items.visit_mut_children_with(self);

        items.retain(|item| match item {
            ModuleItem::Stmt(Stmt::Empty(..)) => false,
            _ => true,
        });
    }

    fn visit_mut_class_members(&mut self, members: &mut Vec<ClassMember>) {
        members.visit_mut_children_with(self);

        members.retain(|member| match member {
            ClassMember::Empty(_) => false,
            _ => true,
        });
    }

    fn visit_mut_var_declarators(&mut self, decls: &mut Vec<VarDeclarator>) {
        if decls.is_empty() {
            self.invalid = true;
        }

        decls.visit_mut_children_with(self)
    }
}

fn is_valid(module: &mut Module) -> bool {
    let mut validator = Validator { invalid: false };
    module.visit_mut_with(&mut validator);
    !validator.invalid
}

fn calc_hash(s: &[u8]) -> String {
    // create a Sha1 object
    let mut hasher = Sha1::new();

    // process input message
    hasher.update(s);

    // acquire hash digest in the form of GenericArray,
    // which in this case is equivalent to [u8; 20]
    let result = hasher.finalize();

    hex::encode(result.as_slice())
}

pub fn check_with_tsc(mut module: Module) -> Result<(), Error> {
    if !is_valid(&mut module) {
        bail!("Invalid module");
    }

    let module = drop_span(module);
    let cm = Arc::new(SourceMap::default());

    let dir = tempfile::tempdir().context("failed to create temp directory")?;
    let path = dir.path().join("file.ts");
    let hash;
    {
        let src = to_str(&cm, &module)?;

        eprintln!("Source:\n{}\n", src);
        fs::write(&path, src.as_bytes()).context("failed to write module")?;
        hash = calc_hash(src.as_bytes());
    }

    let test_dir = Path::new("typescript")
        .join("dts")
        .join("tests")
        .join("fixtures")
        .join("fuzzed");
    // fast path
    let test_file_path = test_dir.join(&hash).with_extension("ts");
    if test_file_path.exists() {
        return Ok(());
    }

    let actual_dts = std::panic::catch_unwind(AssertUnwindSafe(|| {
        let handler = Arc::new(Handler::with_emitter(false, false, box Ignore));
        let checker = Checker::new(
            Logger::root(Discard, slog::o!()),
            cm.clone(),
            handler.clone(),
            ENV.clone(),
            TsConfig {
                decorators: true,
                ..Default::default()
            },
        );

        let id = checker.check(Arc::new(path.clone().into()));
        let dts_module = checker.take_dts(id);

        dts_module.unwrap()
    }))
    .unwrap_or_else(|_| Module {
        span: Default::default(),
        body: Default::default(),
        shebang: Default::default(),
    });

    let expected_dts = get_expected_dts(&cm, &path).context("failed to get expected dts")?;
    let expected_dts = drop_span(expected_dts);

    if actual_dts == expected_dts {
        return Ok(());
    }

    {
        // Compare using string output
        let actual_dts =
            to_str(&cm, &actual_dts).context("failed to print `.d.ts` file generated by stc")?;
        let expected_dts =
            to_str(&cm, &expected_dts).context("failed to print `.d.ts` file generated by tsc")?;

        if actual_dts == expected_dts {
            return Ok(());
        }

        eprintln!("Actaul: \n{}", actual_dts);
        eprintln!("Expected: \n{}", expected_dts);
    }

    fs::create_dir_all(&test_dir).context("failed to create directory")?;

    fs::copy(&path, &test_file_path).context("failed to copy generated typescript file")?;

    Ok(())
}

/// Generates a `.d.ts` file using official typescript compiler.
fn get_expected_dts(cm: &Arc<SourceMap>, path: &Path) -> Result<Module, Error> {
    let mut c = Command::new(Path::new("node_modules").join(".bin").join("tsc"));
    c.arg(&path)
        .arg("--jsx")
        .arg("preserve")
        .arg("-d")
        .arg("--emitDeclarationOnly")
        .arg("--target")
        .arg("es2020")
        .arg("--lib")
        .arg("es2020");
    let _output = c.output().unwrap();

    if !path.with_extension("d.ts").exists() {
        bail!("Failed to generated expected `.d.ts` file")
    }

    let dts_path = path.with_extension("d.ts");
    let fm = cm.load_file(&dts_path)?;
    let lexer = Lexer::new(
        Syntax::Typescript(TsConfig {
            dts: true,
            dynamic_import: true,
            decorators: true,
            ..Default::default()
        }),
        JscTarget::Es2020,
        StringInput::from(&*fm),
        None,
    );
    let mut parser = Parser::new_from(lexer);

    let module = parser
        .parse_module()
        .map_err(|err| Error::msg(format!("Failed to parse: {:?}", err)))?;

    Ok(module)
}

struct Ignore;

impl swc_common::errors::Emitter for Ignore {
    fn emit(&mut self, _: &DiagnosticBuilder<'_>) {}
}
