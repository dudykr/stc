//! A module to validate while type checking
use crate::ty::{self, Ref, Type};
use backtrace::Backtrace;
use slog::Logger;
use stc_types::{Fold, FoldWith, Id, TypeNode, VisitWith};
use std::io::{stderr, stdout};
use swc_atoms::js_word;
use swc_common::{sync::Lrc, FilePathMapping, SourceMap, DUMMY_SP};
use swc_ecma_ast::*;
use swc_ecma_codegen::{text_writer::JsWriter, Emitter};

pub mod duplicate;

pub fn print_type(logger: &Logger, name: &str, cm: &Lrc<SourceMap>, t: &Type) {
    let mut buf = vec![];
    {
        let mut emitter = Emitter {
            cfg: swc_ecma_codegen::Config { minify: false },
            cm: cm.clone(),
            comments: None,
            wr: box JsWriter::new(cm.clone(), "\n", &mut buf, None),
        };

        emitter
            .emit_module(&Module {
                span: DUMMY_SP,
                body: vec![ModuleItem::Stmt(Stmt::Expr(ExprStmt {
                    span: DUMMY_SP,
                    expr: box Expr::TsAs(TsAsExpr {
                        span: DUMMY_SP,
                        expr: box Expr::Ident(Ident::new("TYPE".into(), DUMMY_SP)),
                        type_ann: t.clone().fold_with(&mut Visualizer).into(),
                    }),
                }))],
                shebang: None,
            })
            .unwrap();
    }
    let s = String::from_utf8_lossy(&buf);
    slog::info!(
        logger,
        "===== ===== ===== Type ({}) ===== ===== =====\n{}",
        name,
        s
    );
}

/// Ensures that `ty` does not **contain** [Type::Ref].
pub fn assert_no_ref(ty: &Type) {
    struct Visitor {
        found: bool,
    }
    impl ty::Visit for Visitor {
        fn visit_ref(&mut self, _: &Ref, _: &dyn TypeNode) {
            self.found = true;
        }
    }

    let mut v = Visitor { found: false };
    ty.visit_with(ty, &mut v);
    if v.found {
        print_backtrace();
        unreachable!("A type ({:#?}) should not contain unresolved reference", ty)
    }
}

pub fn print_backtrace() {
    if cfg!(debug_assertions) {
        let bt = Backtrace::new();
        let bt = filter(bt);

        let s = format!("{:?}", bt);

        println!("{}", s);
    }
}

fn filter(mut bt: Backtrace) -> Backtrace {
    bt.resolve();
    let mut frames: Vec<_> = bt.into();
    let mut done = false;

    frames.retain(|frame| {
        if done {
            return false;
        }

        let symbols = frame.symbols();
        let len = symbols.len();
        for symbol in symbols {
            let name = if let Some(name) = symbol.name().and_then(|s| s.as_str()) {
                name
            } else {
                return false;
            };

            if let Some(filename) = symbol.filename() {
                let s = filename.to_string_lossy();

                if s.contains("backtrace")
                    || s.contains("libcore")
                    || s.contains("libstd")
                    || s.contains("/libtest/")
                    || s.contains("/rustc/")
                    || s.contains("rust/library")
                    || s.contains("libpanic_unwind/")
                    || s.contains("/ecmascript/visit/")
                    || s.contains("swc_visit")
                    || s.contains("types/src/visit.rs")
                {
                    return false;
                }

                if len == 1 {
                    if s.contains("scoped-tls") {}

                    if s.contains("/ast/") {
                        return false;
                    }

                    if s.contains("common") && s.ends_with("/fold.rs") {
                        return false;
                    }

                    if s.contains("checker") && s.ends_with("/validator.rs") {
                        return false;
                    }
                }

                //                println!("({}) Filename: {}", len, s);
            }

            if name.contains("Module") {
                done = true;
                // Last one
                return true;
            }
        }

        true
    });

    frames.into()
}

struct Visualizer;
impl Fold for Visualizer {
    fn fold_id(&mut self, id: Id) -> Id {
        Id::word(format!("{}", id).into())
    }

    fn fold_type(&mut self, mut ty: Type) -> Type {
        ty = ty.fold_children_with(self);

        match ty {
            Type::Module(m) => *Type::any(m.span),
            _ => ty,
        }
    }
}
