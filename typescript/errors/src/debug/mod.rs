//! A module to validate while type checking
use backtrace::Backtrace;
use rnode::{Fold, FoldWith, RNode, Visit, VisitWith};
use stc_ts_ast_rnode::RTsType;
use stc_ts_types::{Id, IndexedAccessType, Ref, Type, TypeLit, TypeParam};
use stc_utils::cache::ALLOW_DEEP_CLONE;
use std::{collections::HashSet, fmt::Write};
use swc_common::{sync::Lrc, SourceMap, TypeEq, DUMMY_SP};
use swc_ecma_ast::*;
use swc_ecma_codegen::{text_writer::JsWriter, Emitter};
use tracing::info;

pub mod debugger;

pub fn dump_type_as_string(cm: &Lrc<SourceMap>, t: &Type) -> String {
    if !cfg!(debug_assertions) {
        return String::new();
    }

    let mut buf = vec![];
    {
        let mut emitter = Emitter {
            cfg: swc_ecma_codegen::Config { minify: false },
            cm: cm.clone(),
            comments: None,
            wr: box JsWriter::new(cm.clone(), "\n", &mut buf, None),
        };

        let mut body = vec![];
        body.push(ModuleItem::Stmt(Stmt::Expr(ExprStmt {
            span: DUMMY_SP,
            expr: box Expr::TsAs(TsAsExpr {
                span: DUMMY_SP,
                expr: box Expr::Ident(Ident::new("TYPE".into(), DUMMY_SP)),
                type_ann: box RTsType::from(
                    ALLOW_DEEP_CLONE.set(&(), || t.clone().fold_with(&mut Visualizer::default())),
                )
                .into_orig(),
            }),
        })));

        match t.normalize() {
            Type::Interface(t) => ALLOW_DEEP_CLONE.set(&(), || {
                body.push(ModuleItem::Stmt(Stmt::Expr(ExprStmt {
                    span: DUMMY_SP,
                    expr: box Expr::TsAs(TsAsExpr {
                        span: DUMMY_SP,
                        expr: box Expr::Ident(Ident::new("Member".into(), DUMMY_SP)),
                        type_ann: box RTsType::from(
                            Type::TypeLit(TypeLit {
                                span: DUMMY_SP,
                                members: t.body.clone(),
                                metadata: Default::default(),
                            })
                            .fold_with(&mut Visualizer::default()),
                        )
                        .into_orig(),
                    }),
                })));
            }),
            _ => {}
        }

        emitter
            .emit_module(&Module {
                span: DUMMY_SP,
                body,
                shebang: None,
            })
            .unwrap();
    }
    let mut s = String::from_utf8_lossy(&buf).replace("TYPE as", "");

    if t.normalize().is_instance() {
        s = format!("instanceof {}", s)
    }

    match t.normalize() {
        Type::ClassDef(..) | Type::Class(..) => {
            writeln!(s, "\n{:?}", t.normalize()).unwrap();
        }
        _ => {}
    }

    let s = s.trim();

    s.to_string()
}

pub fn dbg_type(name: &str, cm: &Lrc<SourceMap>, t: &Type) {
    let s = dump_type_as_string(cm, t);
    eprintln!("===== ===== ===== Type ({}) ===== ===== =====\n{}", name, s);
}
pub fn print_type(name: &str, cm: &Lrc<SourceMap>, t: &Type) {
    let s = dump_type_as_string(cm, t);
    info!("===== ===== ===== Type ({}) ===== ===== =====\n{}", name, s);
}

/// Ensures that `ty` does not **contain** [Type::Ref].
pub fn assert_no_ref(ty: &Type) {
    struct Visitor {
        found: bool,
    }
    impl Visit<Ref> for Visitor {
        fn visit(&mut self, _: &Ref) {
            self.found = true;
        }
    }

    let mut v = Visitor { found: false };
    ty.visit_with(&mut v);
    if v.found {
        print_backtrace();
        unreachable!("A type ({:#?}) should not contain unresolved reference", ty)
    }
}

pub fn print_backtrace() {
    if cfg!(debug_assertions) {
        let s = dump_backtace();

        println!("{}", s);
    }
}

pub fn dump_backtace() -> String {
    if cfg!(debug_assertions) {
        let bt = Backtrace::new();
        let bt = filter(bt);

        format!("{:?}", bt)
    } else {
        String::new()
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
                    || s.contains("/visit/")
                    || s.contains("/validator.rs")
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

#[derive(Default)]
struct Visualizer {
    done: HashSet<Id>,
    done_types: Vec<Type>,
}
impl Fold<Id> for Visualizer {
    fn fold(&mut self, id: Id) -> Id {
        Id::word(format!("{}", id).into())
    }
}

impl Fold<TypeParam> for Visualizer {
    fn fold(&mut self, mut ty: TypeParam) -> TypeParam {
        ty.name = ty.name.fold_with(self);
        if !self.done.insert(ty.name.clone()) {
            return ty;
        }
        ty.fold_children_with(self)
    }
}

/// Noop because of stack overflow.
impl Fold<IndexedAccessType> for Visualizer {
    fn fold(&mut self, ty: IndexedAccessType) -> IndexedAccessType {
        ty
    }
}

impl Fold<Type> for Visualizer {
    fn fold(&mut self, mut ty: Type) -> Type {
        if self.done_types.iter().any(|prev| prev.type_eq(&ty)) {
            return ty;
        }

        ty = ty.foldable();

        self.done_types.push(ty.clone());

        ty = ty.fold_children_with(self);

        match ty {
            Type::Module(m) => Type::any(m.span, Default::default()),
            _ => ty,
        }
    }
}
