//! A module to validate while type checking
use std::{collections::HashSet, fmt::Write};

use backtrace::Backtrace;
use fxhash::FxHashMap;
use rnode::{Fold, FoldWith, RNode, Visit, VisitWith};
use stc_ts_ast_rnode::RTsType;
use stc_ts_types::{Id, IndexedAccessType, Ref, Type, TypeLit, TypeParam};
use stc_utils::cache::ALLOW_DEEP_CLONE;
use swc_common::{sync::Lrc, SourceMap, SourceMapper, TypeEq, DUMMY_SP};
use swc_ecma_ast::*;
use swc_ecma_codegen::{text_writer::JsWriter, Emitter};
use swc_ecma_utils::DropSpan;
use swc_ecma_visit::VisitMutWith;
use tracing::{info, Level};

pub mod debugger;

pub fn dump_type_map(map: &FxHashMap<Id, Type>) -> String {
    if !cfg!(debug_assertions) {
        return String::new();
    }

    map.iter()
        .map(|(name, ty)| format!("{:?}: {}", name, dump_type_as_string(ty)))
        .collect::<Vec<_>>()
        .join("\n")
}

struct FakeSourceMap;

impl SourceMapper for FakeSourceMap {
    fn lookup_char_pos(&self, _pos: swc_common::BytePos) -> swc_common::Loc {
        todo!()
    }

    fn span_to_lines(&self, _sp: swc_common::Span) -> swc_common::source_map::FileLinesResult {
        todo!()
    }

    fn span_to_string(&self, _sp: swc_common::Span) -> String {
        todo!()
    }

    fn span_to_filename(&self, _sp: swc_common::Span) -> swc_common::FileName {
        todo!()
    }

    fn merge_spans(&self, _sp_lhs: swc_common::Span, _sp_rhs: swc_common::Span) -> Option<swc_common::Span> {
        todo!()
    }

    fn call_span_if_macro(&self, _sp: swc_common::Span) -> swc_common::Span {
        todo!()
    }

    fn doctest_offset_line(&self, _line: usize) -> usize {
        todo!()
    }

    fn span_to_snippet(&self, _sp: swc_common::Span) -> Result<String, Box<swc_common::SpanSnippetError>> {
        todo!()
    }
}

impl SourceMapperExt for FakeSourceMap {
    fn get_code_map(&self) -> &dyn SourceMapper {
        self
    }
}

pub fn dump_type_as_string(t: &Type) -> String {
    if !tracing::enabled!(Level::WARN) {
        return String::new();
    }

    force_dump_type_as_string(t)
}

pub fn force_dump_type_as_string(t: &Type) -> String {
    if !cfg!(debug_assertions) {
        return String::new();
    }

    if let Type::StringMapping(t) = t.normalize() {
        return format!("intrinsic:{:?}<{}>", t.kind, force_dump_type_as_string(&t.type_args.params[0]));
    }

    if let Type::Module(..) | Type::Namespace(..) = t.normalize() {
        return format!("{:?}", t.normalize());
    }

    let mut buf = vec![];
    {
        let mut emitter = Emitter {
            cfg: swc_ecma_codegen::Config {
                minify: false,
                ..Default::default()
            },
            cm: Lrc::new(FakeSourceMap),
            comments: None,
            wr: box JsWriter::new(Lrc::new(SourceMap::default()), "\n", &mut buf, None),
        };

        let mut body = vec![];
        body.push(ModuleItem::Stmt(Stmt::Expr(ExprStmt {
            span: DUMMY_SP,
            expr: box Expr::TsAs(TsAsExpr {
                span: DUMMY_SP,
                expr: box Expr::Ident(Ident::new("TYPE".into(), DUMMY_SP)),
                type_ann: box RTsType::from(ALLOW_DEEP_CLONE.set(&(), || t.clone().fold_with(&mut Visualizer::default()))).into_orig(),
            }),
        })));

        if let Type::Interface(t) = t.normalize() {
            ALLOW_DEEP_CLONE.set(&(), || {
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
                                tracker: Default::default(),
                            })
                            .fold_with(&mut Visualizer::default()),
                        )
                        .into_orig(),
                    }),
                })));
            })
        }

        body.visit_mut_with(&mut DropSpan { preserve_ctxt: true });

        emitter
            .emit_module(&Module {
                span: DUMMY_SP,
                body,
                shebang: None,
            })
            .unwrap();
    }
    let mut s = String::from_utf8_lossy(&buf).replace("TYPE as", "");

    if t.is_instance() {
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

pub fn print_type(name: &str, t: &Type) {
    let s = dump_type_as_string(t);
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

/// Prints stack trace of current function
///
/// Use this when if you think a wrong code is called, but **it should not be
/// called**.
pub fn print_backtrace() {
    // if cfg!(debug_assertions) {
    let s = dump_backtrace();

    println!("{}", s);
    // }
}

pub fn dump_backtrace() -> String {
    let bt = Backtrace::new();
    let bt = filter(bt);

    format!("{:?}", bt)
}

fn filter(mut bt: Backtrace) -> Backtrace {
    bt.resolve();
    let mut frames: Vec<_> = bt.into();

    frames.retain(|frame| {
        let symbols = frame.symbols();
        let len = symbols.len();
        for symbol in symbols {
            if symbol.name().and_then(|s| s.as_str()).is_none() {
                return false;
            }

            if let Some(filename) = symbol.filename() {
                let s = filename.to_string_lossy();

                if s.contains("backtrace")
                    || s.contains("libcore")
                    || s.contains("libstd")
                    || s.contains("/libtest/")
                    || s.contains("/rustc/")
                    || s.contains("/stc_visit/")
                    || s.contains("rust/library")
                    || s.contains("libpanic_unwind/")
                {
                    return false;
                }

                if len == 1 && (s.contains("scoped-tls") || s.contains("better_scoped_tls")) {
                    return false;
                }

                //                println!("({}) Filename: {}", len, s);
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

        ty.normalize_mut();

        self.done_types.push(ty.clone());

        ty = ty.fold_children_with(self);

        match ty {
            Type::Module(m) => Type::any(m.span, Default::default()),
            _ => ty,
        }
    }
}
