#![recursion_limit = "256"]
#![feature(vec_remove_item)]
#![feature(box_syntax)]
#![feature(box_patterns)]
#![feature(test)]

extern crate test;

#[path = "common/mod.rs"]
mod common;

use self::common::load_fixtures;
use self::common::SwcComments;
use once_cell::sync::Lazy;
use stc_testing::logger;
use stc_ts_builtin_types::Lib;
use stc_ts_errors::debug::debugger::Debugger;
use stc_ts_file_analyzer::env::Env;
use stc_ts_file_analyzer::Rule;
use stc_ts_testing::tsc::TsTestCase;
use stc_ts_type_checker::Checker;
use std::collections::HashSet;
use std::env;
use std::fs::read_to_string;
use std::io;
use std::io::Write;
use std::path::Path;
use std::path::PathBuf;
use std::sync::Arc;
use std::sync::RwLock;
use swc_common::errors::ColorConfig;
use swc_common::errors::EmitterWriter;
use swc_common::errors::Handler;
use swc_common::errors::HandlerFlags;
use swc_common::input::SourceFileInput;
use swc_common::FileName;
use swc_common::SourceMap;
use swc_common::Span;
use swc_common::Spanned;
use swc_ecma_ast::*;
use swc_ecma_parser::JscTarget;
use swc_ecma_parser::Parser;
use swc_ecma_parser::Syntax;
use swc_ecma_parser::TsConfig;
use swc_ecma_visit::Fold;
use swc_ecma_visit::FoldWith;
use test::test_main;
use testing::run_test2;
use testing::NormalizedOutput;
use testing::StdErr;
use testing::Tester;

fn is_ignored(path: &Path) -> bool {
    static IGNORED: Lazy<Vec<String>> = Lazy::new(|| {
        let content = read_to_string("tests/types.ignored.txt").unwrap();

        content
            .lines()
            .filter(|v| !v.trim().is_empty())
            .map(|v| v.to_string())
            .collect()
    });

    static PASS: Lazy<Vec<String>> = Lazy::new(|| {
        let content = read_to_string("tests/types.pass.txt").unwrap();

        content
            .lines()
            .filter(|v| !v.trim().is_empty())
            .map(|v| v.to_string())
            .collect()
    });

    !PASS
        .iter()
        .any(|line| path.to_string_lossy().contains(line))
        || IGNORED
            .iter()
            .any(|line| path.to_string_lossy().contains(line))
}

#[test]
fn types() {
    let args: Vec<_> = env::args().collect();
    let tests = load_fixtures("conformance", |file_name| {
        if is_ignored(&file_name) {
            return None;
        }

        Some(box move || {
            do_test(&file_name).unwrap();
        })
    });
    test_main(&args, tests, Default::default());
}

fn do_test(path: &Path) -> Result<(), StdErr> {
    let str_name = path.display().to_string();

    let (libs, rule, ts_config, target) = ::testing::run_test(false, |cm, handler| {
        let fm = cm.load_file(path).expect("failed to read file");

        Ok({
            // We parse files twice. At first, we read comments and detect
            // configurations for following parse.

            let comments = SwcComments::default();

            let mut parser = Parser::new(
                Syntax::Typescript(TsConfig {
                    tsx: str_name.contains("tsx"),
                    ..Default::default()
                }),
                SourceFileInput::from(&*fm),
                Some(&comments),
            );
            let mut target = JscTarget::default();

            let module = parser.parse_module().map_err(|e| {
                e.into_diagnostic(&handler).emit();
                ()
            })?;
            let module = make_test(&comments, module);

            let mut libs = vec![Lib::Es5];
            let mut rule = Rule::default();
            let ts_config = TsConfig::default();

            let span = module.span;
            let cmts = comments.leading.get(&span.lo());
            match cmts {
                Some(ref cmts) => {
                    for cmt in cmts.iter() {
                        let s = cmt.text.trim();
                        if !s.starts_with("@") {
                            continue;
                        }
                        let s = &s[1..]; // '@'

                        if s.starts_with("target:") || s.starts_with("Target:") {
                            let s = s["target:".len()..].trim().to_lowercase();
                            target = match &*s {
                                "es3" => JscTarget::Es3,
                                "es5" => JscTarget::Es5,
                                "es2015" => JscTarget::Es2015,
                                "es6" => JscTarget::Es2015,
                                "es2016" => JscTarget::Es2016,
                                "es2017" => JscTarget::Es2017,
                                "es2018" => JscTarget::Es2018,
                                "es2019" => JscTarget::Es2019,
                                "esnext" => JscTarget::Es2019,
                                _ => unimplemented!("target: {:?}", s),
                            };
                            libs = match target {
                                JscTarget::Es3 | JscTarget::Es5 => vec![Lib::Es5],
                                JscTarget::Es2015 => Lib::load("es2015"),
                                JscTarget::Es2016 => Lib::load("es2016"),
                                JscTarget::Es2017 => Lib::load("es2017"),
                                JscTarget::Es2018 => Lib::load("es2018"),
                                JscTarget::Es2019 => Lib::load("es2019"),
                                JscTarget::Es2020 => Lib::load("es2020"),
                            };
                        } else if s.starts_with("strict:") {
                            let strict = s["strict:".len()..].trim().parse().unwrap();
                            rule.no_implicit_any = strict;
                            rule.no_implicit_this = strict;
                            rule.always_strict = strict;
                            rule.strict_null_checks = strict;
                            rule.strict_function_types = strict;
                        } else if s.starts_with("noLib:") {
                            let v = s["noLib:".len()..].trim().parse().unwrap();
                            if v {
                                libs = vec![];
                            }
                        } else if s.starts_with("noImplicitAny:") {
                            let v = s["noImplicitAny:".len()..].trim().parse().unwrap();
                            rule.no_implicit_any = v;
                        } else if s.starts_with("noImplicitReturns:") {
                            let v = s["noImplicitReturns:".len()..].trim().parse().unwrap();
                            rule.no_implicit_returns = v;
                        } else if s.starts_with("declaration") {
                        } else if s.starts_with("stripInternal:") {
                            // TODO: Handle
                        } else if s.starts_with("traceResolution") {
                            // no-op
                        } else if s.starts_with("allowUnusedLabels:") {
                            let v = s["allowUnusedLabels:".len()..].trim().parse().unwrap();
                            rule.allow_unused_labels = v;
                        } else if s.starts_with("noEmitHelpers") {
                            // TODO
                        } else if s.starts_with("downlevelIteration: ") {
                            // TODO
                        } else if s.starts_with("sourceMap:") || s.starts_with("sourcemap:") {
                            // TODO
                        } else if s.starts_with("isolatedModules:") {
                            // TODO
                        } else if s.starts_with("lib:") {
                            let mut ls = HashSet::<_>::default();
                            for v in s["lib:".len()..].trim().split(",") {
                                ls.extend(Lib::load(v))
                            }
                            libs = ls.into_iter().collect()
                        } else if s.starts_with("allowUnreachableCode:") {
                            let v = s["allowUnreachableCode:".len()..].trim().parse().unwrap();
                            rule.allow_unreachable_code = v;
                        } else if s.starts_with("strictNullChecks:") {
                            let v = s["strictNullChecks:".len()..].trim().parse().unwrap();
                            rule.strict_null_checks = v;
                        } else if s.starts_with("noImplicitThis:") {
                            let v = s["noImplicitThis:".len()..].trim().parse().unwrap();
                            rule.no_implicit_this = v;
                        } else if s.starts_with("skipDefaultLibCheck") {
                            // TODO
                        } else {
                            panic!("Comment is not handled: {}", s);
                        }
                    }
                }
                None => {}
            }

            (libs, rule, ts_config, target)
        })
    })
    .ok()
    .unwrap_or_default();

    let tester = Tester::new();

    let visualized = tester
        .print_errors(|cm, type_info_handler| -> Result<(), _> {
            let (handler_for_errors, error_text) = new_handler(cm.clone());
            let handler_for_errors = Arc::new(handler_for_errors);

            let log = logger();
            let type_info_handler = Arc::new(type_info_handler);
            let mut checker = Checker::new(
                log.logger,
                cm.clone(),
                handler_for_errors.clone(),
                Env::simple(rule, target, &libs),
                TsConfig {
                    tsx: str_name.contains("tsx"),
                    ..ts_config
                },
                Some(Debugger {
                    cm: cm.clone(),
                    handler: type_info_handler.clone(),
                }),
            );

            checker.check(Arc::new(path.into()));

            let errors = ::stc_ts_errors::Error::flatten(checker.take_errors());

            checker.run(|| {
                for e in errors {
                    e.emit(&handler_for_errors);
                }
            });

            eprintln!("{}", NormalizedOutput::from(error_text));

            Err(())
        })
        .expect_err("should fail");

    let spec = run_test2(false, |cm, _| {
        let handler = Arc::new(Handler::with_tty_emitter(
            ColorConfig::Always,
            true,
            false,
            Some(cm.clone()),
        ));

        Ok(TsTestCase::parse(
            &cm,
            &handler,
            &PathBuf::from("tests")
                .join("reference")
                .join(path.with_extension("types").file_name().unwrap())
                .canonicalize()
                .unwrap(),
            None,
        )
        .unwrap())
    })
    .unwrap();

    // TODO: Match on type data.

    if spec.type_data.is_empty() {
        return Ok(());
    }

    visualized
        .compare_to_file(path.with_extension("stdout"))
        .unwrap();

    Ok(())
}

fn make_test(c: &SwcComments, module: Module) -> Module {
    let mut m = TestMaker {
        c,
        stmts: Default::default(),
    };

    module.fold_with(&mut m)
}

struct TestMaker<'a> {
    c: &'a SwcComments,
    stmts: Vec<Stmt>,
}

impl Fold for TestMaker<'_> {
    fn fold_module_items(&mut self, stmts: Vec<ModuleItem>) -> Vec<ModuleItem> {
        let mut ss = vec![];
        for stmt in stmts {
            let stmt = stmt.fold_with(self);
            ss.push(stmt);
            ss.extend(self.stmts.drain(..).map(ModuleItem::Stmt));
        }

        ss
    }

    fn fold_stmts(&mut self, stmts: Vec<Stmt>) -> Vec<Stmt> {
        let mut ss = vec![];
        for stmt in stmts {
            let stmt = stmt.fold_with(self);
            ss.push(stmt);
            ss.extend(self.stmts.drain(..));
        }

        ss
    }

    fn fold_ts_type_alias_decl(&mut self, decl: TsTypeAliasDecl) -> TsTypeAliasDecl {
        let cmts = self.c.trailing.get(&decl.span.hi());

        match cmts {
            Some(cmts) => {
                assert!(cmts.len() == 1);
                let cmt = cmts.iter().next().unwrap();
                let t = cmt.text.trim().replace("\n", "").replace("\r", "");

                let cmt_type = match parse_type(cmt.span, &t) {
                    Some(ty) => ty,
                    None => return decl,
                };

                //  {
                //      let _value: ty = (Object as any as Alias)
                //  }
                //
                //
                let span = decl.span();
                self.stmts.push(Stmt::Block(BlockStmt {
                    span,
                    stmts: vec![Stmt::Decl(Decl::Var(VarDecl {
                        span,
                        decls: vec![VarDeclarator {
                            span,
                            name: Pat::Ident(Ident {
                                span,
                                sym: "_value".into(),
                                type_ann: Some(TsTypeAnn {
                                    span,
                                    type_ann: box cmt_type,
                                }),
                                optional: false,
                            }),
                            init: Some(box Expr::TsAs(TsAsExpr {
                                span,
                                expr: box Expr::TsAs(TsAsExpr {
                                    span,
                                    expr: box Expr::Ident(Ident::new("Object".into(), span)),
                                    type_ann: box TsType::TsKeywordType(TsKeywordType {
                                        span,
                                        kind: TsKeywordTypeKind::TsAnyKeyword,
                                    }),
                                }),
                                type_ann: box TsType::TsTypeRef(TsTypeRef {
                                    span,
                                    type_name: TsEntityName::Ident(decl.id.clone()),
                                    type_params: None,
                                }),
                            })),
                            definite: false,
                        }],
                        kind: VarDeclKind::Const,
                        declare: false,
                    }))],
                }));
            }
            None => {}
        }

        decl
    }
}

fn parse_type(span: Span, s: &str) -> Option<TsType> {
    let s = s.trim();

    if s.starts_with("error") || s.starts_with("Error") {
        return None;
    }

    let ty = ::testing::run_test(true, |cm, _handler| {
        let fm = cm.new_source_file(FileName::Anon, s.into());

        let mut parser = Parser::new(
            Syntax::Typescript(Default::default()),
            SourceFileInput::from(&*fm),
            None,
        );
        let ty = match parser.parse_type() {
            Ok(v) => v,
            Err(..) => return Err(()),
        };
        Ok(ty)
    });

    let mut spanner = Spanner { span };

    Some(*ty.ok()?.fold_with(&mut spanner))
}

struct Spanner {
    span: Span,
}

impl Fold for Spanner {
    fn fold_span(&mut self, _: Span) -> Span {
        self.span
    }
}

/// Creates a new handler for testing.
fn new_handler(cm: Arc<SourceMap>) -> (Handler, BufferedError) {
    let buf: BufferedError = Default::default();

    let e = EmitterWriter::new(Box::new(buf.clone()), Some(cm.clone()), false, true);

    let handler = Handler::with_emitter_and_flags(
        Box::new(e),
        HandlerFlags {
            treat_err_as_bug: false,
            can_emit_warnings: true,
            ..Default::default()
        },
    );

    (handler, buf)
}

#[derive(Clone, Default)]
struct BufferedError(Arc<RwLock<Vec<u8>>>);

impl Write for BufferedError {
    fn write(&mut self, d: &[u8]) -> io::Result<usize> {
        self.0.write().unwrap().write(d)
    }
    fn flush(&mut self) -> io::Result<()> {
        Ok(())
    }
}

impl From<BufferedError> for NormalizedOutput {
    fn from(buf: BufferedError) -> Self {
        let s = buf.0.read().unwrap();
        let s: String = String::from_utf8_lossy(&s).into();

        s.into()
    }
}
