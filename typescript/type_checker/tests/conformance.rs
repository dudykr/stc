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
use serde::Deserialize;
use stc_testing::logger;
use stc_ts_builtin_types::Lib;
use stc_ts_file_analyzer::env::Env;
use stc_ts_file_analyzer::Rule;
use stc_ts_type_checker::Checker;
use std::collections::HashSet;
use std::env;
use std::fs::File;
use std::path::Path;
use std::sync::Arc;
use swc_common::errors::DiagnosticBuilder;
use swc_common::input::SourceFileInput;
use swc_common::FileName;
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
use testing::StdErr;
use testing::Tester;

#[derive(Debug, Clone, PartialEq, Eq, Deserialize)]
struct Error {
    pub line: usize,
    pub column: usize,
    pub msg: String,
}

#[test]
#[ignore = "Not implemented yet"]
fn conformance() {
    let args: Vec<_> = env::args().collect();
    let tests = load_fixtures("conformance", |file_name| {
        Some(box move || {
            do_test(false, &file_name).unwrap();
        })
    });
    test_main(&args, tests, Default::default());
}

fn do_test(treat_error_as_bug: bool, file_name: &Path) -> Result<(), StdErr> {
    let fname = file_name.display().to_string();
    let mut ref_errors = {
        let fname = file_name.file_name().unwrap();
        let errors_file =
            file_name.with_file_name(format!("{}.errors.json", fname.to_string_lossy()));
        if !errors_file.exists() {
            println!("errors file does not exists: {}", errors_file.display());
            Some(vec![])
        } else {
            let errors: Vec<Error> = serde_json::from_reader(
                File::open(errors_file).expect("failed to open error sfile"),
            )
            .expect("failed to parse errors.txt.json");

            // TODO: Match column and message

            Some(
                errors
                    .into_iter()
                    .map(|e| (e.line, e.column))
                    .collect::<Vec<_>>(),
            )
        }
    };
    let full_ref_errors = ref_errors.clone();
    let full_ref_err_cnt = full_ref_errors.as_ref().map(Vec::len).unwrap_or(0);

    let (libs, rule, ts_config, target) = ::testing::run_test(treat_error_as_bug, |cm, handler| {
        let fm = cm.load_file(file_name).expect("failed to read file");

        Ok({
            // We parse files twice. At first, we read comments and detect
            // configurations for following parse.

            let comments = SwcComments::default();

            let mut parser = Parser::new(
                Syntax::Typescript(TsConfig {
                    tsx: fname.contains("tsx"),
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

    let ref_errors = if let Some(ref mut ref_errors) = ref_errors {
        ref_errors
    } else {
        unreachable!()
    };

    let tester = Tester::new();
    let diagnostics = tester
        .errors(|cm, handler| {
            let log = logger();
            let handler = Arc::new(handler);
            let mut checker = Checker::new(
                log.logger,
                cm.clone(),
                handler.clone(),
                Env::simple(rule, target, &libs),
                TsConfig {
                    tsx: fname.contains("tsx"),
                    ..ts_config
                },
                None,
            );

            checker.check(Arc::new(file_name.into()));

            let errors = ::stc_ts_errors::Error::flatten(checker.take_errors());

            checker.run(|| {
                for e in errors {
                    e.emit(&handler);
                }
            });

            if false {
                return Ok(());
            }

            return Err(());
        })
        .expect_err("");

    let mut actual_error_lines = diagnostics
        .iter()
        .map(|d| {
            let span = d.span.primary_span().unwrap();
            let cp = tester.cm.lookup_char_pos(span.lo());

            (cp.line, cp.col.0 + 1)
        })
        .collect::<Vec<_>>();

    let full_actual_error_lc = actual_error_lines.clone();

    for line_col in full_actual_error_lc.clone() {
        if let Some(..) = ref_errors.remove_item(&line_col) {
            actual_error_lines.remove_item(&line_col);
        }
    }

    //
    //      - All reference errors are matched
    //      - Actual errors does not remain
    let success = ref_errors.is_empty() && actual_error_lines.is_empty();

    let res: Result<(), _> = tester.print_errors(|_, handler| {
        // If we failed, we only emit errors which has wrong line.

        for (d, line_col) in diagnostics.into_iter().zip(full_actual_error_lc.clone()) {
            if success
                || env::var("PRINT_ALL").unwrap_or(String::from("")) == "1"
                || actual_error_lines.contains(&line_col)
            {
                DiagnosticBuilder::new_diagnostic(&handler, d).emit();
            }
        }

        Err(())
    });

    let err = match res {
        Ok(_) => StdErr::from(String::from("")),
        Err(err) => err,
    };

    let err_count = actual_error_lines.len();

    if !success {
        panic!(
            "\n============================================================\n{:?}
============================================================\n{} unmatched errors out of {} \
             errors. Got {} extra errors.\nWanted: {:?}\nUnwanted: {:?}\n\nAll required errors: \
             {:?}\nAll actual errors: {:?}",
            err,
            ref_errors.len(),
            full_ref_err_cnt,
            err_count,
            ref_errors,
            actual_error_lines,
            full_ref_errors.as_ref().unwrap(),
            full_actual_error_lc,
        );
    }

    if err
        .compare_to_file(format!("{}.stderr", file_name.display()))
        .is_err()
    {
        panic!()
    }

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
