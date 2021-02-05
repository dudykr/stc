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
use anyhow::Context;
use anyhow::Error;
use once_cell::sync::Lazy;
use serde::Deserialize;
use stc_testing::logger;
use stc_ts_builtin_types::Lib;
use stc_ts_file_analyzer::env::Env;
use stc_ts_file_analyzer::Rule;
use stc_ts_type_checker::Checker;
use std::collections::HashSet;
use std::env;
use std::fs::read_to_string;
use std::fs::File;
use std::panic::catch_unwind;
use std::path::Path;
use std::sync::Arc;
use swc_common::errors::DiagnosticBuilder;
use swc_common::errors::DiagnosticId;
use swc_common::input::SourceFileInput;
use swc_common::BytePos;
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
use testing::StdErr;
use testing::Tester;

#[derive(Debug, Clone, PartialEq, Eq, Deserialize)]
struct RefError {
    pub line: usize,
    pub column: usize,
    pub code: String,
}

fn is_ignored(path: &Path) -> bool {
    static IGNORED: Lazy<Vec<String>> = Lazy::new(|| {
        let content = read_to_string("tests/conformance.ignored.txt").unwrap();

        content
            .lines()
            .filter(|v| !v.trim().is_empty())
            .map(|v| v.to_string())
            .collect()
    });

    static PASS: Lazy<Vec<String>> = Lazy::new(|| {
        let content = read_to_string("tests/conformance.pass.txt").unwrap();

        content
            .lines()
            .filter(|v| !v.trim().is_empty())
            .map(|v| v.to_string())
            .collect()
    });

    if IGNORED.iter().any(|line| path.to_string_lossy().contains(line)) {
        return true;
    }

    if let Ok(test) = env::var("TEST") {
        return !path.to_string_lossy().contains(&test);
    }

    !PASS.iter().any(|line| path.to_string_lossy().contains(line))
}

#[test]
fn conformance() {
    let args: Vec<_> = env::args().collect();
    let tests = load_fixtures("conformance", |path| {
        if is_ignored(&path) {
            return None;
        }

        let str_name = path.display().to_string();

        // If parser returns error, ignore it for now.

        let cm = SourceMap::default();
        let fm = cm.load_file(&path).unwrap();

        // Postpone multi-file tests.
        if fm.src.to_lowercase().contains("@filename") {
            return None;
        }

        catch_unwind(|| {
            let mut parser = Parser::new(
                Syntax::Typescript(TsConfig {
                    tsx: str_name.contains("tsx"),
                    ..Default::default()
                }),
                SourceFileInput::from(&*fm),
                None,
            );
            parser.parse_module().ok()
        })
        .ok()??;

        if let Ok(errors) = load_expected_errors(&path) {
            for err in errors {
                if err.code.starts_with("TS1") && err.code.len() == 6 {
                    return None;
                }
            }
        }

        Some(box move || {
            do_test(false, &path).unwrap();
        })
    });
    test_main(&args, tests, Default::default());
}

fn load_expected_errors(ts_file: &Path) -> Result<Vec<RefError>, Error> {
    let errors_file = ts_file.with_extension("errors.json");
    if !errors_file.exists() {
        println!("errors file does not exists: {}", errors_file.display());
        Ok(vec![])
    } else {
        let errors: Vec<RefError> =
            serde_json::from_reader(File::open(errors_file).expect("failed to open error sfile"))
                .context("failed to parse errors.txt.json")?;

        // TODO: Match column and message

        Ok(errors)
    }
}

fn do_test(treat_error_as_bug: bool, file_name: &Path) -> Result<(), StdErr> {
    let fname = file_name.display().to_string();
    let mut expected_errors = load_expected_errors(&file_name).unwrap();
    let mut err_shift_n = 0;
    let mut first_stmt_line = 0;

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

            for line in fm.src.lines() {
                if line.is_empty() {
                    err_shift_n += 1;
                } else {
                    break;
                }
            }

            if !module.body.is_empty() {
                first_stmt_line = cm.lookup_line(module.body[0].span().lo).unwrap().line;
            }

            let mut libs = vec![Lib::Es5];
            let mut rule = Rule::default();
            let ts_config = TsConfig::default();

            let mut had_comment = false;

            let span = module.span;
            let cmts = comments.leading.get(&span.lo());
            match cmts {
                Some(ref cmts) => {
                    for cmt in cmts.iter() {
                        let s = cmt.text.trim();
                        if !s.starts_with("@") {
                            if had_comment {
                                err_shift_n = cm.lookup_char_pos(cmt.span.hi).line - 1;
                                break;
                            }
                            continue;
                        }
                        had_comment = true;
                        err_shift_n = cm.lookup_char_pos(cmt.span.hi + BytePos(1)).line;
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
                        } else if s.starts_with("module") {
                        } else {
                            panic!("Comment is not handled: {}", s);
                        }
                    }
                }
                None => {}
            }

            libs.sort();
            dbg!(&libs);
            (libs, rule, ts_config, target)
        })
    })
    .ok()
    .unwrap_or_default();

    err_shift_n = err_shift_n.min(first_stmt_line);

    dbg!(err_shift_n);

    for err in &mut expected_errors {
        // Typescript conformance test remove lines starting with @-directives.
        err.line += err_shift_n;
    }

    let full_ref_errors = expected_errors.clone();
    let full_ref_err_cnt = full_ref_errors.len();

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

    let mut actual_errors = diagnostics
        .iter()
        .map(|d| {
            let span = d.span.primary_span().unwrap();
            let cp = tester.cm.lookup_char_pos(span.lo());
            let code = d
                .code
                .clone()
                .expect("conformance teting: All errors should have proper error code");
            let code = match code {
                DiagnosticId::Error(err) => err,
                DiagnosticId::Lint(lint) => {
                    unreachable!("Unexpected lint '{}' found", lint)
                }
            };

            (cp.line, code)
        })
        .collect::<Vec<_>>();

    let full_actual_errors = actual_errors.clone();

    for (line, error_code) in full_actual_errors.clone() {
        if let Some(idx) = expected_errors
            .iter()
            .position(|err| err.line == line && err.code == error_code)
        {
            expected_errors.remove(idx);
            if let Some(idx) = actual_errors
                .iter()
                .position(|(r_line, r_code)| line == *r_line && error_code == *r_code)
            {
                actual_errors.remove(idx);
            }
        }
    }

    //
    //      - All reference errors are matched
    //      - Actual errors does not remain
    let success = expected_errors.is_empty() && actual_errors.is_empty();

    let res: Result<(), _> = tester.print_errors(|_, handler| {
        // If we failed, we only emit errors which has wrong line.

        for (d, line_col) in diagnostics.into_iter().zip(full_actual_errors.clone()) {
            if success || env::var("PRINT_ALL").unwrap_or(String::from("")) == "1" || actual_errors.contains(&line_col)
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

    let err_count = actual_errors.len();

    if !success {
        panic!(
            "\n============================================================\n{:?}
============================================================\n{} unmatched errors out of {} errors. Got {} extra \
             errors.\nWanted: {:?}\nUnwanted: {:?}\n\nAll required errors: {:?}\nAll actual errors: {:?}",
            err,
            expected_errors.len(),
            full_ref_err_cnt,
            err_count,
            expected_errors,
            actual_errors,
            full_ref_errors,
            full_actual_errors,
        );
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
