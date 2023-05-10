//! Module for parsing conformance test suite
#![allow(clippy::if_same_then_else)]
#![allow(clippy::manual_strip)]

use std::{
    env,
    io::{stderr, Write},
    mem::take,
    path::{Path, PathBuf},
    sync::Arc,
};

use anyhow::{anyhow, Result};
use once_cell::sync::Lazy;
use regex::Regex;
use rustc_hash::FxHashSet;
use stc_ts_builtin_types::Lib;
use stc_ts_env::{JsxMode, ModuleConfig, Rule};
use stc_ts_utils::StcComments;
use swc_common::{input::SourceFileInput, BytePos, Spanned};
use swc_ecma_ast::{EsVersion, Program};
use swc_ecma_parser::{Parser, Syntax, TsConfig};

pub struct TestSpec {
    /// Typescript conformance test remove lines starting with @-directives, and
    /// it changes the line of errors.
    pub err_shift_n: usize,
    pub libs: Vec<Lib>,
    pub rule: Rule,
    #[allow(unused)]
    pub ts_config: TsConfig,
    pub target: EsVersion,

    /// Empty for non-multi-file tests and includes `(` and `)` for multi-file
    /// tests.
    pub suffix: String,
    pub module_config: ModuleConfig,

    /// Empty for single file tests.
    pub sub_files: Arc<Vec<(String, String)>>,

    /// Library types defined by `@libFiles`
    pub lib_files: Vec<PathBuf>,
}

fn parse_sub_files(source: &str) -> Vec<(String, String)> {
    static OPTION_REGEX: Lazy<Regex> = Lazy::new(|| Regex::new(r#"^[\\/]{2}\s*@(\w+)\s*:\s*([^\r\n]*)"#).unwrap());
    let mut files = vec![];

    let mut buf = String::new();
    let mut sub_filename = String::new();

    for line in source.lines() {
        if let Some(cap) = OPTION_REGEX.captures(line) {
            let meta_data_name = cap.get(1).unwrap().as_str().to_lowercase();
            let meta_data_value = cap.get(2).unwrap().as_str();
            // https://github.com/microsoft/TypeScript/blob/71b2ba6111e934f2b4ee112bc4d8d2f47ced22f5/src/testRunner/compilerRunner.ts#L118-L148
            if let "filename" = &*meta_data_name {
                if !sub_filename.is_empty() {
                    buf = buf.trim().to_string();
                    files.push((sub_filename, take(&mut buf)));
                }

                sub_filename = meta_data_value.trim().to_string();
            }
        } else {
            buf += line;
            buf.push('\n');
        }
    }
    if !sub_filename.is_empty() {
        buf = buf.trim().to_string();
        files.push((sub_filename, buf));
    }

    files
}

#[allow(clippy::explicit_write)]
pub fn parse_conformance_test(file_name: &Path) -> Result<Vec<TestSpec>> {
    let mut err_shift_n = 0;
    let mut first_stmt_line = 0;

    let fname = file_name.to_string_lossy();
    ::testing::run_test(false, |cm, handler| {
        let _tracing = tracing::subscriber::set_default(tracing::subscriber::NoSubscriber::default());

        let fm = cm.load_file(file_name).expect("failed to read file");

        let sub_files = Arc::new(parse_sub_files(&fm.src));

        // We parse files twice. At first, we read comments and detect
        // configurations for following parse.

        let comments = StcComments::default();

        let mut parser = Parser::new(
            Syntax::Typescript(TsConfig {
                tsx: fname.contains("tsx"),
                ..Default::default()
            }),
            SourceFileInput::from(&*fm),
            Some(&comments),
        );
        let mut targets = vec![("".into(), EsVersion::default(), false)];

        let program = parser.parse_program().map_err(|e| {
            e.into_diagnostic(handler).emit();
        })?;

        for line in fm.src.lines() {
            if line.is_empty() {
                err_shift_n += 1;
            } else {
                break;
            }
        }

        match &program {
            Program::Module(v) => {
                if !v.body.is_empty() {
                    first_stmt_line = cm.lookup_line(v.body[0].span().lo).unwrap().line;
                }
            }
            Program::Script(v) => {
                if !v.body.is_empty() {
                    first_stmt_line = cm.lookup_line(v.body[0].span().lo).unwrap().line;
                }
            }
        }

        let mut jsx_options: Vec<(_, JsxMode)> = vec![];
        let mut libs = vec![Lib::Es5, Lib::Dom];
        let mut rule = Rule {
            allow_unreachable_code: false,
            ..Default::default()
        };
        let mut module_config = vec![("".into(), ModuleConfig::None)];
        let ts_config = TsConfig::default();
        let mut lib_files = vec![];

        let mut had_comment = false;

        let span = program.span();
        let comments = comments.leading.get(&span.lo());
        if let Some(ref comments) = comments {
            let directive_start = comments.iter().position(|cmt| cmt.text.trim().starts_with('@')).unwrap_or(0);
            let cmt_start_line = if directive_start == 0 {
                0
            } else {
                comments
                    .iter()
                    .find(|cmt| cmt.text.trim().starts_with('@'))
                    .map(|cmt| cm.lookup_char_pos(cmt.span.hi).line)
                    .unwrap_or(0)
            };

            for cmt in comments.iter().skip(directive_start) {
                let s = cmt.text.trim();
                if !s.starts_with('@') {
                    if had_comment {
                        err_shift_n = cm.lookup_char_pos(cmt.span.hi).line - 1 - cmt_start_line;
                        break;
                    }
                    continue;
                }
                had_comment = true;
                err_shift_n = cm.lookup_char_pos(cmt.span.hi + BytePos(1)).line - cmt_start_line;
                let s = &s[1..]; // '@'

                if s.starts_with("target:") || s.starts_with("Target:") {
                    let s = s["target:".len()..].trim().to_lowercase();
                    targets = parse_targets(&s).into_iter().map(|v| (v.0, v.1, true)).collect();
                } else if s.starts_with("strict:") {
                    let strict = s["strict:".len()..].trim().parse().unwrap();
                    rule.no_implicit_any = strict;
                    rule.no_implicit_this = strict;
                    rule.always_strict = strict;
                    rule.strict_null_checks = strict;
                    rule.strict_function_types = strict;
                } else if s.starts_with("noLib:") {
                    let v = s["noLib:".len()..].trim().parse().unwrap();
                    rule.no_lib = v;
                    if v {
                        libs = vec![];
                    }
                } else if s.to_lowercase().starts_with(&"noImplicitAny:".to_ascii_lowercase()) {
                    let v = s["noImplicitAny:".len()..].trim().parse().unwrap();
                    rule.no_implicit_any = v;
                } else if s.starts_with("noImplicitReturns:") {
                    let v = s["noImplicitReturns:".len()..].trim().parse().unwrap();
                    rule.no_implicit_returns = v;
                } else if s.starts_with("noUncheckedIndexedAccess:") {
                    let v = s["noUncheckedIndexedAccess:".len()..].trim().parse().unwrap();
                    rule.no_unchecked_indexed_access = v;
                } else if s.starts_with("declaration") {
                } else if s.starts_with("stripInternal:") {
                    // TODO(kdy1): Handle
                } else if s.starts_with("traceResolution") {
                    // no-op
                } else if s.starts_with("allowUnusedLabels:") {
                    let v = s["allowUnusedLabels:".len()..].trim().parse().unwrap();
                    rule.allow_unused_labels = v;
                } else if s.starts_with("noEmitHelpers") {
                    // TODO
                } else if s.starts_with("downlevelIteration:") {
                    // TODO
                } else if s.starts_with("sourceMap:") || s.starts_with("sourcemap:") {
                    // TODO
                } else if s.starts_with("isolatedModules:") {
                    // TODO
                } else if s.starts_with("lib:") {
                    let s = s["lib:".len()..].trim();
                    let mut ls = FxHashSet::<_>::default();
                    for v in s.split(',') {
                        ls.extend(Lib::load(&v.to_lowercase().replace("es6", "es2015")))
                    }
                    libs = ls.into_iter().collect()
                } else if s.starts_with("libFiles:") {
                    lib_files = s["libFiles:".len()..]
                        .trim()
                        .split(',')
                        .map(|s| env::current_dir().unwrap().join("tests").join("libs").join(s))
                        .collect();
                    lib_files.sort();
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
                } else if s.starts_with("suppressImplicitAnyIndexErrors:") {
                    // TODO
                    let v = s["suppressImplicitAnyIndexErrors:".len()..].trim().parse().unwrap();
                    rule.suppress_implicit_any_index_errors = v;
                } else if s.starts_with("module:") {
                    module_config = parse_directive_values(&s["module:".len()..], &|s| s.parse().unwrap());
                } else if s.to_lowercase().starts_with("notypesandsymbols") {
                    // Ignored as we don't generate them.
                } else if s.to_lowercase().starts_with("usedefineforclassfields") {
                    rule.use_define_property_for_class_fields = true;
                } else if s.to_lowercase().starts_with("jsx") {
                    jsx_options = parse_directive_values(&s["jsx:".len()..], &|s| s.to_ascii_lowercase().parse().unwrap());
                } else if s.to_lowercase().starts_with("noemit") || s.to_lowercase().starts_with("preserveconstenums") {
                    // Ignored as we only checks type.
                } else if s.starts_with("strict") {
                    let strict = true;
                    rule.no_implicit_any = strict;
                    rule.no_implicit_this = strict;
                    rule.always_strict = strict;
                    rule.strict_null_checks = strict;
                    rule.strict_function_types = strict;
                } else if s.to_ascii_lowercase().starts_with("filename") {
                } else if s.to_ascii_lowercase().starts_with("allowjs") || s.to_ascii_lowercase().starts_with("checkjs") {
                    // panic!("allowJs and checkJs are not supported yet. See https://github.com/dudykr/stc/issues/702")
                    return Err(());
                } else {
                    writeln!(stderr(), "Comment is not handled: {}", s).unwrap();
                }
            }
        }

        libs.sort();

        err_shift_n = err_shift_n.min(first_stmt_line);

        if !sub_files.is_empty() {
            err_shift_n = 0;
        }

        dbg!(err_shift_n);

        if targets.len() > 1 {
            return Ok(targets
                .into_iter()
                .map(|(raw, target, specified)| {
                    let libs = build_target(target, specified, &libs);

                    TestSpec {
                        err_shift_n,
                        libs,
                        rule,
                        ts_config,
                        target,
                        suffix: format!("(target={})", raw),
                        module_config: module_config[0].1,
                        sub_files: sub_files.clone(),
                        lib_files: lib_files.clone(),
                    }
                })
                .collect());
        }

        let target = targets[0].1;
        let specified = targets[0].2;

        let libs = build_target(target, specified, &libs);
        if module_config.len() > 1 {
            return Ok(module_config
                .into_iter()
                .map(|(raw, module_config)| TestSpec {
                    err_shift_n,
                    libs: libs.clone(),
                    rule,
                    ts_config,
                    target,
                    suffix: format!("(module={})", raw),
                    module_config,
                    sub_files: sub_files.clone(),
                    lib_files: lib_files.clone(),
                })
                .collect());
        }

        let module_config = module_config[0].1;

        if jsx_options.len() > 1 {
            return Ok(jsx_options
                .into_iter()
                .map(|(raw, jsx)| TestSpec {
                    err_shift_n,
                    libs: libs.clone(),
                    rule: Rule { jsx, ..rule },
                    ts_config,
                    target,
                    suffix: format!("(jsx={})", raw),
                    module_config,
                    sub_files: sub_files.clone(),
                    lib_files: lib_files.clone(),
                })
                .collect());
        }

        Ok(vec![TestSpec {
            err_shift_n,
            libs,
            rule,
            ts_config,
            target,
            suffix: Default::default(),
            module_config,
            sub_files,
            lib_files,
        }])
    })
    .map_err(|err| anyhow!("Failed to parse test case: {}", err))
}

fn parse_directive_values<T>(s: &str, parser: &dyn Fn(&str) -> T) -> Vec<(String, T)> {
    s.trim().split(',').map(|s| s.trim()).map(|s| (s.into(), parser(s))).collect()
}

fn parse_targets(s: &str) -> Vec<(String, EsVersion)> {
    fn parse_target_inner(s: &str) -> Vec<EsVersion> {
        match s {
            "es3" => return vec![EsVersion::Es3],
            "es5" => return vec![EsVersion::Es5],
            "es2015" => return vec![EsVersion::Es2015],
            "es6" => return vec![EsVersion::Es2015],
            "es2016" => return vec![EsVersion::Es2016],
            "es2017" => return vec![EsVersion::Es2017],
            "es2018" => return vec![EsVersion::Es2018],
            "es2019" => return vec![EsVersion::Es2019],
            "es2020" => return vec![EsVersion::Es2020],
            "es2021" => return vec![EsVersion::Es2021],
            "es2022" => return vec![EsVersion::Es2022],
            // TODO(upstream): enable es2023
            // "es2023" => return vec![EsVersion::Es2023],
            "esnext" => return vec![EsVersion::EsNext],
            _ => {}
        }
        if !s.contains(',') {
            panic!("failed to parse `{}` as targets", s)
        }
        s.split(',').map(|s| s.trim()).flat_map(parse_target_inner).collect()
    }

    if !s.contains(',') {
        return vec![(s.into(), parse_target_inner(s)[0])];
    }
    s.split(',').map(|s| s.trim()).flat_map(parse_targets).collect()
}

fn build_target(target: EsVersion, specified: bool, libs: &[Lib]) -> Vec<Lib> {
    if specified && libs == vec![Lib::Es5, Lib::Dom] {
        match target {
            EsVersion::Es3 | EsVersion::Es5 => vec![Lib::Es5, Lib::Dom],
            EsVersion::Es2015 => Lib::load("es2015.full"),
            EsVersion::Es2016 => Lib::load("es2016.full"),
            EsVersion::Es2017 => Lib::load("es2017.full"),
            EsVersion::Es2018 => Lib::load("es2018.full"),
            EsVersion::Es2019 => Lib::load("es2019.full"),
            EsVersion::Es2021 => Lib::load("es2021.full"),
            EsVersion::Es2022 => Lib::load("es2022.full"),
            // TODO(upstream): enable es2023
            // EsVersion::Es2023 => Lib::load("es2023.full"),
            _ => Lib::load("es2022.full"),
        }
    } else if specified {
        libs_with_deps(libs)
    } else {
        libs.to_vec()
    }
}

fn libs_with_deps(libs: &[Lib]) -> Vec<Lib> {
    fn add(libs: &mut Vec<Lib>, l: Lib) {
        if libs.contains(&l) {
            return;
        }
        libs.push(l);

        match l {
            Lib::Es5 | Lib::Es5Full => {}

            Lib::Es2015Collection
            | Lib::Es2015Core
            | Lib::Es2015
            | Lib::Es2015Full
            | Lib::Es2015Generator
            | Lib::Es2015Iterable
            | Lib::Es2015Promise
            | Lib::Es2015Proxy
            | Lib::Es2015Reflect
            | Lib::Es2015Symbol
            | Lib::Es2015SymbolWellknown => add(libs, Lib::Es2015Full),

            Lib::Es2016ArrayInclude | Lib::Es2016 | Lib::Es2016Full => add(libs, Lib::Es2016Full),
            Lib::Es2017
            | Lib::Es2017Full
            | Lib::Es2017Intl
            | Lib::Es2017Object
            | Lib::Es2017Sharedmemory
            | Lib::Es2017String
            | Lib::Es2017Typedarrays => add(libs, Lib::Es2017Full),

            Lib::Es2018Asyncgenerator
            | Lib::Es2018Asynciterable
            | Lib::Es2018
            | Lib::Es2018Full
            | Lib::Es2018Intl
            | Lib::Es2018Promise
            | Lib::Es2018Regexp => add(libs, Lib::Es2018Full),

            Lib::Es2019Array
            | Lib::Es2019
            | Lib::Es2019Full
            | Lib::Es2019Object
            | Lib::Es2019String
            | Lib::Es2019Symbol
            | Lib::Es2019Intl => add(libs, Lib::Es2019Full),

            Lib::Es2020Bigint
            | Lib::Es2020
            | Lib::Es2020Full
            | Lib::Es2020Intl
            | Lib::Es2020Promise
            | Lib::Es2020Sharedmemory
            | Lib::Es2020String
            | Lib::Es2020SymbolWellknown
            | Lib::Es2020Date
            | Lib::Es2020Number => add(libs, Lib::Es2020Full),

            Lib::Es2021 | Lib::Es2021Full | Lib::Es2021Weakref | Lib::Es2021Intl | Lib::Es2021Promise | Lib::Es2021String => {
                add(libs, Lib::Es2021Full)
            }

            Lib::Es2022
            | Lib::Es2022Array
            | Lib::Es2022Error
            | Lib::Es2022Object
            | Lib::Es2022Full
            | Lib::Es2022Intl
            | Lib::Es2022Sharedmemory
            | Lib::Es2022String
            | Lib::Es2022Regexp => add(libs, Lib::Es2022Full),

            Lib::Es2023 | Lib::Es2023Array | Lib::Es2023Full => add(libs, Lib::Es2023Full),

            Lib::Esnext | Lib::EsnextFull | Lib::EsnextIntl | Lib::EsnextPromise | Lib::EsnextString | Lib::EsnextWeakref => {
                add(libs, Lib::Es2022Full)
            }

            Lib::Dom
            | Lib::DomIterable
            | Lib::DomIterableGenerated
            | Lib::Header
            | Lib::Scripthost
            | Lib::WebworkerGenerated
            | Lib::WebworkerImportscripts
            | Lib::WebworkerIterableGenerated
            | Lib::Decorators
            | Lib::DecoratorsLegacy => {}
        }

        for l in l.deps() {
            add(libs, l);
        }
    }

    let mut all = vec![];

    for lib in libs {
        add(&mut all, *lib);
    }

    all.sort();
    all.dedup();

    all
}
