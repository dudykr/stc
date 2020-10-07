#![feature(box_syntax)]
#![feature(test)]

extern crate test;

use slog::Logger;
use stc_checker::{env::Env, Checker, Lib};
use std::{
    fs::read_to_string,
    hint::black_box,
    path::{Path, PathBuf},
    sync::Arc,
};
use swc_common::errors::{ColorConfig, Handler};
use swc_ecma_parser::{JscTarget, TsConfig};
use test::Bencher;

#[bench]
fn bench_001(b: &mut Bencher) {
    run_bench(
        b,
        Path::new("tests")
            .join("fixture")
            .join("classes")
            .join("mixin")
            .join("access-modifiers")
            .join("test-1-2-4.ts"),
    );
}

#[bench]
fn bench_002(b: &mut Bencher) {
    run_bench(
        b,
        Path::new("tests")
            .join("fixture")
            .join("expression")
            .join("type-guards")
            .join("this")
            .join("test-1-1.ts"),
    );
}

#[bench]
fn bench_003(b: &mut Bencher) {
    run_bench(
        b,
        Path::new("tests")
            .join("fixture")
            .join("key-of")
            .join("indexed-access")
            .join("test-6-9.ts"),
    );
}

#[bench]
fn bench_004(b: &mut Bencher) {
    run_bench(
        b,
        Path::new("tests")
            .join("fixture")
            .join("libs")
            .join("rxjs")
            .join("internal")
            .join("util")
            .join("Immediate-1-1.ts"),
    );
}

#[bench]
fn bench_005(b: &mut Bencher) {
    fixture(b, "types::conditional::infer::test-2::index.ts");
}

#[bench]
fn bench_006(b: &mut Bencher) {
    fixture(b, "types::generic::context::inference::test-2-4.ts");
}

#[bench]
fn bench_007(b: &mut Bencher) {
    fixture(b, "libs::rxjs::pipe.ts");
}

#[bench]
fn bench_008(b: &mut Bencher) {
    fixture(b, "types::generic::context::inference::test-4.ts");
}

#[bench]
fn bench_009(b: &mut Bencher) {
    fixture(b, "types::mapped::inference::test-1-4.ts");
}

#[bench]
fn bench_010(b: &mut Bencher) {
    fixture(b, "types::mapped::inference::test-1-6-1.ts");
}

#[bench]
fn bench_011(b: &mut Bencher) {
    fixture(b, "types::mapped::types::array-tuple::test-5-1-3.ts");
}

#[bench]
fn bench_012(b: &mut Bencher) {
    fixture(b, "types::mapped::types::array-tuple::test-5-1-4.ts");
}

#[bench]
fn bench_013(b: &mut Bencher) {
    fixture(b, "types::mapped::types::array-tuple::test-5-1-5.ts");
}

#[bench]
fn bench_014(b: &mut Bencher) {
    fixture(b, "types::mapped::types::array-tuple::test-5-1.ts");
}

#[bench]
fn bench_015(b: &mut Bencher) {
    fixture(b, "types::mapped::types::array-tuple::test-1.ts");
}

#[bench]
fn bench_016(b: &mut Bencher) {
    fixture(b, "types::mapped::types::conformance-2::test-1.ts");
}

#[bench]
fn bench_017(b: &mut Bencher) {
    fixture(b, "types::mapped::types::array-tuple::test-5.ts");
}

fn fixture(b: &mut Bencher, s: &str) {
    let mut path = PathBuf::new().join("tests").join("fixture");
    for s in s.split("::") {
        path.push(s);
    }

    run_bench(b, path)
}

fn run_bench(b: &mut Bencher, path: PathBuf) {
    let size = read_to_string(&path).unwrap().as_bytes().len();
    b.bytes = size as _;

    ::testing::run_test2(false, |cm, _| {
        let handler = Handler::with_tty_emitter(ColorConfig::Always, true, false, Some(cm.clone()));
        let handler = Arc::new(handler);

        b.iter(|| {
            let mut checker = Checker::new(
                Logger::root(slog::Discard, slog::o!()),
                cm.clone(),
                handler.clone(),
                Env::simple(
                    Default::default(),
                    JscTarget::Es2020,
                    &Lib::load("es2020.full"),
                ),
                TsConfig {
                    ..Default::default()
                },
            );

            black_box(checker.check(Arc::new(path.to_path_buf())));
            black_box(checker.take_errors());
        });

        Ok(())
    })
    .unwrap();
}
