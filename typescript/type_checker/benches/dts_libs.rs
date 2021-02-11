#![feature(box_syntax)]
#![feature(test)]

extern crate test;

use slog::Logger;
use stc_ts_builtin_types::Lib;
use stc_ts_file_analyzer::env::Env;
use stc_ts_module_loader::resolver::node::NodeResolver;
use stc_ts_type_checker::Checker;
use std::{
    hint::black_box,
    path::{Path, PathBuf},
    process::{Command, Stdio},
    sync::Arc,
};
use swc_common::errors::{ColorConfig, Handler};
use swc_ecma_parser::{JscTarget, TsConfig};
use test::Bencher;

#[bench]
fn rxjs(b: &mut Bencher) {
    let dir = clone("https://github.com/ReactiveX/rxjs.git", "7.0.0-beta.7");
    run_bench(b, &dir.join("src").join("index.ts"));
}

fn clone(git_url: &str, tag: &str) -> PathBuf {
    let dir = PathBuf::from(env!("OUT_DIR")).join(format!("bench-{}", git_url.split("/").last().unwrap()));
    if dir.exists() {
        return dir;
    }
    let mut c = Command::new("git");
    let status = c
        .arg("clone")
        .arg(git_url)
        .arg("-b")
        .arg(tag)
        .arg(&dir)
        .stderr(Stdio::inherit())
        .stdout(Stdio::inherit())
        .status()
        .expect("git clone failed");

    assert!(status.success());

    dir
}

fn run_bench(b: &mut Bencher, path: &Path) {
    ::testing::run_test2(false, |cm, _| {
        let handler = Handler::with_tty_emitter(ColorConfig::Always, true, false, Some(cm.clone()));

        let handler = Arc::new(handler);

        b.iter(|| {
            let mut checker = Checker::new(
                Logger::root(slog::Discard, slog::o!()),
                cm.clone(),
                handler.clone(),
                Env::simple(Default::default(), JscTarget::Es2020, &Lib::load("es2020.full")),
                TsConfig { ..Default::default() },
                None,
                Arc::new(NodeResolver),
            );

            let id = checker.check(Arc::new(path.to_path_buf()));
            black_box(checker.take_errors());
            black_box(checker.take_dts(id));
        });

        Ok(())
    })
    .unwrap();
}
