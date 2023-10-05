use std::path::Path;

use anyhow::ensure;

use crate::utils::{cargo_test, CargoTestArgs};

pub struct TestBaseArgs<'a> {
    pub log: Option<&'a str>,
    pub name: Option<&'a str>,
    pub ignored: bool,
}

pub fn test_base(args: &TestBaseArgs<'_>) -> anyhow::Result<()> {
    println!("analyzer test base");
    let mut cmd = cargo_test(&CargoTestArgs {
        log: args.log,
        package: "stc_ts_file_analyzer",
        test: Some("base"),
        name: args.name,
    });

    if let Some(value) = args.log {
        cmd.env("RUST_LOG", value);
    }

    cmd.env("UPDATE", "1")
        .arg("--lib")
        .arg("--")
        .arg("-Zunstable-options")
        .arg("--report-time");

    if args.ignored {
        cmd.arg("--ignored");
    }

    let status = cmd.status()?;
    ensure!(status.success(), "test base failed: {status}");
    Ok(())
}

pub fn auto_unignore() -> anyhow::Result<()> {
    unignore_cleanup()?;

    // touch crates/stc_ts_file_analyzer/tests/base.rs
    let _ = test_base(&TestBaseArgs {
        log: None,
        name: None,
        ignored: true,
    });

    unignore_cleanup()?;
    Ok(())
}

fn unignore_cleanup() -> anyhow::Result<()> {
    println!("Deleting cache for ignored tests");
    // find ./tests/tsc -name '\.*.tsc-errors.json' -type f -delete
    return walk(Path::new("crates/stc_ts_file_analyzer/tests/tsc"));

    fn walk(dir: &Path) -> anyhow::Result<()> {
        for entry in std::fs::read_dir(dir)? {
            let entry = entry?;
            let path = entry.path();
            if entry.metadata()?.is_dir() {
                walk(&path)?;
            } else {
                let name = path.file_name().expect("invalid path").to_str().expect("invalid file name");
                if name.starts_with('.') && name.ends_with(".tsc-errors.json") {
                    std::fs::remove_file(&path)?;
                }
            }
        }
        Ok(())
    }
}
