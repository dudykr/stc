//! Provide helpers for crates/stc_ts_file_analyzer

use std::path::Path;

use anyhow::ensure;

use crate::run_cargo::{CargoTestArgs, LogFilterLevel};

/// Command builder for running tests/base.rs
pub struct TestBaseArgs<'a> {
    inner: CargoTestArgs<'a>,
    ignored: bool,
}

impl<'a> TestBaseArgs<'a> {
    /// Create an empty TestBaseArgs.
    pub fn new() -> Self {
        Self {
            inner: CargoTestArgs::new("stc_ts_file_analyzer").with_test("base"),
            ignored: false,
        }
    }

    /// Set the tracing static level filter.
    #[must_use]
    pub fn with_log_max_level(mut self, log_max_level: LogFilterLevel) -> Self {
        self.inner = self.inner.with_log_max_level(log_max_level);
        self
    }

    /// Set the tracing log directive.
    #[must_use]
    pub fn with_log(mut self, log: &'a str) -> Self {
        self.inner = self.inner.with_log(log);
        self
    }

    /// Set the test name to run.
    #[must_use]
    pub fn with_name(mut self, name: &'a str) -> Self {
        self.inner = self.inner.with_name(name);
        self
    }

    /// cargo test --ignored: Whether to run ignored tests. Used by
    /// auto-unignore.
    #[must_use]
    pub fn with_ignored(mut self) -> Self {
        self.ignored = true;
        self
    }

    /// Set the standard --fast options.
    #[must_use]
    pub fn fast(self) -> Self {
        self.with_log_max_level(LogFilterLevel::Error).with_log("off")
    }

    /// Return a Command that can be used to run the configured tests.
    pub fn to_command(&self) -> std::process::Command {
        let mut cmd = self.inner.to_command();

        cmd.env("UPDATE", "1")
            .arg("--lib")
            .arg("--")
            .arg("-Zunstable-options")
            .arg("--report-time");

        if self.ignored {
            cmd.arg("--ignored");
        }

        cmd
    }

    /// Run the tests using inherited stdio.
    pub fn run(&self) -> anyhow::Result<()> {
        println!("analyzer test base");

        // Shell scripts did this here, presumably to force a fresh build:
        //   touch crates/stc_ts_file_analyzer/tests/base.rs
        // I'm not sure why, it seems to work OK without it and to do it portably needs
        // #[feature(file_set_times)]:   https://github.com/rust-lang/rust/issues/98245
        // Add it if we need it, but it might be better with a build.rs with the
        // appropriate println!("cargo:rerun-if-changed=<path>");

        let mut cmd = self.to_command();

        let status = cmd.status()?;
        ensure!(status.success(), "test base failed: {status}");
        Ok(())
    }
}

/// Implements xtask auto-unignore
pub fn auto_unignore() -> anyhow::Result<()> {
    // Remove any ignored tests cached outputs so they are re-run.
    // (shouldn't all tests be re-run?)
    unignore_cleanup()?;

    // Ignore the result of testing, we just want the updated .tsc-errors.json as a
    // side-effect.
    let _ = TestBaseArgs::new().with_ignored().run();

    // Remove failed tests caches again, only successful tests should be committed.
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
                let name = path
                    .file_name()
                    .expect("missing file name in directory entry")
                    .to_str()
                    .expect("non-unicode test file name");
                if name.starts_with('.') && name.ends_with(".tsc-errors.json") {
                    std::fs::remove_file(&path)?;
                }
            }
        }
        Ok(())
    }
}
