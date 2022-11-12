use std::{
    path::{Path, PathBuf},
    process::{Command, Stdio},
};

use once_cell::sync::Lazy;

/// Builds the example lsp command, and returns to the path to it.
fn exec_path() -> PathBuf {
    static BIN_PATH: Lazy<PathBuf> = Lazy::new(|| {
        let output_dir = Path::new(".stc").join(".lsp-test");

        let mut c = Command::new("cargo");
        c.arg("build").arg("--example").arg("lsp");
        c.arg("-Z").arg("unstable-options");
        c.arg("--out-dir").arg(&output_dir);

        c.stderr(Stdio::inherit());
        let output = c.output().expect("cargo build --example lsp failed");

        assert!(output.status.success());

        output_dir.join("lsp")
    });

    BIN_PATH.to_path_buf()
}

#[test]
fn test_build() {
    assert!(exec_path().exists());
}
