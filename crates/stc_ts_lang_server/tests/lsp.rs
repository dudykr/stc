use std::{
    path::PathBuf,
    process::{Command, Stdio},
};

use once_cell::sync::Lazy;

/// Builds the example lsp command, and returns to the path to it.
fn exec_path() -> PathBuf {
    static BIN_PATH: Lazy<PathBuf> = Lazy::new(|| {
        let mut c = Command::new("cargo");
        c.arg("build").arg("--example").arg("lsp");
        c.stderr(Stdio::inherit());
        let output = c.output().expect("cargo build --example lsp failed");

        assert!(output.status.success());

        let output = String::from_utf8(output.stdout).expect("cargo build --example lsp produced invalid utf8");

        output.into()
    });

    BIN_PATH.to_path_buf()
}

#[test]
fn test_build() {
    assert!(exec_path().exists());
}
