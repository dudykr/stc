use std::{
    fs::read_to_string,
    path::{Path, PathBuf},
    process::{Command, Stdio},
};

use once_cell::sync::Lazy;
use serde::de::DeserializeOwned;
use serde_json::{json, Value};
use stc_ts_testing::lsp::LspClient;

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

fn load_fixture(path: &str) -> Value {
    load_fixture_as(path)
}

fn load_fixture_as<T>(path: &str) -> T
where
    T: DeserializeOwned,
{
    let fixture_str = load_fixture_str(path);
    serde_json::from_str::<T>(&fixture_str).unwrap()
}

fn load_fixture_str(path: &str) -> String {
    let fixtures_path = Path::new("tests").join("fixture");
    let path = fixtures_path.join(path);
    read_to_string(path).unwrap()
}

fn init(init_path: &str) -> LspClient {
    let mut client = LspClient::new(&exec_path(), true).expect("failed to create a lsp client");

    client.write_request::<_, _, Value>("initialize", load_fixture(init_path)).unwrap();
    client.write_notification("initialized", json!({})).unwrap();
    client
}

fn shutdown(client: &mut LspClient) {
    client.write_request::<_, _, Value>("shutdown", json!(null)).unwrap();
    client.write_notification("exit", json!(null)).unwrap();
}

#[test]
fn test_build() {
    assert!(exec_path().exists());
}

#[test]
fn test_init() {
    let mut client = init("initialize_params.json");

    shutdown(&mut client);
}
