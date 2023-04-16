use std::{
    fs::read_to_string,
    path::{Path, PathBuf},
    process::{Command, Stdio},
};

use once_cell::sync::Lazy;
use serde::{de::DeserializeOwned, Serialize};
use serde_json::{json, Value};
use stc_ts_testing::lsp::LspClient;
use stc_utils::AHashSet;
use testing::run_test;
use tower_lsp::lsp_types::{Diagnostic, PublishDiagnosticsParams};
use tracing::info;

/// Builds the example lsp command, and returns to the path to it.
fn exec_path() -> PathBuf {
    static BIN_PATH: Lazy<PathBuf> = Lazy::new(|| {
        let output_dir = Path::new(".stc").join(".lsp-test");

        let mut c = Command::new("cargo");
        c.arg("build").arg("--example").arg("stc-debug-lsp");
        c.arg("-Z").arg("unstable-options");
        c.arg("--out-dir").arg(&output_dir);

        c.stderr(Stdio::inherit());
        let output = c.output().expect("cargo build --example lsp failed");

        assert!(output.status.success());

        output_dir.join("stc-debug-lsp")
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

fn did_open<V>(client: &mut LspClient, params: V) -> Vec<PublishDiagnosticsParams>
where
    V: Serialize,
{
    client.write_notification("textDocument/didOpen", params).unwrap();

    dbg!("after write_notification");
    handle_configuration_request(
        client,
        json!([{
          "enable": true,
          "codeLens": {
            "test": true
          }
        }]),
    );

    dbg!("after handle_configuration_request");

    read_diagnostics(client).0
}

#[tracing::instrument(skip_all)]
#[allow(unused)]
fn handle_configuration_request(client: &mut LspClient, result: Value) {
    // TODO: Implement this after implementing lsp

    // let (id, method, _) = client.read_request::<Value>().unwrap();
    // assert_eq!(method, "workspace/configuration");
    // client.write_response(id, result).unwrap();
}

#[tracing::instrument(skip_all)]
#[allow(unused)]
fn read_diagnostics(client: &mut LspClient) -> CollectedDiagnostics {
    CollectedDiagnostics(Default::default())

    // TODO: Implement this after implementing lsp

    // let mut diagnostics = vec![];
    // let (method, response) =
    // client.read_notification::<PublishDiagnosticsParams>().unwrap();
    // assert_eq!(method, "textDocument/publishDiagnostics");
    // diagnostics.push(response.unwrap());
    // CollectedDiagnostics(diagnostics)
}

fn shutdown(client: &mut LspClient) {
    info!("Shutdown");

    client.write_request::<_, _, Value>("shutdown", json!(null)).unwrap();
    client.write_notification("exit", json!(null)).unwrap();
}

#[derive(Debug, Clone)]
struct CollectedDiagnostics(Vec<PublishDiagnosticsParams>);

#[allow(unused)]
impl CollectedDiagnostics {
    /// Gets the diagnostics that the editor will see after all the publishes.
    pub fn viewed(&self) -> Vec<Diagnostic> {
        self.viewed_messages().into_iter().flat_map(|m| m.diagnostics).collect()
    }

    /// Gets the messages that the editor will see after all the publishes.
    pub fn viewed_messages(&self) -> Vec<PublishDiagnosticsParams> {
        // go over the publishes in reverse order in order to get
        // the final messages that will be shown in the editor
        let mut messages = Vec::new();
        let mut had_specifier = AHashSet::default();
        for message in self.0.iter().rev() {
            if had_specifier.insert(message.uri.clone()) {
                messages.insert(0, message.clone());
            }
        }
        messages
    }

    pub fn with_source(&self, source: &str) -> PublishDiagnosticsParams {
        self.viewed_messages()
            .iter()
            .find(|p| p.diagnostics.iter().any(|d| d.source == Some(source.to_string())))
            .map(ToOwned::to_owned)
            .unwrap()
    }

    // pub fn with_file_and_source(&self, specifier: &str, source: &str) ->
    // PublishDiagnosticsParams {     let specifier =
    // ModuleSpecifier::parse(specifier).unwrap();     self.viewed_messages()
    //         .iter()
    //         .find(|p| p.uri == specifier && p.diagnostics.iter().any(|d| d.source
    // == Some(source.to_string())))         .map(ToOwned::to_owned)
    //         .unwrap()
    // }
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

#[test]
fn test_hover() {
    run_test(false, |_cm, _handler| {
        let mut client = init("initialize_params.json");
        did_open(
            &mut client,
            json!({
              "textDocument": {
                "uri": "file:///a/file.ts",
                "languageId": "typescript",
                "version": 1,
                "text": "console.log('foo');\n"
              }
            }),
        );
        dbg!("After did_open");
        let (maybe_res, maybe_err) = client
            .write_request(
                "textDocument/hover",
                json!({
                  "textDocument": {
                    "uri": "file:///a/file.ts"
                  },
                  "position": {
                    "line": 0,
                    "character": 5
                  }
                }),
            )
            .unwrap();
        dbg!("After client.write_request");

        assert!(maybe_err.is_none());
        assert_eq!(
            maybe_res,
            Some(json!({
              "contents": "hover test",
            }))
        );
        shutdown(&mut client);

        Ok(())
    })
    .unwrap();
}
