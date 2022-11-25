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
use tower_lsp::lsp_types::{Diagnostic, PublishDiagnosticsParams};
use tracing::debug;

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

fn did_open<V>(client: &mut LspClient, params: V) -> Vec<PublishDiagnosticsParams>
where
    V: Serialize,
{
    client.write_notification("textDocument/didOpen", params).unwrap();

    handle_configuration_request(
        client,
        json!([{
          "enable": true,
          "codeLens": {
            "test": true
          }
        }]),
    );
    read_diagnostics(client).0
}

fn handle_configuration_request(client: &mut LspClient, result: Value) {
    debug!("handle_configuration_request");

    let (id, method, _) = client.read_request::<Value>().unwrap();
    assert_eq!(method, "workspace/configuration");
    client.write_response(id, result).unwrap();
}

fn read_diagnostics(client: &mut LspClient) -> CollectedDiagnostics {
    debug!("read_diagnostics");

    // diagnostics come in batches of three unless they're cancelled
    let mut diagnostics = vec![];
    for _ in 0..3 {
        let (method, response) = client.read_notification::<PublishDiagnosticsParams>().unwrap();
        assert_eq!(method, "textDocument/publishDiagnostics");
        diagnostics.push(response.unwrap());
    }
    CollectedDiagnostics(diagnostics)
}

fn shutdown(client: &mut LspClient) {
    client.write_request::<_, _, Value>("shutdown", json!(null)).unwrap();
    client.write_notification("exit", json!(null)).unwrap();
}

#[derive(Debug, Clone)]
struct CollectedDiagnostics(Vec<PublishDiagnosticsParams>);

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
    let mut client = init("initialize_params.json");
    did_open(
        &mut client,
        json!({
          "textDocument": {
            "uri": "file:///a/file.ts",
            "languageId": "typescript",
            "version": 1,
            "text": "console.log(Deno.args);\n"
          }
        }),
    );
    let (maybe_res, maybe_err) = client
        .write_request(
            "textDocument/hover",
            json!({
              "textDocument": {
                "uri": "file:///a/file.ts"
              },
              "position": {
                "line": 0,
                "character": 19
              }
            }),
        )
        .unwrap();
    assert!(maybe_err.is_none());
    assert_eq!(
        maybe_res,
        Some(json!({
          "contents": [
            {
              "language": "typescript",
              "value": "const Deno.args: string[]"
            },
            "Returns the script arguments to the program.\n\nGive the following command line invocation of Deno:\n\n```sh\ndeno run --allow-read https://deno.land/std/examples/cat.ts /etc/passwd\n```\n\nThen `Deno.args` will contain:\n\n```\n[ \"/etc/passwd\" ]\n```\n\nIf you are looking for a structured way to parse arguments, there is the\n[`std/flags`](https://deno.land/std/flags) module as part of the Deno\nstandard library.",
            "\n\n*@category* - Runtime Environment",
          ],
          "range": {
            "start": {
              "line": 0,
              "character": 17
            },
            "end": {
              "line": 0,
              "character": 21
            }
          }
        }))
    );
    shutdown(&mut client);
}
