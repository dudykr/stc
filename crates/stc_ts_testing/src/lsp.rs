use std::{path::Path, process::Stdio};

use anyhow::{Context, Result};
use tokio::{
    io::{BufReader, BufWriter},
    process::{Child, ChildStdin, ChildStdout},
};

pub struct LspClient {
    child: Child,

    reader: BufReader<ChildStdout>,
    writer: BufWriter<ChildStdin>,
}

impl LspClient {
    pub async fn new(exec_path: &Path) -> Result<Self> {
        let mut child = tokio::process::Command::new(exec_path)
            .stdin(Stdio::piped())
            .stdout(Stdio::piped())
            .kill_on_drop(true)
            .spawn()
            .context("failed to spawn lsp executable")?;

        let stdout = child.stdout.take().unwrap();
        let reader = BufReader::new(stdout);

        let stdin = child.stdin.take().unwrap();
        let writer = BufWriter::new(stdin);

        Ok(LspClient { child, reader, writer })
    }
}
