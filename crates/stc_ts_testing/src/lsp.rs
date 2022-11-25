use std::{path::Path, process::Stdio};

use anyhow::{Context, Result};
use tokio::process::Child;

pub struct LspClient {
    child: Child,
}

impl LspClient {
    pub async fn new(exec_path: &Path) -> Result<Self> {
        let child = tokio::process::Command::new(exec_path)
            .stdin(Stdio::piped())
            .stdout(Stdio::piped())
            .stderr(Stdio::piped())
            .kill_on_drop(true)
            .spawn()
            .context("failed to spawn lsp executable")?;

        Ok(LspClient { child })
    }
}
