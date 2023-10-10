pub fn notify_result(prefix: &str, result: anyhow::Result<()>) -> anyhow::Result<()> {
    match &result {
        Ok(()) => {
            notify_rust::Notification::new().summary(&format!("{prefix} done!")).show()?;
        }
        Err(err) => {
            notify_rust::Notification::new()
                .summary(&format!("{prefix} failed!"))
                .body(&format!("{err}"))
                .show()?;
        }
    }
    result
}
