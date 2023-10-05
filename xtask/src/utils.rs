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

#[derive(Debug)]
pub struct CargoTestArgs<'a> {
    pub log: Option<&'a str>,
    pub package: &'a str,
    pub test: Option<&'a str>,
    pub name: Option<&'a str>,
}

pub fn cargo_test(args: &CargoTestArgs<'_>) -> std::process::Command {
    let mut cmd = std::process::Command::new("cargo");
    if let Some(log) = args.log {
        cmd.env("RUST_LOG", log);
    }
    cmd.env("RUST_BACKTRACE", "1")
        .env("RUST_MIN_STACK", "8388608")
        .arg("--color")
        .arg("always")
        .arg("test")
        .arg("--features")
        .arg("no-threading")
        .arg("--package")
        .arg(args.package);
    if let Some(test) = args.test {
        cmd.arg("--test").arg(test);
    }
    if let Some(name) = args.name {
        cmd.arg(name);
    }

    cmd
}
