//! Provide general helpers to run a nested cargo command.

use std::path::PathBuf;

/// tracing static filter level, to improve performance at the cost of needing
/// to rebuild.
#[derive(Debug)]
pub enum LogFilterLevel {
    Off,
    Error,
    Warn,
    Info,
    Debug,
    Trace,
}

impl std::str::FromStr for LogFilterLevel {
    type Err = anyhow::Error;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        match s {
            "off" => Ok(LogFilterLevel::Off),
            "error" => Ok(LogFilterLevel::Error),
            "warn" => Ok(LogFilterLevel::Warn),
            "info" => Ok(LogFilterLevel::Info),
            "debug" => Ok(LogFilterLevel::Debug),
            "trace" => Ok(LogFilterLevel::Trace),
            _ => anyhow::bail!("invalid log filter level: {s}"),
        }
    }
}

impl LogFilterLevel {
    fn as_max_level_feature(&self) -> &'static str {
        match self {
            LogFilterLevel::Off => "tracing/max_level_off",
            LogFilterLevel::Error => "tracing/max_level_error",
            LogFilterLevel::Warn => "tracing/max_level_warn",
            LogFilterLevel::Info => "tracing/max_level_info",
            LogFilterLevel::Debug => "tracing/max_level_debug",
            LogFilterLevel::Trace => "tracing/max_level_trace",
        }
    }
}

/// Build a basic cargo test command
#[derive(Debug)]
pub struct CargoTestArgs<'a> {
    log_max_level: Option<LogFilterLevel>,
    log: Option<&'a str>,
    package: &'a str,
    lib: bool,
    test: Option<&'a str>,
    name: Option<&'a str>,
}

impl<'a> CargoTestArgs<'a> {
    /// Create an empty test command builder for the given package name.
    ///
    /// @param package
    ///     cargo --package to build (required), e.g. stc_ts_file_analyzer
    pub fn new(package: &'a str) -> Self {
        Self {
            log_max_level: None,
            log: None,
            package,
            lib: false,
            test: None,
            name: None,
        }
    }

    /// Set --features tracing/max_level_*, runs faster than with_log(), but
    /// requires rebuilding
    #[must_use]
    pub fn with_log_max_level(mut self, log_max_level: LogFilterLevel) -> Self {
        self.log_max_level = Some(log_max_level);
        self
    }

    /// Set RUST_LOG=*: does not require rebuild and can include fine-grained
    /// directives, but is slower than with_log_max_level().
    #[must_use]
    pub fn with_log(mut self, log: &'a str) -> Self {
        self.log = Some(log);
        self
    }

    /// Set cargo test --lib to run the unit tests in the library
    #[must_use]
    pub fn with_lib(mut self, lib: bool) -> Self {
        self.lib = lib;
        self
    }

    /// Set cargo test --test file to run, e.g. base or prof
    #[must_use]
    pub fn with_test(mut self, test: &'a str) -> Self {
        self.test = Some(test);
        self
    }

    /// Set cargo test name to match, e.g. compare
    #[must_use]
    pub fn with_name(mut self, name: &'a str) -> Self {
        self.name = Some(name);
        self
    }

    /// Return a Command that can be further configured and run.
    pub fn to_command(&self) -> std::process::Command {
        let mut cmd = std::process::Command::new("cargo");
        // normalize working dir
        cmd.current_dir(root_dir());

        if let Some(log) = self.log {
            cmd.env("RUST_LOG", log);
        }
        cmd.env("RUST_BACKTRACE", "1")
            .env("RUST_MIN_STACK", "8388608")
            .arg("--color")
            .arg("always")
            .arg("test")
            .arg("--package")
            .arg(self.package)
            .arg("--features")
            .arg("no-threading");
        if let Some(level) = &self.log_max_level {
            cmd.arg("--features").arg(level.as_max_level_feature());
        }
        if self.lib {
            cmd.arg("--lib");
        }
        if let Some(test) = self.test {
            cmd.arg("--test").arg(test);
        }
        if let Some(name) = self.name {
            cmd.arg(name);
        }
        cmd
    }
}

pub fn root_dir() -> PathBuf {
    let mut manifest_dir =
        PathBuf::from(std::env::var_os("CARGO_MANIFEST_DIR").expect("$CARGO_MANIFEST_DIR not set: are you using 'cargo xtask ...'?"));
    // xtask manifest should be at: <root>/xtask/Cargo.toml
    manifest_dir.pop();
    manifest_dir
}
