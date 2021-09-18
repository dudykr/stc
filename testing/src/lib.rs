use once_cell::sync::Lazy;
use std::{self, fs::read_to_string, path::PathBuf, process::Command};
use tracing::Level;
use tracing_subscriber::prelude::*;

pub fn get_git_root() -> PathBuf {
    static DIR: Lazy<PathBuf> = Lazy::new(|| {
        let output = Command::new("git")
            .arg("rev-parse")
            .arg("--show-toplevel")
            .output()
            .expect("failed to get root git direcrtory");

        assert!(output.status.success());
        String::from_utf8_lossy(&output.stdout).trim().to_string().into()
    });

    DIR.clone()
}

/// Used for loading golden txt files.
pub fn load_txt(path: &str) -> Vec<String> {
    let s = read_to_string(&path).expect("failed to load txt file");
    s.lines()
        .map(|s| s.trim())
        .filter(|&s| s != "")
        .map(|s| s.to_string())
        .collect()
}

pub fn logger() -> impl tracing::Subscriber {
    tracing_subscriber::FmtSubscriber::builder()
        .without_time()
        .with_target(false)
        .with_ansi(true)
        .with_test_writer()
        .pretty()
        .finish()
}

pub fn init_logger() -> tracing::subscriber::DefaultGuard {
    tracing::subscriber::set_default(logger())
}

pub fn init_tracing(name: String) -> tracing::subscriber::DefaultGuard {
    let logger = tracing_subscriber::FmtSubscriber::builder()
        .without_time()
        .with_target(false)
        .with_ansi(true)
        .with_max_level(Level::DEBUG)
        .with_test_writer()
        .pretty()
        .finish();

    if cfg!(debug_assertions) {
        tracing::subscriber::set_default(logger)
    } else {
        // Install a new OpenTelemetry trace pipeline
        let tracer = opentelemetry_jaeger::new_pipeline()
            .with_service_name(name)
            .install_simple()
            .expect("failed to create open telemtry pipeline");

        // Create a tracing subscriber with the configured tracer
        let telemetry = tracing_opentelemetry::layer().with_tracer(tracer);

        let collector = logger.with(telemetry);
        tracing::subscriber::set_default(collector)
    }
}
