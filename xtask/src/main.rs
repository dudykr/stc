fn main() {
    let matches = clap::Command::new("xtask")
        .subcommand_required(true)
        .subcommand(clap::Command::new("check"))
        .get_matches();

    #[allow(clippy::single_match)]
    match matches.subcommand() {
        Some(("check", matches)) => check(matches),
        _ => {
            unreachable!("unhandled subcommand");
        }
    }
}

fn check(_: &clap::ArgMatches) {
    println!("test base");
    let status = cargo_test()
        .env("UPDATE", "1")
        .env("RUST_LOG", "off")
        .arg("--lib")
        .arg("--test")
        .arg("base")
        .arg("--package")
        .arg("stc_ts_file_analyzer")
        .status()
        .unwrap();
    assert!(status.success(), "test base failed: {status}");

    println!("test conformance");
    let output = cargo_test()
        .env("RUST_LOG", "error")
        .env("TEST", "")
        .env("DO_NOT_PRINT_MATCHED", "1")
        .arg("--test")
        .arg("tsc")
        .stdin(std::process::Stdio::null())
        .stdout(std::process::Stdio::piped())
        .stderr(std::process::Stdio::inherit())
        .output()
        .unwrap();
    let mut lines = String::from_utf8_lossy(&output.stdout)
        .lines()
        .filter(|line| line.ends_with(".ts ... ok"))
        .map(|line| {
            line.replace("test conformance::", "")
                .replace(" ... ok", "")
                .replace("::", "/")
                .replace("test ", "")
                .replace('\\', "/")
        })
        .collect::<Vec<_>>();
    lines.sort();
    use std::io::Write;
    let mut file = std::fs::File::create("crates/stc_ts_type_checker/tests/conformance.pass.txt").unwrap();
    for line in &lines {
        writeln!(file, "{line}").unwrap();
    }
    assert!(status.success(), "test base failed: {status}");
}

fn cargo_test() -> std::process::Command {
    let mut cmd = std::process::Command::new("cargo");
    cmd.env("RUST_BACKTRACE", "1")
        .env("RUST_MIN_STACK", "8388608")
        .arg("test")
        .arg("--features")
        .arg("no-threading")
        .arg("--features")
        .arg("tracing/max_level_off");
    cmd
}
