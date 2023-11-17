use anyhow::ensure;

use crate::run_cargo::CargoTestArgs;

pub fn check() -> anyhow::Result<()> {
    test_regressions()?;
    test_conformance()?;
    Ok(())
}

pub fn test(test: &str) -> anyhow::Result<()> {
    test_regressions()?;
    println!("checker test {test:?}");
    let status = cargo_test_tsc("debug,swc_common=off", test).arg("--quiet").status()?;
    ensure!(status.success(), "test checker {test:?} failed: {status}");
    Ok(())
}

fn test_regressions() -> anyhow::Result<()> {
    use crate::file_analyzer::TestBaseArgs;
    // Prevent regression using faster checks
    TestBaseArgs::new().fast().run()
}

fn cargo_test_tsc(log: &str, test: &str) -> std::process::Command {
    let mut cmd = CargoTestArgs::new("stc_ts_type_checker")
        .with_log(log)
        .with_test("tsc")
        .to_command();
    cmd.env("TEST", test);
    cmd
}

fn test_conformance() -> anyhow::Result<()> {
    println!("test conformance");
    let mut child = cargo_test_tsc("error", "")
        .env("DO_NOT_PRINT_MATCHED", "1")
        .stdin(std::process::Stdio::null())
        .stdout(std::process::Stdio::piped())
        .stderr(std::process::Stdio::inherit())
        .spawn()?;

    // This is more complicated than using .output(), but there's a *lot* of
    // output we throw out, so this is much better for memory. You could also use it
    // for reporting progress, I guess?
    let mut passed_test_lines = Vec::new();
    let stdout = child.stdout.take().unwrap();
    std::thread::scope({
        // Avoid moving the Vec into the thread
        let passed_test_lines = &mut passed_test_lines;
        move |scope| {
            scope.spawn(move || {
                use std::io::BufRead;
                let stdout = std::io::BufReader::new(stdout);
                for line in stdout.lines() {
                    let mut line = line.unwrap();
                    if line.starts_with("test ") {
                        if line.ends_with(" ... ok") {
                            line = line
                                .replace("test conformance::", "")
                                .replace(" ... ok", "")
                                .replace("::", "/")
                                .replace("test ", "")
                                .replace('\\', "/");
                            line.push('\n');
                            passed_test_lines.push(line);
                            eprint!(".");
                        } else if line.ends_with(" ... FAILED") {
                            eprint!("F");
                        } else if line.ends_with(" ... ignored") {
                            eprint!("i");
                        } else {
                            eprintln!();
                            eprintln!("{line}");
                        }
                    }
                }
            });
        }
    });
    eprintln!();

    let status = child.wait()?;

    passed_test_lines.sort();
    std::fs::write("crates/stc_ts_type_checker/tests/conformance.pass.txt", passed_test_lines.concat())?;

    ensure!(status.success(), "test conformance failed: {status}");

    Ok(())
}
