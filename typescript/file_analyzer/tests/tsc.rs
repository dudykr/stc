use std::{
    path::{Path, PathBuf},
    process::Command,
};
use testing::fixture;

/// This invokes `tsc` to get expected result.
#[fixture("tsc/**/*.ts")]
fn compare(input: PathBuf) {
    let tsc_result = invoke_tsc(&input);

    panic!("{}", tsc_result)
}

fn invoke_tsc(input: &Path) -> String {
    let output = Command::new("tsc")
        .arg("--noEmit")
        .arg(&input)
        .output()
        .expect("failed to invoke tsc");
    let stdout = String::from_utf8_lossy(&output.stdout);
    let stderr = String::from_utf8_lossy(&output.stderr);

    eprintln!("tsc output: \nStdout:\n{}\nStderr:\n{}", stdout, stderr);

    stderr.into_owned()
}
