use once_cell::sync::Lazy;
use std::{self, fs::read_to_string, path::PathBuf, process::Command};

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
