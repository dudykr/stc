//! Support for official typescript tests.

use serde::{Deserialize, Serialize};

/// Error from `tsc`.
#[derive(Debug, Default, Clone, PartialEq, Eq, PartialOrd, Ord, Hash, Serialize, Deserialize)]
pub struct TscError {
    #[serde(default)]
    pub file: Option<String>,
    pub line: usize,
    pub col: usize,
    pub code: usize,
    // pub msg: String,
}

impl TscError {
    pub fn ts_error_code(&self) -> String {
        format!("TS{}", self.code)
    }

    pub fn parse_all(output: &str) -> Vec<Self> {
        let mut errors = vec![];

        'outer: for line in output.lines() {
            if line.trim().is_empty() || line.starts_with(|c: char| c.is_whitespace()) {
                continue;
            }
            let mut error = TscError::default();

            for (idx, item) in line.split(':').enumerate() {
                match idx {
                    0 => {
                        let item = item.strip_prefix("\u{001b}[96m");

                        let item = match item {
                            Some(v) => v,
                            None => continue 'outer,
                        };
                        let item = item.strip_suffix("\u{001b}[0m").expect("expected colored output");

                        error.file = Some(item.to_string());
                    }
                    1 => {
                        let item = item
                            .strip_prefix("\u{001b}[93m")
                            .expect("expected colored output")
                            .strip_suffix("\u{001b}[0m")
                            .expect("expected colored output");
                        error.line = item.parse().expect("failed to parse line");
                    }
                    2 => {
                        for (j, item) in item.split(' ').enumerate() {
                            if let Some(item) = item.strip_prefix("TS") {
                                error.code = item.parse().expect("failed to parse ts error code");
                                continue;
                            }

                            if j == 0 {
                                let item = item
                                    .strip_prefix("\u{001b}[93m")
                                    .expect("expected colored output")
                                    .strip_suffix("\u{001b}[0m")
                                    .expect("expected colored output");

                                error.col = item.parse().expect("failed to parse column");
                            }
                        }
                    }
                    3 => {
                        // error.msg = line.to_string();
                    }
                    _ => {}
                }
            }

            errors.push(error);
        }

        errors
    }
}
