use std::{
    fs::File,
    io::Read,
    path::{Path, PathBuf},
};

use test::{DynTestFn, ShouldPanic::No, TestDesc, TestDescAndFn, TestName, TestType};
use walkdir::WalkDir;

pub fn load_fixtures<F>(dir_name: &str, op: F) -> Vec<TestDescAndFn>
where
    F: Fn(PathBuf) -> Option<Box<dyn FnOnce() + Send + Sync>>,
{
    let root = {
        let mut root = Path::new(env!("CARGO_MANIFEST_DIR")).to_path_buf();
        root.push("tests");
        root.push(dir_name);

        root
    };

    eprintln!("Loading tests from {}", root.display());

    let mut tests = vec![];

    for entry in WalkDir::new(&root).into_iter() {
        let entry = entry.unwrap();
        let is_ts = entry.file_name().to_string_lossy().ends_with(".ts")
            || entry.file_name().to_string_lossy().ends_with(".tsx");
        if entry.file_type().is_dir() || !is_ts {
            continue;
        }

        let file_name = entry
            .path()
            .strip_prefix(&root)
            .expect("failed to strip prefix")
            .to_str()
            .unwrap()
            .to_string();

        eprintln!("Test: {}", entry.path().display());

        let input = {
            let mut buf = String::new();
            if File::open(entry.path())
                .unwrap()
                .read_to_string(&mut buf)
                .is_err()
            {
                continue;
            }
            buf
        };

        let test_name = format!("{}::{}", dir_name, file_name.replace("/", "::"));
        let test_fn = op(entry.path().to_path_buf());
        let (test_fn, ignore) = match test_fn {
            Some(v) => (v, false),
            None => ((box || {}) as Box<dyn FnOnce() + Send + Sync>, true),
        };
        let ignore = ignore || test_name.starts_with(".") || test_name.contains("::.");

        tests.push(TestDescAndFn {
            desc: TestDesc {
                test_type: TestType::UnitTest,
                name: TestName::DynTestName(test_name),
                ignore,
                should_panic: No,
                compile_fail: Default::default(),
                no_run: Default::default(),
                ignore_message: Default::default(),
            },
            testfn: DynTestFn(box move || {
                eprintln!(
                    "\n\n========== Running test {}\nSource:\n{}\n",
                    file_name, input
                );

                test_fn()
            }),
        });
    }

    tests
}
