use dashmap::DashMap;
use fxhash::FxBuildHasher;
use std::{
    fs::File,
    io::Read,
    path::{Path, PathBuf},
    sync::Arc,
};
use swc_common::{
    comments::{Comment, Comments},
    BytePos,
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
            if File::open(entry.path()).unwrap().read_to_string(&mut buf).is_err() {
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
                allow_fail: false,
                compile_fail: Default::default(),
                no_run: Default::default(),
            },
            testfn: DynTestFn(box move || {
                eprintln!("\n\n========== Running test {}\nSource:\n{}\n", file_name, input);

                test_fn()
            }),
        });
    }

    tests
}

pub type CommentMap = Arc<DashMap<BytePos, Vec<Comment>, FxBuildHasher>>;

/// Multi-threaded implementation of [Comments]
#[derive(Clone, Default)]
pub struct SwcComments {
    pub leading: CommentMap,
    pub trailing: CommentMap,
}

impl Comments for SwcComments {
    fn add_leading(&self, pos: BytePos, cmt: Comment) {
        self.leading.entry(pos).or_default().push(cmt);
    }

    fn add_leading_comments(&self, pos: BytePos, comments: Vec<Comment>) {
        self.leading.entry(pos).or_default().extend(comments);
    }

    fn has_leading(&self, pos: BytePos) -> bool {
        self.leading.contains_key(&pos)
    }

    fn move_leading(&self, from: BytePos, to: BytePos) {
        let cmt = self.leading.remove(&from);

        if let Some(cmt) = cmt {
            self.leading.entry(to).or_default().extend(cmt.1);
        }
    }

    fn take_leading(&self, pos: BytePos) -> Option<Vec<Comment>> {
        self.leading.remove(&pos).map(|v| v.1)
    }

    fn add_trailing(&self, pos: BytePos, cmt: Comment) {
        self.trailing.entry(pos).or_default().push(cmt)
    }

    fn add_trailing_comments(&self, pos: BytePos, comments: Vec<Comment>) {
        self.trailing.entry(pos).or_default().extend(comments)
    }

    fn has_trailing(&self, pos: BytePos) -> bool {
        self.trailing.contains_key(&pos)
    }

    fn move_trailing(&self, from: BytePos, to: BytePos) {
        let cmt = self.trailing.remove(&from);

        if let Some(cmt) = cmt {
            self.trailing.entry(to).or_default().extend(cmt.1);
        }
    }

    fn take_trailing(&self, pos: BytePos) -> Option<Vec<Comment>> {
        self.trailing.remove(&pos).map(|v| v.1)
    }
}
