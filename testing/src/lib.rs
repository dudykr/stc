use ansi_term::Color;
use once_cell::sync::Lazy;
use slog::Drain;
use slog::Level;
use slog::Logger;
use slog_envlogger::EnvLogger;
use std::fmt::Write;
use std::io;
use std::path::PathBuf;
use std::process::Command;
use std::sync::mpsc::channel;
use std::sync::mpsc::Receiver;
use std::sync::mpsc::Sender;
use std::sync::Mutex;

pub struct LogGuard {
    pub logger: Logger,
    _writer: LogWriter,
}

impl LogGuard {
    /// Print all logs.
    pub fn flush(self) {}
}

struct LogWriter {
    receiver: Receiver<String>,
}

impl Drop for LogWriter {
    fn drop(&mut self) {
        while let Ok(data) = self.receiver.try_recv() {
            print!("{}", data);
        }
    }
}

pub fn term_logger() -> Logger {
    fn no_timestamp(_: &mut dyn io::Write) -> io::Result<()> {
        Ok(())
    }

    let decorator = slog_term::TermDecorator::new().build();
    let drain = slog_term::CompactFormat::new(decorator)
        .use_custom_timestamp(no_timestamp)
        .build();

    let drain = EnvLogger::new(drain).fuse();

    let drain = std::sync::Mutex::new(drain).fuse();

    let log = slog::Logger::root(drain, slog::o!());

    log
}

pub fn logger() -> LogGuard {
    let (sender, receiver) = channel::<String>();
    let drain = TestDrain { sender };

    let drain = slog_envlogger::new(drain);

    let drain = Mutex::new(drain).fuse();

    let logger = Logger::root(drain, slog::o!());

    LogGuard {
        logger,
        _writer: LogWriter { receiver },
    }
}

struct TestDrain {
    sender: Sender<String>,
}

impl TestDrain {
    /// `Level` color
    ///
    /// Standard level to Unix color conversion used by `TermDecorator`
    fn level_to_color(level: slog::Level) -> Color {
        match level {
            Level::Critical => Color::Red,
            Level::Error => Color::Red,
            Level::Warning => Color::Yellow,
            Level::Info => Color::Green,
            Level::Debug => Color::Blue,
            Level::Trace => Color::White,
        }
    }
}

impl Drain for TestDrain {
    type Ok = ();
    type Err = slog::Never;

    fn log(
        &self,
        record: &slog::Record,
        _values: &slog::OwnedKVList,
    ) -> Result<Self::Ok, Self::Err> {
        let mut s = String::new();

        let level = record.level();
        let color = Self::level_to_color(level);
        write!(s, "{}", &color.paint(level.to_string())).unwrap();
        write!(s, ": ").unwrap();
        write!(s, "{}", record.msg()).unwrap();
        write!(s, "\n").unwrap();

        self.sender.send(s).unwrap();

        Ok(())
    }
}

pub fn get_git_root() -> PathBuf {
    static DIR: Lazy<PathBuf> = Lazy::new(|| {
        let output = Command::new("git")
            .arg("rev-parse")
            .arg("--show-toplevel")
            .output()
            .expect("failed to get root git direcrtory");

        assert!(output.status.success());
        String::from_utf8_lossy(&output.stdout)
            .trim()
            .to_string()
            .into()
    });

    DIR.clone()
}
