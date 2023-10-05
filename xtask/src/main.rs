use clap::{Parser, Subcommand};

mod file_analyzer;
mod type_checker;
mod utils;

pub(crate) use utils::*;

#[derive(Parser)]
/// Project task runner
struct Cli {
    #[clap(subcommand)]
    command: Command,
}

#[derive(Subcommand)]
enum Command {
    /// Run tests for stc_ts_file_analyzer
    TestAnalyzer(TestAnalyzerArgs),
    /// Automatically find a test to work on.
    AutoUnignore,
    /// Run tests for stc_ts_type_checker
    TestChecker(TestCheckerArgs),
}

#[derive(Parser)]
struct TestAnalyzerArgs {
    #[clap(long)]
    /// Run all base tests without output
    fast: bool,

    #[clap(long)]
    /// Run all tests
    all: bool,

    /// Run test with the given name
    name: Option<String>,
}

#[derive(Parser)]
struct TestCheckerArgs {
    // filter to tests with the given name
    name: Option<String>,
}

fn main() -> anyhow::Result<()> {
    let args = Cli::parse();

    match args.command {
        Command::TestAnalyzer(args) => {
            if !args.fast && !args.all && args.name.is_none() {
                anyhow::bail!("must specify one of --fast, --all, or test name");
            }
            if args.all && args.name.is_some() {
                anyhow::bail!("cannot specify both --all and test name");
            }

            file_analyzer::test_base(&file_analyzer::TestBaseArgs {
                log: args.fast.then_some("off"),
                name: args.name.as_deref(),
                ignored: false,
            })
        }
        Command::AutoUnignore => file_analyzer::auto_unignore(),
        Command::TestChecker(args) => match args.name {
            None => notify_result("Check", type_checker::check()),
            Some(name) => notify_result("Test", type_checker::test(&name)),
        },
    }
}
