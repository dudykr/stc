use structopt::StructOpt;

/// Perform type checking, but this command is not public api and is only used
/// for testing.
#[derive(Debug, StructOpt)]
#[structopt(rename_all = "camel-case")]
pub struct TestCommand {
    #[structopt(name = "file")]
    pub file: String,

    /// The builtin libraries to load. Defaults to `es5`.
    #[structopt(long)]
    pub libs: Option<Vec<String>>,

    /// Directory name of typings to load.
    #[structopt(long)]
    pub types: Option<Vec<String>>,
}
