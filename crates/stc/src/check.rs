use clap::Args;

/// Perform type checking, but this command is not public api and is only used
/// for testing.
#[derive(Debug, Args)]
#[clap(rename_all = "camel-case")]
pub struct TestCommand {
    #[clap(name = "file")]
    pub file: String,

    /// The builtin libraries to load. Defaults to `es5`.
    #[clap(long)]
    pub libs: Option<Vec<String>>,

    /// Directory name of typings to load.
    #[clap(long)]
    pub types: Option<Vec<String>>,
}
