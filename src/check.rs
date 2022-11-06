use structopt::StructOpt;

/// Iterate 1 times to profile.
#[derive(Debug, StructOpt)]
#[structopt(rename_all = "camel-case")]
pub struct IterateCommand {
    #[structopt(name = "file")]
    pub file: String,

    /// The builtin libraries to load. Defaults to `es5`.
    #[structopt(long)]
    pub libs: Option<Vec<String>>,

    /// Directory name of typings to load.
    #[structopt(long)]
    pub types: Option<Vec<String>>,
}
