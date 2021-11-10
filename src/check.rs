use structopt::StructOpt;

#[derive(Debug, StructOpt)]
#[structopt(rename_all = "camel-case")]
pub struct CheckCommand {
    #[structopt(name = "file")]
    pub file: String,

    /// The builtin libraries to load.
    #[structopt(long)]
    pub libs: Vec<String>,

    /// Directory name of typings to load.
    #[structopt(long)]
    pub typings: Vec<String>,
}
