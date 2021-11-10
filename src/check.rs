use structopt::StructOpt;

#[derive(Debug, StructOpt)]
#[structopt(rename_all = "camel-case")]
pub struct CheckCommand {
    #[structopt(name = "file")]
    pub file: String,

    #[structopt(long)]
    pub libs: Vec<String>,
}
