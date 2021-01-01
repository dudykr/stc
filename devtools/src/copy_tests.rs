use anyhow::bail;
use anyhow::Error;
use std::path::PathBuf;
use structopt::StructOpt;

#[derive(Debug, StructOpt)]
pub struct CopyTests {
    /// Include tests only if there's no import.
    #[structopt(long)]
    no_import: bool,

    src: PathBuf,

    dst: PathBuf,
}

impl CopyTests {
    pub fn run(self) -> Result<(), Error> {
        bail!("not implemented yet")
    }
}
