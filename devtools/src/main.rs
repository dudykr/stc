use self::copy_tests::CopyTests;
use anyhow::Error;
use structopt::StructOpt;

mod copy_tests;

#[derive(Debug, StructOpt)]
pub enum Command {
    CopyTests(CopyTests),
}

fn main() -> Result<(), Error> {
    let cmd: Command = Command::from_args();

    match cmd {
        Command::CopyTests(cmd) => {
            cmd.run()?;
        }
    }

    Ok(())
}
