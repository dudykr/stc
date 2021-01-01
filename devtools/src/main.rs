use structopt::StructOpt;

#[derive(StructOpt)]
pub enum Command {
    CopyTests {
        /// Include tests only if there's no import.
        #[structopt(long)]
        no_import: bool,
    },
}

fn main() {
    println!("Hello, world!");
}
