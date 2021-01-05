use anyhow::Error;

mod server;
mod util;

fn main() -> Result<(), Error> {
    server::run()
}
