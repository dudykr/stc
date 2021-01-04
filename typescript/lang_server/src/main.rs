use anyhow::Error;

mod server;

fn main() -> Result<(), Error> {
    server::run()
}
