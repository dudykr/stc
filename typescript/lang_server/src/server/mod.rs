use self::capabilities::server_capabilities;
use anyhow::Context;
use anyhow::Error;
use crossbeam_channel::Receiver;
use crossbeam_channel::Sender;
use lsp_server::Connection;
use lsp_server::Message;
use lsp_server::Notification;
use lsp_types::InitializeParams;

mod capabilities;
mod dispatcher;
mod handlers;
mod state;

pub fn run() -> Result<(), Error> {
    log::info!("server will start");
    let (connection, io_threads) = Connection::stdio();

    let (initialize_id, initialize_params) = connection.initialize_start()?;
    log::info!("InitializeParams: {}", initialize_params);
    let initialize_params = serde_json::from_value::<InitializeParams>(initialize_params)
        .context("fail to deserialize lsp initialize params")?;

    let server_capabilities = server_capabilities(&initialize_params.capabilities);

    let initialize_result = lsp_types::InitializeResult {
        capabilities: server_capabilities,
        server_info: Some(lsp_types::ServerInfo {
            name: String::from("stc-lang-server"),
            version: Some(String::from(env!("CARGO_PKG_VERSION"))),
        }),
    };

    let initialize_result = serde_json::to_value(initialize_result).unwrap();

    connection.initialize_finish(initialize_id, initialize_result)?;

    if let Some(client_info) = initialize_params.client_info {
        log::info!(
            "Client '{}' {}",
            client_info.name,
            client_info.version.unwrap_or_default()
        );
    }

    handle(connection)?;

    io_threads.join()?;
    log::info!("server did shut down");
    Ok(())
}

fn handle(c: Connection) -> Result<(), Error> {
    Runner { sender: c.sender }.run(c.receiver)
}

struct Runner {
    sender: Sender<Message>,
}

impl Runner {
    fn show_notification(&mut self) -> Result<(), Error> {
        self.sender.send(Message::Notification(Notification {}))
    }

    pub fn run(self, inbox: Receiver<Message>) -> Result<(), Error> {}
}
