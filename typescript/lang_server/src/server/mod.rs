use self::capabilities::server_capabilities;
use self::state::GlobalState;
use anyhow::bail;
use anyhow::Context;
use anyhow::Error;
use crossbeam_channel::Receiver;
use dispatcher::RequestDispatcher;
use lsp_server::Connection;
use lsp_server::Message;
use lsp_server::Notification;
use lsp_server::Request;
use lsp_types::notification::Notification as _;
use lsp_types::request;
use lsp_types::InitializeParams;
use std::env;
use std::time::Duration;
use std::time::Instant;

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

    let server_capabilities = server_capabilities(initialize_params.capabilities);

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
    GlobalState::new(c.sender).run(c.receiver)
}

enum Event {
    Lsp(Message),
}

impl GlobalState {
    pub fn run(mut self, inbox: Receiver<Message>) -> Result<(), Error> {
        let save_registration_options = lsp_types::TextDocumentSaveRegistrationOptions {
            include_text: Some(false),
            text_document_registration_options: lsp_types::TextDocumentRegistrationOptions {
                document_selector: Some(vec![
                    lsp_types::DocumentFilter {
                        language: None,
                        scheme: None,
                        pattern: Some("**/*.js".into()),
                    },
                    lsp_types::DocumentFilter {
                        language: None,
                        scheme: None,
                        pattern: Some("**/*.jsx".into()),
                    },
                    lsp_types::DocumentFilter {
                        language: None,
                        scheme: None,
                        pattern: Some("**/*.ts".into()),
                    },
                    lsp_types::DocumentFilter {
                        language: None,
                        scheme: None,
                        pattern: Some("**/*.ts".into()),
                    },
                    lsp_types::DocumentFilter {
                        language: None,
                        scheme: None,
                        pattern: Some("**/*.tsx".into()),
                    },
                    lsp_types::DocumentFilter {
                        language: None,
                        scheme: None,
                        pattern: Some("**/package.json".into()),
                    },
                ]),
            },
        };

        let registration = lsp_types::Registration {
            id: "textDocument/didSave".to_string(),
            method: "textDocument/didSave".to_string(),
            register_options: Some(serde_json::to_value(save_registration_options).unwrap()),
        };
        self.send_request::<lsp_types::request::RegisterCapability>(
            lsp_types::RegistrationParams {
                registrations: vec![registration],
            },
            |_, _| (),
        );

        while let Some(event) = self.next_event(&inbox) {
            if let Event::Lsp(Message::Notification(not)) = &event {
                if not.method == lsp_types::notification::Exit::METHOD {
                    return Ok(());
                }
            }
            self.handle_event(event)?
        }

        bail!("client exited without proper shutdown sequence")
    }

    fn next_event(&self, inbox: &Receiver<Message>) -> Option<Event> {
        // select! {
        //     recv(inbox) -> msg =>
        //         msg.ok().map(Event::Lsp),
        // }
        inbox.recv().ok().map(Event::Lsp)
    }

    fn handle_event(&mut self, event: Event) -> Result<(), Error> {
        let loop_start = Instant::now();
        // NOTE: don't count blocking select! call as a loop-turn time
        let _p = profile::span("GlobalState::handle_event");

        match event {
            Event::Lsp(msg) => {
                //
                match msg {
                    Message::Request(req) => {
                        self.handle_request(req);
                    }
                    Message::Notification(noti) => {
                        self.handle_notification(noti)?;
                    }
                    Message::Response(resp) => self.handle_response_from_client(resp),
                }
            }
        }

        let loop_duration = loop_start.elapsed();
        if loop_duration > Duration::from_millis(100) {
            log::warn!("overly long loop turn: {:?}", loop_duration);
            if env::var("STC_PROFILE").is_ok() {
                self.show_message(
                    lsp_types::MessageType::Error,
                    format!("overly long loop turn: {:?}", loop_duration),
                )
            }
        }

        Ok(())
    }

    fn handle_request(&mut self, req: Request) {
        RequestDispatcher {
            req: Some(req),
            global_state: self,
        }
        .on::<request::Completion>(handlers::handler_completion)
        .finish();
    }

    fn handle_notification(&mut self, noti: Notification) -> Result<(), Error> {
        bail!("unimplemented")
    }
}
