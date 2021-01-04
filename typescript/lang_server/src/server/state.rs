use anyhow::Context;
use anyhow::Error;
use crossbeam_channel::Sender;
use lsp_server::Message;
use lsp_server::Notification;
use lsp_server::Request;
use lsp_server::Response;
use lsp_types::TextDocumentPositionParams;
use std::ops::Deref;
use std::time::Instant;
use swc_common::BytePos;
use swc_common::SourceFileAndBytePos;

pub(crate) type ReqQueue = lsp_server::ReqQueue<(String, Instant), ReqHandler>;

pub(crate) type ReqHandler = fn(&mut GlobalState, Response);

pub(crate) struct GlobalState {
    shared: Shared,
    req_queue: ReqQueue,
}

#[derive(Clone)]
pub(crate) struct Shared {
    sender: Sender<Message>,
}

pub(crate) struct GlobalStateSnapshot {
    shared: Shared,
}

impl GlobalState {
    pub fn new(sender: Sender<Message>) -> Self {
        Self {
            shared: Shared { sender },
            req_queue: Default::default(),
        }
    }

    pub fn snapshot(&self) -> GlobalStateSnapshot {
        GlobalStateSnapshot {
            shared: self.shared.clone(),
        }
    }
}

impl Shared {
    pub fn show_notification(&mut self, noti: Notification) -> Result<(), Error> {
        self.sender
            .send(Message::Notification(noti))
            .context("failed to show notification")
    }

    pub fn respond(&self, response: Response) -> Result<(), Error> {
        self.sender
            .send(Message::Response(response))
            .context("failed to send data back to the client")
    }

    pub fn send_request(&self, request: Request) -> Result<(), Error> {
        self.sender
            .send(Message::Request(request))
            .context("failed to send a request to client")
    }

    pub fn file_position(
        &self,
        param: TextDocumentPositionParams,
    ) -> Result<SourceFileAndBytePos, Error> {
    }
}

impl Deref for GlobalState {
    type Target = Shared;

    fn deref(&self) -> &Self::Target {
        &self.shared
    }
}

impl Deref for GlobalStateSnapshot {
    type Target = Shared;

    fn deref(&self) -> &Self::Target {
        &self.shared
    }
}
