use lsp_server::Response;
use std::time::Instant;

pub(crate) type ReqQueue = lsp_server::ReqQueue<(String, Instant), ReqHandler>;

pub(crate) type ReqHandler = fn(&mut State, Response);

pub(crate) struct State {}

pub(crate) struct Snapshot {}
