#!/usr/bin/env bash

export RUST_BACKTRACE=1
export RUST_LOG=trace
 
 
cargo run --features no-threading -- check $1 --types node