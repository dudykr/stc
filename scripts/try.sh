#!/usr/bin/env bash

export RUST_BACKTRACE=1
export RUST_LOG=trace

export STC_DIR=$(pwd)
cargo build --release
$STC_DIR/target/release/stc --help
(cd $1 && $STC_DIR/target/release/stc test $2)