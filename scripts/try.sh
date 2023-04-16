#!/usr/bin/env bash

export RUST_BACKTRACE=1
export RUST_LOG=trace

export STC_DIR=$(pwd)
cargo build
$STC_DIR/target/debug/stc --help
(cd $1 && $STC_DIR/target/debug/stc test $2)