#!/usr/bin/env bash
set -eu


export RUST_BACKTRACE=1
export RUST_LOG=debug,swc_common=off

UPDATE=1 cargo test -p stc_ts_file_analyzer --test base