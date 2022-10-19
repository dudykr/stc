#!/usr/bin/env bash
set -eu

err_handler () {
    ./scripts/_/notify.sh 'Test failed!'
    exit 1
}

trap err_handler ERR

./scripts/sort.sh

export RUST_BACKTRACE=1
export RUST_LOG=debug,swc_common=off
export RUST_MIN_STACK=$((16 * 1024 * 1024))

# We prevent regression using faster checks
cargo test -p stc_ts_file_analyzer --test base

TEST="$@" WIP_STATS=1 cargo test --color always -q --test tsc

./scripts/_/notify.sh 'Test finished!'