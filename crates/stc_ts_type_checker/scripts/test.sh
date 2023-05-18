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
export RUST_MIN_STACK=$((8 * 1024 * 1024))

# We prevent regression using faster checks
RUST_LOG=error ./scripts/base.sh  --features tracing/max_level_off

TEST="$@" cargo test --color always -q --test tsc  --features no-threading

./scripts/_/notify.sh 'Test finished!'