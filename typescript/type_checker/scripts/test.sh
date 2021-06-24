#!/usr/bin/env bash
set -eux

err_handler () {
    ./scripts/_/notify.sh 'Test failed!'
    exit 1
}

trap err_handler ERR

./scripts/sort.sh

export RUST_BACKTRACE=1
export RUST_LOG=debug,swc_common=off

# We prevent regression using faster checks
GOLDEN_ONLY=1 cargo test -q -p stc_ts_file_analyzer --test visualize

WIP_STATS=1 cargo test --color always -q --test tsc

./scripts/_/notify.sh 'Test finished!'