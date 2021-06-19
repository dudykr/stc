#!/usr/bin/env bash
set -eux

err_handler () {
    ./scrtips/_/notify.sh 'Test failed!'
    exit
}

trap err_handler ERR

./scripts/sort.sh

export RUST_BACKTRACE=1
export RUST_LOG=debug

# We prevent regression using faster checks
GOLDEN_ONLY=1 cargo test -q -p stc_ts_file_analyzer --test visualize

WIP_STATS=1 cargo test --color always -q --test tsc

./scrtips/_/notify.sh 'Test finished!'