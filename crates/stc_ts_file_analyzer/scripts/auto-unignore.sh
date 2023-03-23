#!/usr/bin/env bash
# This script un-ignores passing tests
set -eu
set -o pipefail

export CARGO_TERM_COLOR=always
export RUST_BACKTRACE=1
export RUST_MIN_STACK=$((8 * 1024 * 1024))

function cleanup {
    echo "Deleting cache for ignored tests"
    find ./tests/tsc -name '\.*.tsc-errors.json' -type f -delete
}

cleanup

# We prevent regression using faster checks
touch ../stc_ts_file_analyzer/tests/base.rs
cargo test -p stc_ts_file_analyzer --features tracing/max_level_off --features no-threading --color always --test base $@ -- -Zunstable-options --report-time --ignored || true


cleanup
