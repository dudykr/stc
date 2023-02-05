#!/usr/bin/env bash
# This script un-ignores passing tests
set -eu
set -o pipefail

export CARGO_TERM_COLOR=always
export RUST_BACKTRACE=1
export RUST_MIN_STACK=$((16 * 1024 * 1024))

function deleteCache {
    find tests/tsc -name '\.*.tsc-errors.json' - -delete
}

deleteCache

# We prevent regression using faster checks
touch ../stc_ts_file_analyzer/tests/base.rs
cargo test -p stc_ts_file_analyzer --features tracing/max_level_off --color always --test base -- -Zunstable-options --report-time --ignored || true


deleteCache
