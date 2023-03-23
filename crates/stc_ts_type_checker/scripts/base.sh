#!/usr/bin/env bash
#
# Usage: `./scripts/base.sh test_name_to_run``
#
set -eu
set -o pipefail

export CARGO_TERM_COLOR=always
export RUST_BACKTRACE=1
export RUST_MIN_STACK=$((8 * 1024 * 1024))

# We prevent regression using faster checks
touch ../stc_ts_file_analyzer/tests/base.rs
UPDATE=1 cargo test -p stc_ts_file_analyzer $@ --color always --lib --test base --features no-threading -- -Zunstable-options --report-time
