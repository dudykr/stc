#!/usr/bin/env bash
#
# This scripts invokes all unignored tests, update pass list (append-only)
# and print the list of failing tests.
# 
#

set -eu

err_handler () {
   ./scripts/_/notify.sh 'Check failed!'
   exit 1
}

trap err_handler ERR

export CARGO_TERM_COLOR=always
export RUST_BACKTRACE=1
export RUST_MIN_STACK=$((8 * 1024 * 1024))

RUST_LOG=error TEST='' DO_NOT_PRINT_MATCHED=1 cargo test --test tsc  --features tracing/max_level_error --features no-threading | grep '\[REMOVE_ONLY\]' > remove-only.txt

./scripts/sort.sh

./scripts/_/notify.sh 'Check done!'
