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
export RUST_MIN_STACK=$((16 * 1024 * 1024))

# We prevent regression using faster checks
RUST_LOG=off ./scripts/base.sh --features tracing/max_level_error
