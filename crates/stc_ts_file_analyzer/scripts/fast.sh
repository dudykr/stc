#!/usr/bin/env bash
#
# This scripts invokes all base tests but without logging, to make test faster.
#

set -eu

export CARGO_TERM_COLOR=always
export RUST_BACKTRACE=1
export RUST_MIN_STACK=$((8 * 1024 * 1024))

# We prevent regression using faster checks
RUST_LOG=off ./scripts/base.sh --features tracing/max_level_error $@
