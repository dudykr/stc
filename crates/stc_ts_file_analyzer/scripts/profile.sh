#!/usr/bin/env bash
set -eu

export RUST_LOG=off
export CARGO_MANIFEST_DIR="$(pwd)"

cargo profile instruments -t time --lib --release --features tracing/release_max_level_off > /dev/null