#!/usr/bin/env bash
set -eu

export RUST_LOG=off
export CARGO_MANIFEST_DIR="$(pwd)"
export UPDATE=1

cargo profile instruments -t time --test base --release --features tracing/release_max_level_off