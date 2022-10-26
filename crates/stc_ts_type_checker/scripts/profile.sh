#!/usr/bin/env bash

set -eux

export RUST_LOG=off
export CARGO_MANIFEST_DIR="$(pwd)"

cargo profile instruments -t time --test tsc --release --features tracing/release_max_level_off