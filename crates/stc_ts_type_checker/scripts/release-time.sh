#!/usr/bin/env bash

set -eux

export RUST_LOG=off

TEST='' cargo test --test tsc --release --features tracing/release_max_level_off > /dev/null