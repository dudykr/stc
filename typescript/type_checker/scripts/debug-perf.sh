#!/usr/bin/env bash
set -eux

export RUST_LOG=debug,swc_common=off

TEST="$1" cargo test --test tsc --release -- --nocapture