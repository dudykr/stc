#!/usr/bin/env bash
set -eux

export RUST_LOG=stc_checker=debug
cargo test --test fixture -- -Z unstable-options --report-time=colored

# Clear screen
printf "\33c\e[3J"

# export RUST_LOG=0
export RUST_LOG=0
export STC_EARLY_ERROR=1

cargo test --test libraries -- --nocapture

cargo test --test libraries -- --nocapture --ignored