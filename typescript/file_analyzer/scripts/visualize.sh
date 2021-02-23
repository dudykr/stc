#!/usr/bin/env bash
set -eux

export RUST_BACKTRACE=1
cargo test --test visualize $@