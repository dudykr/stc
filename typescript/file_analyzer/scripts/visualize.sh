#!/usr/bin/env bash
set -eux

export RUST_BACKTRACE=1
git restore -- tests/visualize
touch ./tests/visualize.rs
UPDATE=1 cargo test --test visualize $@