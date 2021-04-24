#!/usr/bin/env bash
set -eux

export RUST_BACKTRACE=1

# All reference files should be manually updated.
git restore -- tests/pass
git restore -- tests/visualize

export RUST_LOG=debug

touch ./tests/visualize.rs
UPDATE=1 cargo test --test visualize $@