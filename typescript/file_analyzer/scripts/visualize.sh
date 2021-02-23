#!/usr/bin/env bash
set -eux

export RUST_BACKTRACE=1
git restore -- tests/visualize
UPDATE=1 cargo test --test visualize $@