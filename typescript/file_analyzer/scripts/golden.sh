#!/usr/bin/env bash
set -eu

export RUST_BACKTRACE=0
export RUST_LOG=0

echo 'Updating golden.txt'
cargo test --test visualize -- --nocapture 2>&1 \
    | grep '\[SUCCESS\]' \
    | sed -e 's!\[SUCCESS\]!!' \
    | xargs -L 1 realpath --relative-to="." \
    >> tests/golden.txt

./scripts/sort.sh