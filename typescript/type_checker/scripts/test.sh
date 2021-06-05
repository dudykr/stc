#!/usr/bin/env bash
set -eux

./scripts/sort.sh

export RUST_BACKTRACE=1
export RUST_LOG=debug

# We prevent regression using faster checks
GOLDEN_ONLY=1 cargo test -p stc_ts_file_analyzer --test visualize

cargo test --color always -q --test tsc

if command -v osascript &> /dev/null
then
    osascript -e 'display notification "Test finished!"'
fi