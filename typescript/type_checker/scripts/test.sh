#!/usr/bin/env bash
set -eux

err_handler () {
   if command -v osascript &> /dev/null
  then
      osascript -e 'display notification "Test failed!"'
  fi
   exit
}

trap err_handler ERR

./scripts/sort.sh

export RUST_BACKTRACE=1
export RUST_LOG=debug

# We prevent regression using faster checks
GOLDEN_ONLY=1 cargo test -q -p stc_ts_file_analyzer --test visualize

WIP_STATS=1 cargo test --color always -q --test tsc

if command -v osascript &> /dev/null
then
    osascript -e 'display notification "Test finished!"'
fi