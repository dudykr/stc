#!/usr/bin/env bash
#
# This scripts invokes all unignored tests, update pass list (append-only)
# and print the list of failing tests.
# 
#

set -eu

err_handler () {
   ./scripts/_/notify.sh 'Check failed!'
   exit 1
}

trap err_handler ERR

export CARGO_TERM_COLOR=always
export RUST_BACKTRACE=1
export RUST_MIN_STACK=$((16 * 1024 * 1024))

# We prevent regression using faster checks
./scripts/bash.sh

RUST_LOG=off TEST='' DONT_PRINT_MATCHED=1 cargo test --test tsc \
  | tee /dev/stderr \
  | grep 'ts .\.\. ok$' \
  | sed -e 's!test conformance::!!' \
  | sed -e 's! ... ok!!' \
  | sed -e 's!::!/!g' \
  | sed -e 's!test !!' \
  >> tests/conformance.pass.txt

./scripts/sort.sh

./scripts/_/notify.sh 'Check done!'
