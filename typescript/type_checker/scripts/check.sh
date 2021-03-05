#!/usr/bin/env bash
#
# This scripts invokes all unignored tests, update pass list (append-only)
# and print the list of failing tests.
# 
#

set -eu

TEST='' cargo test --test conformance \
  | grep 'ts .\.\. ok$' \
  | sed -e 's!test tsc::conformance::!!' \
  | sed -e 's! ... ok!!' \
  | sed -e 's!::!/!g' \
  | sed -e 's!test !!' \
  >> tests/conformance.pass.txt

./scripts/sort.sh

TEST='' cargo test --test conformance \
  | grep 'ts .\.\. FAILED$' \
  | sed -e 's!test tsc::conformance::!!'