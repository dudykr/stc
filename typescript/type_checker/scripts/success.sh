#!/usr/bin/env bash

set -eu

TEST='' cargo test --test conformance \
  | grep 'ts .\.\. ok$' \
  | sed -e 's!test tsc::conformance::!!' \
  | sed -e 's! ... ok!!' \
  | sed -e 's!::!/!g' \
  | sed -e 's!test !!' \
  >> tests/conformance.pass.txt

./scripts/sort.sh