#!/usr/bin/env bash

set -eu

cargo test --test conformance \
  | grep '.\.\. ok$' \
  | sed -e 's!test tsc::conformance::!!' \
  | sed -e 's! ... ok!!' \
  | sed -e 's!::!/!' \
  | sed -e 's!test !!' \
  > tests/success.txt