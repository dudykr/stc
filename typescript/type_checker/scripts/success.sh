#!/usr/bin/env bash

set -eu

cargo test --test conformance \
  | grep '.\.\. ok$' \
  | sed -e 's!test tsc::conformance::!!' \
  | sed -e 's! ... ok!!' \
  > tests/success.txt